(ns visitor
  (:import (com.grammar GrammarBaseVisitor GrammarParser GrammarVisitor)
           (org.antlr.runtime.tree ParseTree)))


(defrecord Var [name scope value n])
(defrecord Fun [name bindings block])
(defrecord Context [value vars scope recur-queue user-defs])

(defn get-ctx [visitor]
  (-> visitor .-context))

(def symbol-mapper {GrammarParser/ADD    :add
                    GrammarParser/MUL    :mul
                    GrammarParser/DIV    :div
                    GrammarParser/SUB    :sub
                    GrammarParser/EQ     :eq
                    GrammarParser/GR     :gr
                    GrammarParser/GREQ   :gr-eq
                    GrammarParser/LESS   :less
                    GrammarParser/LESSEQ :less-eq
                    GrammarParser/AND    :and
                    GrammarParser/OR     :or
                    GrammarParser/NOT    :not
                    GrammarParser/PRINT  :print
                    GrammarParser/RECUR  :recur
                    GrammarParser/ADDSTR :add-str
                    GrammarParser/ID     :id})

(defn get-symbol [ctx]
  (->> ctx
       .symbol
       .getType
       (get symbol-mapper)))

(defn update-value [visitor value]
  (let [ctx (-> visitor .-context)]
    (assoc ctx :value value)))

(defn set-var [visitor ^Var var]
  (let [ctx (-> visitor .-context)
        scope (:scope var)
        name (:name var)
        existed (-> ctx :vars (get name))
        vars (-> ctx
                 :vars
                 (assoc name var))
        vars (-> vars
                 (assoc scope (-> (get vars scope)
                                  (assoc name existed))))]
    (assoc ctx :vars vars)))

(defn get-var [visitor name]
  (let [var (-> visitor get-ctx :vars (get name))]
    (if (nil? var)
      var
      (:value var))))

(defn set-loop-block [visitor scope block]
  (let [ctx (-> visitor .-context)
        vars (-> ctx
                 :vars)
        vars (-> vars
                 (assoc scope (-> vars (get scope)
                                  (assoc 0 block))))]
    (-> ctx
        (assoc :vars vars)
        (assoc :recur-queue (-> ctx :recur-queue (conj scope))))))

(defn get-loop-block [visitor scope]
  (-> visitor .-context :vars (get scope) (get 0)))

(defn get-recur-scope [visitor]
  (-> visitor get-ctx :recur-queue last))

(defn drop-recur-scope [visitor]
  (let [ctx (-> visitor get-ctx)]
    (assoc ctx :recur-queue (-> ctx :recur-queue drop-last))))

(defn get-loop-var-name [visitor scope n]
  (let [vars (-> visitor get-ctx :vars (get scope))
        [name _] (first (filter (fn [[k _]]
                                  (= (-> visitor get-ctx :vars (get k) :n) n)) vars))]
    name))

(defn update-var [visitor name val]
  (let [ctx (-> visitor get-ctx)
        var (-> ctx :vars (get name))
        vars (-> ctx :vars
                 (assoc name (assoc var :value val)))]
    (assoc ctx :vars vars)))

(defn get-cur-scope [visitor]
  (-> visitor .-context :scope))

(defn get-value [visitor]
  (-> visitor .-context :value))

(defn create-scope [visitor name]
  (loop [i 0]
    (let [free (-> visitor get-ctx :vars (get (str i name)))]
      (if (nil? free)
        (str i name)
        (recur (inc i))))))

(defn init-scope [visitor scope]
  (let [ctx (get-ctx visitor)
        vars (-> ctx :vars
                 (assoc scope {}))
        ctx (-> ctx
                (assoc :vars vars)
                (assoc :scope scope))]
    ctx))

(defn drop-scope [visitor scope old]
  (let [ctx (get-ctx visitor)
        scoped-vars (-> ctx :vars (get scope))
        vars (reduce (fn [vars [k v]]
                       (if (nil? v)
                         (dissoc vars k)
                         (assoc vars k v))) (:vars ctx) scoped-vars)
        vars (dissoc vars scope)
        ctx (-> ctx
                (assoc :vars vars)
                (assoc :scope old))]
    ctx))

(defn def-fun [visitor name bindings block]
  (let [ctx (get-ctx visitor)]
    (-> ctx
        (assoc :user-defs
               (-> ctx :user-defs
                   (assoc name (->Fun name bindings block)))))))

(defn invoke [visitor name items]
  (let [scope (create-scope visitor name)
        ctx (get-ctx visitor)
        vars (:vars ctx)
        fun (-> ctx :user-defs (get name))
        new-vars (->> fun :bindings
                     (reduce (fn [[i vars] [k v]] ; k, v -> binding pair
                               [(inc i) (assoc vars k (->Var k scope (nth items i) v))]) [0 {}])
                     second)]
    [vars (-> ctx
              (assoc :vars new-vars)
              (assoc :scope scope)) (:block fun)]))

(defn end-function-call [visitor vars scope]
  (-> visitor
      get-ctx
      (assoc :vars vars)
      (assoc :scope scope)))


(deftype VisitorImpl [context]
  GrammarVisitor
  (visit [_ ctx]
    ;(println "visit" ctx (get-ctx _))
    (.accept ctx _))
  (visitChildren [_ node]
    (let [child-count (.getChildCount node)]
      (if (> child-count 0)
        (loop [idx 0
               val _
               cur-node (.getChild node idx)]
          (if (not (nil? cur-node))
            (recur (inc idx) (.accept cur-node val) (.getChild node (inc idx)))
            val))
        _)))
  (visitList [_ node]
    (let [symbol (get-symbol node)
          [ctx items] (reduce (fn [[ctx values] expr]
                                (let [visited (.visit ctx expr)
                                      value (get-value visited)]
                                  [visited (conj values value)])) [_ []] (.expr node))]
      (VisitorImpl. (update-value ctx (case symbol
                                       :add (apply + items)
                                       :mul (apply * items)
                                       :div (apply / items)
                                       :sub (apply - items)
                                       :eq (apply = items)
                                       :gr (apply > items)
                                       :gr-eq (apply >= items)
                                       :less (apply < items)
                                       :less-eq (apply <= items)
                                       ;:and (apply and items)
                                       ;:or (apply or items)
                                       :not (apply not items)
                                       :print (apply println items)
                                       :add-str (apply str items)
                                       :recur (let [scope (get-recur-scope _)
                                                    block (get-loop-block _ scope)
                                                    [_ ctx] (reduce (fn [[i ctx] item]
                                                                      (let [var-name (get-loop-var-name ctx scope i)]
                                                                        [(inc i) (-> ctx
                                                                                     (update-var var-name item)
                                                                                     VisitorImpl.)])) [0 ctx] items)]
                                                (-> ctx
                                                    (.visit block)
                                                    (get-value)))

                                       :id (let [scope (get-cur-scope _)
                                                 name (-> node .-symbol .getText)
                                                 [old-vars invoke-ctx block] (invoke ctx name items)]
                                             ;(println "INVOKE FUNCTION" name)
                                             ;(clojure.pprint/pprint old-vars)
                                             ;(clojure.pprint/pprint invoke-ctx)
                                             (-> invoke-ctx VisitorImpl.
                                                 (.visit block)
                                                 (end-function-call old-vars scope)
                                                 VisitorImpl.
                                                 get-value)))))))





  (visitBinding [_ node]
    (let [scope (get-cur-scope _)
          n (get-value _)
          id (-> node .ID .getText)
          val (-> _ (.visit (.getChild node 1)) get-value)
          var (->Var id scope val n)]
      (-> _
          (set-var var)
          VisitorImpl.)))

  (visitBindings [_ node]
    (let [[_ ctx] (reduce (fn [[i visitor] binding]
                            [(inc i) (.visit (-> visitor (update-value i) VisitorImpl.) binding)])
                          [0 _]
                          (-> node .-children))]
      ctx))

  (visitLet [_ node]
    (let [cur-scope (get-cur-scope _)
          scope-name (create-scope _ "let")
          ctx (-> _ (init-scope scope-name) VisitorImpl.)
          ^VisitorImpl bindings (->> node
                                     .bindings
                                     (.visit ctx))]

      (-> bindings
          (.visit (-> node .block))
          (drop-scope scope-name cur-scope)
          VisitorImpl.)))

  (visitLoop [_ node]
    (let [cur-scope (get-cur-scope _)
          scope-name (create-scope _ "loop")
          block (-> node .block)
          ctx (-> _
                  (init-scope scope-name)
                  VisitorImpl.
                  (set-loop-block scope-name block) ; Set vars: { Nloop: { 0: node } }
                  VisitorImpl.)
          ^VisitorImpl bindings (->> node
                                     .bindings
                                     (.visit ctx))]
      (-> bindings
          (.visit block)
          (drop-scope scope-name cur-scope)
          VisitorImpl.
          (drop-recur-scope)
          VisitorImpl.)))
  (visitDefn [_ node]
    (let [name (-> node .ID .getFirst .getText)
          bindings (->> node
                        .ID
                        (drop 1)
                        (reduce (fn [[i bindings] id]
                                  [(inc i) (into bindings {(.getText id) i})]) [0 {}])
                        (second))
          block (-> node .block)
          ctx (-> _
                  (def-fun name bindings block)
                  VisitorImpl.)]
      ;(println "DEF FUN")
      ;(clojure.pprint/pprint (-> ctx get-ctx))
      ctx))


  (visitBlock [_ node]
    (reduce (fn [visitor ctx] (.visit visitor ctx)) _ (.expr node)))

  (visitIf [_ node]
    (let [cond (->> node
                    .cond
                    (.visit _)
                    (get-value))]
      (if cond
        (.visit _ (->> node .-if_branch))
        (.visit _ (->> node .-else_branch)))))


  (visitId [_ node]
    (VisitorImpl. (->> node
                       .ID
                       .getText
                       (get-var _)
                       (update-value _))))

  (visitExpr [_ node]
    (.visit _ (.getChild node 0)))
  (visitProg [_ ctx]
    (.visitChildren _ ctx))
  (visitTerminal [_ node] _)
  (visitErrorNode [_ node] _)
  (visitInt [_ node]
    (VisitorImpl. (->> node
                       .getText
                       parse-long
                       (update-value _))))
  (visitStr [_ node]
    (VisitorImpl. (->> node .getText (update-value _)))))

(defn visit [^ParseTree tree]
  (let [v (VisitorImpl. (->Context nil {} nil [] {}))]
    (.visit v tree)))