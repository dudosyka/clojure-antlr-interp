(ns visitor-project
  (:require [math]
            [op]
            [logic])
  (:import (com.grammar GrammarParser GrammarVisitor)
           (org.antlr.runtime.tree ParseTree)))

(def zero-reg "x0")
(def nested-res-reg "x22")
(def acc-reg "x20")
(def last-addr 65535)
(def max-stack 1000)
(def ascii 128)
(def ascii-num-char-start 48)
(def min-op-reg 5)

(defrecord VarStackEntry [value addr])
(defrecord Var [name stack])
(defrecord ListItem [type value])
(defrecord Context [value next-reg next-cell vars list-item op stack])

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

(defn op-mapper
  ([op op1 op2]
   (op-mapper op op1 op1 op2))
  ([op rd op1 op2]
   (case op
     :add (str "add " rd ", " op1 ", " op2)
     :mul (str "mul " rd ", " op1 ", " op2)
     :div (str "div " rd ", " op1 ", " op2)
     :sub (str "sub " rd ", " op1 ", " op2)
     :eq (str "seq " rd ", " op1 ", " op2))))

(defn is-item-expr [expr]
  (try
    (.item expr)
    (catch Exception _ nil)))

(defn is-list-expr [expr]
  (try
    (.list expr)
    (catch Exception _ nil)))

(defn append-asm [ctx cmd]
  (let [op (:op ctx)]
    (assoc ctx :op (conj op cmd))))

(defn comparison-op-mapper [ctx op rd r1 r2]
  (case op
    :less (append-asm ctx (str "slt " rd ", " r1 ", " r2))
    :eq (append-asm ctx (str "seq " rd ", " r1 ", " r2))
    :gr-eq (append-asm ctx (str "sge " rd ", " r1 ", " r2))
    :neq (append-asm ctx (str "sne " rd ", " r1 ", " r2))
    :less-eq (-> ctx
                 (comparison-op-mapper :less "x1" r1 r2)
                 (comparison-op-mapper :eq "x2" r1 r2)
                 (append-asm (op-mapper :add rd r1 r2))
                 (append-asm (str "li x1, 2"))
                 (comparison-op-mapper :eq rd rd "x1"))
    :gr (-> ctx
            (comparison-op-mapper :gr-eq "x1" r1 r2)
            (comparison-op-mapper :neq "x2" r1 r2)
            (append-asm (op-mapper :add rd r1 r2))
            (append-asm (str "li x1, 2"))
            (comparison-op-mapper :eq rd rd "x1"))))

(defn next-reg [visitor]
  (let [ctx (-> visitor .-context)
        next-reg (:next-reg ctx)]
    [(str "x" next-reg) (assoc ctx :next-reg (inc next-reg))]))

(defn set-reg-by-acc [ctx reg]
  (append-asm ctx (op-mapper :add reg zero-reg acc-reg)))

(defn copy-reg-to [ctx from to]
  (append-asm ctx (op-mapper :add to zero-reg from)))

(defn load-var [ctx var reg]
  (let [var (-> ctx :vars (get var))
        {:keys [value addr]} (-> var :stack first)]
    (if (nil? addr)
      (append-asm ctx (str "li " reg ", " value))
      (append-asm ctx (str "lw " reg ", x0, " addr)))))

;(+ 23 (* a b) (/ (/ b 2) 4 5 (/ 4 2)) (- 4 2 (+ b 3)) 3)

(defn start-nested [visitor lead simplified op ns vars]
  (let [ctx (-> visitor .-context)
        {:keys [value type]} lead]
    (case type
      :int (if (and (empty? ns) (empty? vars))
             (append-asm ctx (str "li " acc-reg ", " value))
             (-> ctx
                 (append-asm (str "li " acc-reg ", " value))
                 (append-asm (str "li x21, " simplified))
                 (append-asm (op-mapper op acc-reg "x21"))))
      :var (-> ctx
               (load-var value acc-reg)
               (append-asm (str "li x21, " simplified))
               (append-asm (op-mapper op acc-reg "x21")))
      :expr (-> ctx
                (append-asm (op-mapper :add acc-reg zero-reg nested-res-reg))
                (append-asm (str "li x21, " simplified))
                (append-asm (op-mapper op acc-reg "x21"))))))

(defn set-nested-res [ctx val]
  (-> ctx
      (append-asm (op-mapper :add nested-res-reg zero-reg val))))

(defn set-nested-res-scalar [ctx val]
  (-> ctx
      (append-asm (str "li " nested-res-reg ", " val))))

(defn append-vars-op
  ([ctx op vars]
   (if (empty? vars)
     ctx
     (append-vars-op ctx op vars (first vars))))
  ([ctx op vars var]
   (if (empty? vars)
     ctx
     (let [var (-> ctx :vars (get var))
           {:keys [value addr]} (-> var :stack first)
           vars (drop 1 vars)
           var (first vars)]
       (-> (if (nil? addr)
             (-> ctx
                 (append-asm (str "li x21, " value))
                 (append-asm (op-mapper op "x20" "x21")))
             (-> ctx
                 (append-asm (str "lw x21, x0, " addr))
                 (append-asm (op-mapper op "x20" "x21"))))
           (recur op vars var))))))

(defn get-value [visitor]
  (-> visitor .-context :value))

(defn set-list-item [visitor value type]
  (let [ctx (-> visitor .-context)]
    (assoc ctx :list-item (->ListItem type value))))

(defn get-list-item [visitor]
  (-> visitor .-context :list-item))

(defn get-var [visitor var]
  (let [var (-> visitor .-context :vars (get var))]
    (if (nil? var)
      nil
      (-> var :stack first))))

(defn get-var-addr [visitor var]
  (let [var (-> visitor (get-var var))]
    (if (nil? var) nil (:addr var))))

(defn get-var-value [visitor var]
  (let [var (-> visitor (get-var var))]
    (if (nil? var) nil (:value var))))

(defn allocate [ctx]
  (let [next-addr (:next-cell ctx)]
    [(assoc ctx :next-cell (dec next-addr)) next-addr]))

(defn create-var [visitor var-name value]
  (let [ctx (-> visitor .-context)
        vars (-> ctx :vars)
        exist? (-> vars (get var-name) nil?)
        [ctx addr val] (if (nil? value)
                         (-> ctx allocate (conj value))
                         [ctx nil value])]
    (if (not exist?)
      (-> ctx (assoc :vars
                     (assoc vars
                            var-name
                            (->Var var-name '((->VarStackEntry val addr)))))))))

(defn get-vars-map [ctx]
  (->> ctx :vars
       (filter (fn [[_ {:keys [stack]}]]
                 (let [{:keys [value addr]} (first stack)]
                   (and (nil? addr) (-> value nil? not)))))
       (map (fn [[name {:keys [stack]}]]
              {name (-> stack first :value)}))
       (reduce into {})))

(defn release-reg [visitor count]
  (let [ctx (-> visitor .-context)
        next-reg (:next-reg ctx)]
    (assoc ctx :next-reg (- next-reg count))))

(defn jump-if-zero [ctx reg target]
  (append-asm ctx (str "beq " reg ", x0, " target)))

(defn jump-if-not-zero [ctx reg target]
  (append-asm ctx (str "bne " reg ", x0, " target)))

(defn set-label [ctx label]
  (append-asm ctx (str label ":")))

(deftype VisitorImpl [context]
  GrammarVisitor
  (visit [_ ctx]
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
          ctx _
          items (reduce (fn [values expr]
                          (if (-> (is-item-expr expr) nil? not)
                            (let [visited (.visit ctx expr)
                                  list-item (-> visited get-list-item)]
                              (conj values list-item))
                            (conj values (->ListItem :expr expr)))) [] (.expr node))
          vars (get-vars-map (-> ctx .-context))
          process-num-list (fn [items by simplifier]
                             (let [{:keys [lead simplified ns vars]} (simplifier items vars)
                                   ctx _
                                   visitor (case (:type lead)
                                             :int (-> ctx
                                                      (start-nested lead simplified by ns vars)
                                                      (append-vars-op by vars)
                                                      VisitorImpl.)
                                             :var (-> ctx
                                                      (start-nested lead simplified by ns vars)
                                                      (append-vars-op by vars)
                                                      VisitorImpl.)
                                             :expr (-> ctx
                                                       (.visit (:value lead)) .-context
                                                       VisitorImpl.
                                                       (start-nested lead simplified by ns vars)
                                                       (append-vars-op by vars)
                                                       VisitorImpl.))]
                               (if (empty? ns)
                                 (let []
                                   (-> visitor .-context
                                       (set-nested-res acc-reg)
                                       VisitorImpl.))
                                 (let [[reg ctx] (next-reg visitor)
                                       visitor (-> ctx (set-reg-by-acc reg) VisitorImpl.)
                                       visitor (reduce (fn [ctx item]
                                                         (-> ctx
                                                             (.visit item) .-context
                                                             (append-asm (op-mapper by "x20" reg "x22"))
                                                             (set-reg-by-acc reg)
                                                             (set-nested-res acc-reg)
                                                             VisitorImpl.)) visitor ns)]
                                   visitor))))

          visit-comparison-operand (fn [ctx item]
                                     (let [[reg ctx] (next-reg ctx)]
                                       (case (:type item)
                                         :int [reg (-> ctx
                                                       (append-asm (str "li " reg ", " (:value item)))
                                                       VisitorImpl.)]
                                         :var [reg (-> ctx
                                                       (load-var (:value item) reg)
                                                       VisitorImpl.)]
                                         :expr (let [ctx (-> ctx (.visit (:value item)) .-context)
                                                     [reg ctx] (-> ctx VisitorImpl. next-reg)
                                                     ctx (-> ctx
                                                             (copy-reg-to nested-res-reg reg)
                                                             VisitorImpl.)]
                                                 [reg ctx]))))

          process-comparison-list (fn [items by]
                                    (let [f (logic/comparison-fun-mapper by)
                                          {:keys [current ns]} (logic/simplify-comparison items f vars)]
                                      (if (-> current nil? not)
                                        (-> (.-context ctx)
                                            (set-nested-res-scalar (if current 1 0))
                                            VisitorImpl.)
                                        (let [end-label (->> (str by "_" (System/currentTimeMillis) "_end")
                                                             (drop 1)
                                                             (apply str))]
                                          (-> (loop [ns ns
                                                     item (first ns)
                                                     ctx ctx]
                                                (if (empty? ns)
                                                  ctx
                                                  (let [ns (drop 1 ns)
                                                        [reg-op1 ctx] (visit-comparison-operand ctx (first item))
                                                        [reg-op2 ctx] (visit-comparison-operand ctx (second item))
                                                        ctx (-> ctx
                                                                (release-reg 2)
                                                                (comparison-op-mapper by nested-res-reg reg-op1 reg-op2)
                                                                (jump-if-zero nested-res-reg end-label)
                                                                VisitorImpl.)]
                                                    (recur ns (first ns) ctx))))
                                              .-context
                                              (set-label end-label)
                                              VisitorImpl.)))))

          visit-logic-item (fn [ctx item]
                             (case (:type item)
                               :var (-> ctx .-context
                                        (load-var (:value item) nested-res-reg)
                                        VisitorImpl.)
                               :expr (.visit ctx (:value item))))

          check-logic-item (fn [ctx by end-label]
                             (-> (case by
                                   :and (-> ctx .-context (jump-if-zero nested-res-reg end-label))
                                   :or (-> ctx .-context (jump-if-not-zero nested-res-reg end-label)))
                                 VisitorImpl.))

          process-logic-list (fn [items by]
                               (let [f (logic/logic-fun-mapper by)
                                     {:keys [current ns]} (logic/simplify-logic items f vars)]
                                 (if (empty? ns)
                                   (-> ctx .context
                                       (set-nested-res-scalar current)
                                       VisitorImpl.)
                                   (let [end-label (->> (str by "_" (System/currentTimeMillis) "_end")
                                                        (drop 1)
                                                        (apply str))]
                                     (-> (loop [ns ns
                                                item (first ns)
                                                ctx ctx]
                                           (if (empty? ns)
                                             ctx
                                             (recur (drop 1 ns)
                                                    (first ns)
                                                    (-> ctx
                                                        (visit-logic-item item)
                                                        (check-logic-item by end-label)))))

                                         .-context
                                         (set-label end-label)
                                         VisitorImpl.)))))]

      (case symbol
        :add (process-num-list items :add math/simplify-sum)
        :sub (process-num-list items :sub math/simplify-sub)
        :mul (process-num-list items :mul math/simplify-multiplication)
        :div (process-num-list items :div math/simplify-division)
        (:eq :gr :gr-eq :less :less-eq) (process-comparison-list items symbol)
        (:and :or) (process-logic-list items symbol))))


      ;(VisitorImpl.
      ;  (update-value ctx (case symbol
      ;                     :add (apply + items)
      ;                     :mul (apply * items)
      ;                     :div (apply / items)
      ;                     :sub (apply - items)
      ;                     :eq (apply = items)
      ;                     :gr (apply > items)
      ;                     :gr-eq (apply >= items)
      ;                     :less (apply < items)
      ;                     :less-eq (apply <= items)
      ;                     :and (apply and-fun items)
      ;                     :or (apply or-fun items)
      ;                     :not (apply not items)
      ;                     :print (apply println items)
      ;                     :add-str (apply str items))))))

  (visitBinding [_ node])
    ;(let [id (-> node .ID .getText)
    ;      val (-> _ (.visit (.getChild node 1)) get-value)
    ;      var (->Var id scope val n)]
    ;  (-> _
    ;      (set-var var)
    ;      VisitorImpl.)))


  (visitBindings [_ node])
    ;(let [[_ ctx] (reduce (fn [[i visitor] binding]
    ;                        [(inc i) (.visit (-> visitor (update-value i) VisitorImpl.) binding)])
    ;                      [0 _]
    ;                      (-> node .-children))]
    ;  ctx))

  (visitLet [_ node])

  (visitLoop [_ node])

  (visitDefn [_ node])

  (visitBlock [_ node])

  (visitIf [_ node])

  (visitExpr [_ node]
    (.visit _ (.getChild node 0)))

  (visitProg [_ ctx]
    (.visitChildren _ ctx))

  (visitTerminal [ctx _] ctx)

  (visitErrorNode [ctx _] ctx)

  (visitId [_ node]
    (let [var-name (->> node
                        .ID
                        .getText)]
      (VisitorImpl. (set-list-item _ var-name :var))))

  (visitInt [_ node]
    (let [int-val (->> node
                       .getText
                       parse-long)]
      (VisitorImpl. (set-list-item _ int-val :int))))

  (visitStr [_ node]
    (let [str (->> node .getText)
          str (-> str (subs 1 (-> str count dec)))]
      (VisitorImpl. (set-list-item _ str :str))))

  (visitBool [_ node]))

(defn visit [^ParseTree tree]
  (let [a (->VarStackEntry 1 nil)
        b (->VarStackEntry nil 4234)
        v (VisitorImpl. (->Context nil
                                   5
                                   (- last-addr max-stack ascii)
                                   {"a" (->Var "a" (conj '() a))
                                    "b" (->Var "b" (conj '() b))}
                                   nil
                                   []
                                   '()))]
    (.visit v tree)))