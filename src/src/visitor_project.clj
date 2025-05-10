(ns visitor-project
  (:require [clojure.string :as str]
            [math]
            [op]
            [logic]
            [asm]
            [asm :refer [x0 x1 x2 x20 x21 x22 x28 x29 x31]])
  (:import (com.grammar GrammarParser GrammarParser$BindingContext GrammarVisitor)
           (org.antlr.runtime.tree ParseTree)))

(def zero-reg x0)
(def nested-res-reg x22)
(def acc-reg x20)
(def memory-size 65535)
(def str-op-buffer 1000)
(def max-stack 1000)
(def ascii 128)
(def ascii-num-char-start 48)
(def min-op-reg 5)

(def invoke-label "invoke")
(def return-label "return")



(defrecord VarStackEntry [type value])
(defrecord Var [name stack])
(defrecord ListItem [type value])
(defrecord Scope [name vars label type])
(defrecord Function [name label op args])
(defrecord FunctionArg [name addr])
(defrecord Context [value next-reg next-cell vars
                    list-item op stack scope
                    functions load-item])

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
     :add (asm/add rd op1 op2)
     :mul (asm/mul rd op1 op2)
     :div (asm/div rd op1 op2)
     :sub (asm/sub rd op1 op2)
     :eq (asm/seq rd op1 op2))))

(defn is-item-expr [expr]
  (-> (try
        (.item expr)
        (catch Exception _ nil)) nil? not))

(defn append-asm [ctx cmd]
  (let [op (:op ctx)]
    (if (string? cmd)
      (assoc ctx :op (conj op cmd))
      (assoc ctx :op (into op cmd)))))

(defn comparison-op-mapper [ctx op rd r1 r2]
  (case op
    :less (append-asm ctx (asm/slt rd r1 r2))
    :eq (append-asm ctx (asm/seq rd r1 r2))
    :gr-eq (append-asm ctx (asm/sge rd r1 r2))
    :neq (append-asm ctx (asm/sne rd r1 r2))
    :less-eq (-> ctx
                 (comparison-op-mapper :less x1 r1 r2)
                 (comparison-op-mapper :eq x2 r1 r2)
                 (append-asm (op-mapper :add rd x1 x2))
                 (append-asm (asm/li x1 2))
                 (comparison-op-mapper :eq rd rd x1))
    :gr (-> ctx
            (comparison-op-mapper :gr-eq x1 r1 r2)
            (comparison-op-mapper :neq x2 r1 r2)
            (append-asm (op-mapper :add rd x1 x2))
            (append-asm (asm/li x1 2))
            (comparison-op-mapper :eq rd rd x1))))

(defn next-reg-ctx [ctx]
  (let [next-reg (:next-reg ctx)]
    [(str "x" next-reg) (assoc ctx :next-reg (inc next-reg))]))

(defn next-reg [visitor]
  (-> visitor .-context next-reg-ctx))

(defn set-reg-by-acc [ctx reg]
  (append-asm ctx (op-mapper :add reg zero-reg acc-reg)))

(defn copy-reg-to [ctx from to]
  (append-asm ctx (op-mapper :add to zero-reg from)))

(defn load-var [ctx var reg]
  (let [var (-> ctx :vars (get var))
        {:keys [value type]} (-> var :stack first)]
    (case type
      :memory (append-asm ctx (asm/lw reg value))
      :value (append-asm ctx (asm/li reg value))
      :reg (append-asm ctx (asm/add reg x0 value)))))

;(+ 23 (* a b) (/ (/ b 2) 4 5 (/ 4 2)) (- 4 2 (+ b 3)) 3)

(defn start-nested [visitor lead simplified op ns vars]
  (let [ctx (-> visitor .-context)
        {:keys [value type]} lead]
    (case type
      :int (if (and (empty? ns) (empty? vars))
             (append-asm ctx (asm/li acc-reg value))
             (-> ctx
                 (append-asm (asm/li acc-reg value))
                 (append-asm (asm/li x21 simplified))
                 (append-asm (op-mapper op acc-reg x21))))
      :var (-> ctx
               (load-var value acc-reg)
               (append-asm (asm/li x21 simplified))
               (append-asm (op-mapper op acc-reg x21)))
      :expr (-> ctx
                (append-asm (op-mapper :add acc-reg zero-reg nested-res-reg))
                (append-asm (asm/li x21 simplified))
                (append-asm (op-mapper op acc-reg x21))))))

(defn set-nested-res [ctx val]
  (-> ctx
      (append-asm (op-mapper :add nested-res-reg zero-reg val))))

(defn set-nested-res-scalar [ctx val]
  (-> ctx
      (append-asm (asm/li nested-res-reg val))))

(defn append-vars-op
  ([ctx op vars]
   (if (empty? vars)
     ctx
     (append-vars-op ctx op vars (first vars))))
  ([ctx op vars var]
   (if (empty? vars)
     ctx
     (let [var (-> ctx :vars (get var))
           {:keys [value type]} (-> var :stack first)
           vars (drop 1 vars)
           var (first vars)]
       (-> (case type
             :value (-> ctx
                        (append-asm (asm/li x21 value))
                        (append-asm (op-mapper op acc-reg x21)))
             :memory (-> ctx
                         (append-asm (asm/lw x21 value))
                         (append-asm (op-mapper op acc-reg x21)))
             :reg (append-asm ctx (op-mapper op acc-reg value)))
           (recur op vars var))))))

(defn set-list-item [visitor value type]
  (let [ctx (-> visitor .-context)]
    (assoc ctx :list-item (->ListItem type value))))

(defn get-list-item [visitor]
  (-> visitor .-context :list-item))

(defn allocate [ctx]
  (let [next-addr (:next-cell ctx)]
    [(assoc ctx :next-cell (dec next-addr)) next-addr]))

(defn update-var-stack [vars var-name stack-entry]
  (-> vars
      (get var-name)
      :stack
      (assoc :stack (conj (-> vars (get var-name) :stack) stack-entry))))

(defn create-var [visitor var-name type value]
  (let [ctx (-> visitor .-context)
        vars (-> ctx :vars)
        exist? (-> vars (get var-name) nil?)
        [ctx val type] (case type
                         :memory (-> ctx allocate (conj :memory))
                         :value [ctx value :value]
                         :reg (let [[reg ctx] (-> ctx
                                                  next-reg-ctx)]
                                [ctx reg :reg]))
        vars (:vars ctx)
        stack-entry (->VarStackEntry type val)
        var (if (not exist?)
              (->Var var-name (conj '() stack-entry))
              (update-var-stack vars var-name stack-entry))]
    [[var-name stack-entry] (assoc ctx :vars (assoc vars var-name var))]))

(defn get-vars-map [ctx]
  (->> ctx :vars
       (filter (fn [[_ {:keys [stack]}]]
                 (let [{:keys [value type]} (first stack)]
                   (and (= type :value) (-> value nil? not)))))
       (map (fn [[name {:keys [stack]}]]
              {name (-> stack first :value)}))
       (reduce into {})))

(defn release-reg [visitor count]
  (let [ctx (-> visitor .-context)
        next-reg (:next-reg ctx)]
    (assoc ctx :next-reg (- next-reg count))))

(defn jump-if-zero [ctx reg target]
  (append-asm ctx (asm/beq reg zero-reg target)))

(defn jump-if-not-zero [ctx reg target]
  (append-asm ctx (asm/bne reg zero-reg target)))

(defn set-label [ctx label]
  (append-asm ctx (str label ":")))

(defn create-scope [ctx name label type]
  (let [name (->> ctx :scope count (str name))
        scope (-> ctx :scope (conj (->Scope name {} label type)))]
    (assoc ctx :scope scope)))

(defn add-scoped-var [ctx name i]
  (let [scope (-> ctx :scope first)
        scope-stack (->> ctx :scope (drop 1))
        vars (-> scope :vars
                 (assoc i name))
        scope (assoc scope :vars vars)]
    (assoc ctx :scope (conj scope-stack scope))))

(defn drop-scope [visitor]
  (let [ctx (-> visitor .-context)
        scoped-vars (map second (-> ctx :scope first :vars))]
    (println "Drop scope" ctx)
    (reduce (fn [ctx name]
              (let [vars (-> ctx :vars)
                    var (-> vars (get name))
                    type (-> var :stack first :type)
                    var (assoc var :stack (drop 1 (:stack var)))
                    vars (assoc vars name var)
                    ctx (assoc ctx :vars vars)]
                (case type
                  :reg (-> ctx
                           (assoc :next-reg (-> ctx :next-reg dec)))
                  :memory (-> ctx
                              (assoc :next-cell (-> ctx :next-cell inc)))))) ctx scoped-vars)))

(defn set-reg-limit [ctx limit]
  (assoc ctx :value limit))

(defn dec-reg-limit [ctx]
  (let [reg-limit (:value ctx)]
    (assoc ctx :value (dec reg-limit))))

(defn generate-scope-name [prefix suffix]
  (str prefix "_" (System/currentTimeMillis) "_" suffix))

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

  (visitBinding [_ node])

  (visitBindings [_ node]
    (second (reduce (fn [[i visitor] node]
                      (println i node)
                      (let [id (-> node .ID .getText)
                            node (.expr node)
                            ctx (if (is-item-expr node)
                                  (-> visitor .-context
                                      (assoc :load-item true)
                                      VisitorImpl.
                                      (.visit node)
                                      .-context
                                      (assoc :load-item false)
                                      VisitorImpl.)
                                  (.visit visitor node))
                            new-var-type (if (> (-> visitor .-context :value) 0) :reg :memory)
                            [[name {:keys [value type]}] ctx] (create-var ctx id new-var-type nil)
                            ctx (add-scoped-var ctx name i)]
                        [(inc i) (case type
                                   :memory (-> ctx
                                               (append-asm (asm/sw x1 value x22))
                                               VisitorImpl.)
                                   :reg (-> ctx
                                            (append-asm (asm/add value x0 x22))
                                            dec-reg-limit
                                            VisitorImpl.))])) [0 _] (-> node .binding))))

  (visitLet [_ node]
    (let [ctx (-> _ .-context)
          old-reg-limit (:value ctx)
          ctx (-> ctx (create-scope "let" nil :no-recursion) (set-reg-limit 3) VisitorImpl.)
          ctx (->> node
                   .bindings
                   (.visit ctx))]
      (-> ctx
          (.visit (-> node .block))
          drop-scope
          (set-reg-limit old-reg-limit)
          VisitorImpl.)))

  (visitLoop [_ node]
    (let [ctx (-> _ .-context)
          start-label (generate-scope-name "start_loop" "loop")
          ctx (-> ctx (create-scope "loop" start-label :simple) (set-reg-limit 3) VisitorImpl.)
          ctx (->> node
                   .bindings
                   (.visit ctx))]
      (-> ctx
          .-context
          (set-label start-label)
          VisitorImpl.
          (.visit (-> node .block))
          drop-scope
          VisitorImpl.)))

  (visitDefn [visitor node]
    (let [ctx (-> visitor .-context)
          name (-> node .ID .getFirst .getText)
          label (generate-scope-name name "user_defined_function")
          ctx (create-scope ctx name label :invocation)
          [_ args ctx] (->> node
                            .ID
                            (drop 1)
                            (reduce (fn [[i bindings ctx] id]
                                      (let [name (.getText id)
                                            [[_ {:keys [value]}] ctx] (create-var (VisitorImpl. ctx) name :memory nil)
                                            ctx (add-scoped-var ctx name i)]
                                        [(inc i)
                                         (into bindings {i (->FunctionArg name value)})
                                         ctx])) [0 {} ctx]))
          block (-> node .block)
          functions (:functions ctx)
          copy-ctx (-> ctx
                       (assoc :op [])
                       (assoc :functions (assoc functions name (->Function name label [] args))))
          fun-op (-> copy-ctx
                     (set-label label)
                     VisitorImpl.
                     (.visit block)
                     .-context
                     (append-asm (asm/jump return-label))
                     :op)
          function (->Function name label fun-op args)]
      (-> ctx
          VisitorImpl.
          drop-scope
          (assoc :functions (assoc functions name function))
          VisitorImpl.)))

  (visitBlock [_ node]
    (let [block-size (-> node .expr .size)]
      (-> (reduce (fn [[i visitor] expr]
                    [(inc i) (if (and (= (inc i) block-size) (is-item-expr expr))
                               (-> visitor .-context
                                   (assoc :load-item true)
                                   VisitorImpl.
                                   (.visit expr))
                               (.visit visitor expr))]) [0 _] (.expr node)) second)))

  (visitIf [_ node]
    (let [ctx (->> node
                   .-cond
                   (.visit _)
                   .-context)
          else-label (generate-scope-name "else" "if")
          end-label (generate-scope-name "end" "if")
          visit-branch (fn [ctx branch]
                         (if (is-item-expr branch)
                           (-> ctx
                               (assoc :load-item true)
                               VisitorImpl.
                               (.visit branch)
                               .-context
                               (assoc :load-item false))
                           (-> ctx
                               VisitorImpl.
                               (.visit branch)
                               .-context)))]
      (-> ctx
          (jump-if-zero x22 else-label)
          (visit-branch (->> node .-if_branch))
          (append-asm (asm/jump end-label))
          (set-label else-label)
          (visit-branch (->> node .-else_branch))
          (set-label end-label)
          VisitorImpl.)))

  (visitExpr [_ node]
    (.visit _ (.getChild node 0)))

  (visitProg [_ ctx]
    (.visitChildren _ ctx))

  (visitTerminal [ctx _] ctx)

  (visitErrorNode [ctx _] ctx)

  (visitId [_ node]
    (let [load-item (-> _ .-context :load-item)
          var-name (->> node
                        .ID
                        .getText)]
      (-> (if load-item
            (-> _ .-context
                (load-var var-name x22)
                VisitorImpl.)
            _)
          (set-list-item var-name :var)
          VisitorImpl.)))


  (visitInt [_ node]
    (let [load-item (-> _ .-context :load-item)
          int-val (->> node
                       .getText
                       parse-long)]
      (-> (if load-item
            (-> _ .-context
                (append-asm (asm/li x22 int-val))
                VisitorImpl.)
            _)
          (set-list-item int-val :int)
          VisitorImpl.)))

  (visitStr [_ node]
    (let [str (->> node .getText)
          str (-> str (subs 1 (-> str count dec)))]
      (VisitorImpl. (set-list-item _ str :str))))

  (visitBool [_ node]
    (let [load-item (-> _ .-context :load-item)
          bool (->> node .getText (= "true"))
          val (if bool 1 0)]
      (-> (if load-item
            (-> _ .-context
                (append-asm (asm/li x22 val))
                VisitorImpl.)
            _)
          (set-list-item val :int)
          VisitorImpl.)))

  (visitList [_ node]
    (let [symbol (get-symbol node)
          ctx _
          items (reduce (fn [values expr]
                          (if (is-item-expr expr)
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
                                                             (append-asm (op-mapper by x20 reg x22))
                                                             (set-reg-by-acc reg)
                                                             (set-nested-res acc-reg)
                                                             VisitorImpl.)) visitor ns)]
                                   visitor))))

          visit-comparison-operand (fn [ctx item]
                                     (let [[reg ctx] (next-reg ctx)]
                                       (case (:type item)
                                         :int [reg (-> ctx
                                                       (append-asm (asm/li reg (:value item)))
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
                                        (let [end-label (->> (generate-scope-name by "end")
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
                                   (let [end-label (->> (generate-scope-name by "end")
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
        (:and :or) (process-logic-list items symbol)

        :recur (let [visitor ctx
                     scopes (-> ctx .-context :scope)
                     {:keys [label type]} (-> scopes first)]
                 (if (empty? scopes)
                   visitor
                   (let [ctx (let [{:keys [_ vars _]} (first scopes)]
                               (loop [i 0
                                      ctx (-> visitor .-context)]
                                 (if (>= i (count vars))
                                   ctx
                                   (let [expr (-> node (.expr i))
                                         var (get vars i)
                                         {:keys [type value]} (-> ctx :vars (get var) :stack first)
                                         cur-var value
                                         ctx (-> ctx VisitorImpl. (.visit expr))
                                         processed (if (is-item-expr expr)
                                                     (-> ctx
                                                         (assoc :load-item true)
                                                         VisitorImpl.
                                                         (.visit expr)
                                                         .-context
                                                         (assoc :load-item false))
                                                     (-> ctx VisitorImpl. (.visit expr)))]
                                         ;(let [{:keys [type value]} (-> ctx get-list-item)
                                         ;      ctx (-> ctx .-context)]
                                         ;  (if (is-item-expr expr)
                                         ;    (case type
                                         ;      :int (-> ctx (append-asm (asm/li x22 value)))
                                         ;      :var (if (= value cur-var)
                                         ;             nil
                                         ;             (let [var (-> ctx :vars (get value) :stack first)
                                         ;                   var-type (:type var)
                                         ;                   var-value (:value var)]
                                         ;               (case var-type
                                         ;                 :memory (-> ctx
                                         ;                             (append-asm (asm/lw x22 var-value)))
                                         ;                 :reg (-> ctx
                                         ;                          (append-asm (asm/add x22 x0 var-value)))
                                         ;                 :value (-> ctx
                                         ;                            (append-asm (asm/addi x22 x0 var-value)))))))
                                         ;    ctx))]
                                     (recur (inc i) (case type
                                                      :memory (-> processed
                                                                  (append-asm (asm/sw x1 value x22)))
                                                      :reg (-> processed
                                                               (append-asm (asm/add value x0 x22)))))))))]
                                     ;(if (nil? processed)
                                     ;  (recur (inc i) (-> ctx .-context))
                                     ;  (recur (inc i) (case type
                                     ;                   :memory (-> processed
                                     ;                               (append-asm (asm/sw x1 value x22)))
                                     ;                   :reg (-> processed
                                     ;                            (append-asm (asm/add value x0 x22))))))))))]
                     (case type
                       :no-recursion visitor
                       :invocation (-> ctx
                                       (append-asm (asm/jump label))
                                       VisitorImpl.)
                       :simple (-> ctx
                                   (append-asm (asm/jump label))
                                   VisitorImpl.)))))

        :id (let [visitor ctx
                  func-name (-> node .-symbol .getText)
                  {:keys [args label]} (-> ctx .-context :functions (get func-name))
                  count-args (count args)
                  visitor (loop [i 0
                                 visitor visitor]
                            (if (>= i count-args)
                              visitor
                              (let [expr (.expr node i)
                                    {:keys [addr]} (get args i)
                                    visitor (.visit visitor expr)
                                    ctx (.-context visitor)
                                    ctx (let [{:keys [type value]} (-> visitor get-list-item)]
                                          (if (is-item-expr expr)
                                            (case type
                                              :int (-> ctx (append-asm (asm/li x22 value)))
                                              :var (let [var (-> ctx :vars (get value) :stack first)
                                                         var-type (:type var)
                                                         var-value (:value var)]
                                                     (case var-type
                                                       :memory (-> ctx
                                                                   (append-asm (asm/lw x22 var-value)))
                                                       :reg (-> ctx
                                                                (append-asm (asm/add x22 x0 var-value)))
                                                       :value (-> ctx
                                                                  (append-asm (asm/addi x22 x0 var-value))))))
                                            ctx))]
                                (recur (inc i) (-> ctx
                                                   (append-asm (asm/sw x1 addr x22))
                                                   VisitorImpl.)))))]
              (-> visitor .-context
                  (append-asm (asm/li x29 label))
                  (append-asm (asm/jump invoke-label x28))
                  VisitorImpl.)))))

  (visitNot_list [_ node]
    (-> _
        (.visit (-> node .expr))
        .-context
        (jump-if-zero nested-res-reg 2)
        (set-nested-res-scalar 0)
        (jump-if-zero zero-reg 2)
        (jump-if-not-zero nested-res-reg 1)
        (set-nested-res-scalar 1)
        VisitorImpl.)))

(defn generate-system [visitor]
  (loop [op [(asm/li x31 memory-size)]
         i 1]
    (if (= i 128)
      op
      (let [addr (- memory-size max-stack i)]
        (-> op
            (conj (asm/li x2 i))
            (conj (asm/sw x1 addr x2))
            (recur (inc i)))))))

(defn sum-functions [functions]
  (reduce (fn [acc [name {:keys [op]}]]
            (-> acc
                (into op)
                (conj "\n\n"))) [] functions))

(defn visit [^ParseTree tree]
  (let [ctx (-> (->Context nil
                           5
                           (- memory-size max-stack ascii str-op-buffer)
                           {}
                           nil
                           []
                           '()
                           '()
                           {}
                           false)
                VisitorImpl.
                (.visit tree)
                .-context)
        functions (sum-functions (:functions ctx))
        op []; (generate-system v)
        ctx (assoc ctx :op (-> op
                               (into (:op ctx))
                               (conj "ebreak\n\n")
                               (into functions)))]
    ctx))




