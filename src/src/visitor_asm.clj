(ns visitor-asm
  (:require [clojure.string :as str]
            [math]
            [op]
            [logic]
            [asm]
            [asm :refer [x0 x1 x2 x5 x20 x22 x25 x26 x27 x31
                         nested-res-reg acc-reg memory-size str-op-buffer
                         max-stack ascii print-str-label return-label]]
            [strings]
            [scope]
            [vars]
            [errors])
  (:import (com.grammar GrammarParser$IdContext GrammarVisitor)
           (org.antlr.runtime.tree ParseTree)))

(def min-op-reg 6)

(defrecord ListItem [type value])
(defrecord Function [name label op args label-save label-load scope-name])
(defrecord FunctionArg [name addr type])
(defrecord Return [type value])
(defrecord Context [value next-reg next-cell vars
                    list-item op stack scope
                    functions load-item return
                    strings-to-load])

(defn is-item-expr [expr]
  (-> (try
        (.item expr)
        (catch Exception _ nil)) nil? not))

(defn set-list-item [visitor value type]
  (let [ctx (-> visitor .-context)]
    (assoc ctx :list-item (->ListItem type value))))

(defn get-list-item [visitor]
  (-> visitor .-context :list-item))

(defn release-reg [visitor count]
  (let [ctx (-> visitor .-context)
        next-reg (:next-reg ctx)]
    (assoc ctx :next-reg (- next-reg count))))

(defn set-return [ctx type value]
  (assoc ctx :return (->Return type value)))

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

  (visitBindings [_ node]
    (let [load-item (fn [visitor node default]
                      (let [visitor (-> visitor
                                        .-context
                                        (assoc :load-item true)
                                        VisitorImpl.
                                        (.visit node)
                                        .-context
                                        (assoc :load-item false)
                                        VisitorImpl.)
                            return (-> visitor .-context :return)
                            {:keys [value type]} (-> visitor .-context :list-item)
                            default (if (= type :str) :memory default)]
                        (if (->> node .item (instance? GrammarParser$IdContext) not)
                          [default (->ListItem type (:value return)) value visitor]
                          (let [ctx (.-context visitor)
                                  {:keys [value type data-type pure-value]} (->> node .item .ID .getText (vars/get-var ctx (.item node)))]
                            (if (= type :value)
                              [default (->ListItem data-type value) pure-value visitor]
                              [default (->ListItem data-type nil) nil visitor])))))]

      (-> (reduce (fn [[i visitor] node]
                    (let [id (-> node .ID .getText)
                          node (.expr node)
                          new-var-type (if (> (-> visitor .-context :value) 0) :reg :memory)
                          [var-type {:keys [value type]} pure-value visitor] (if (is-item-expr node)
                                                                               (load-item visitor node new-var-type)
                                                                               (let [visitor (.visit visitor node)
                                                                                     data-type (-> visitor .-context :return :type)]
                                                                                 [new-var-type (->ListItem data-type nil) nil visitor]))
                          [[name {:keys [value type]}] ctx] (vars/create-var visitor id var-type value type pure-value)
                          ctx (scope/add-scoped-var ctx name i)]
                      [(inc i) (case type
                                 :value (VisitorImpl. ctx)
                                 :memory (-> ctx
                                             (asm/append-asm (asm/sw x1 value x22))
                                             VisitorImpl.)
                                 :reg (-> ctx
                                          (asm/append-asm (asm/add value x0 x22))
                                          scope/dec-reg-limit
                                          VisitorImpl.))])) [0 _] (-> node .binding))
          second)))

  (visitLet [_ node]
    (let [ctx (-> _ .-context)
          old-reg-limit (:value ctx)
          ctx (-> ctx (scope/create-scope "let" nil :no-recursion) (scope/set-reg-limit 3) VisitorImpl.)
          ctx (->> node
                   .bindings
                   (.visit ctx))]
      (-> ctx
          (.visit (-> node .block))
          scope/drop-scope
          (scope/set-reg-limit old-reg-limit)
          VisitorImpl.)))

  (visitLoop [_ node]
    (let [ctx (-> _ .-context)
          start-label (scope/generate-scope-name "start_loop" "loop")
          ctx (-> ctx (scope/create-scope "loop" start-label :simple) (scope/set-reg-limit 3) VisitorImpl.)
          ctx (->> node
                   .bindings
                   (.visit ctx))]
      (-> ctx
          .-context
          (asm/set-label start-label)
          VisitorImpl.
          (.visit (-> node .block))
          scope/drop-scope
          VisitorImpl.)))

  (visitDefn [visitor node]
    (let [ctx (-> visitor .-context)
          name (-> node .ID .getText)
          label (scope/generate-scope-name name "user_defined_function")
          label-save (scope/generate-scope-name name "user_defined_function_save_regs")
          label-load (scope/generate-scope-name name "user_defined_function_load_regs")
          ctx (scope/create-scope ctx name label :invocation)
          scope-name (-> ctx :scope first :name)
          _ (when (-> ctx :functions (get name) nil? not)
              (errors/duplicate-declaration node name))
          [_ args ctx] (->> node
                            .arg
                            (reduce (fn [[i bindings ctx] arg]
                                      (let [name (-> arg .ID .getText)
                                            type (->> arg .type .getType (get op/type-mapper))
                                            [[_ {:keys [value]}] ctx] (vars/create-var (VisitorImpl. ctx) name :memory nil type nil)
                                            ctx (scope/add-scoped-var ctx name i)]
                                        [(inc i)
                                         (into bindings {i (->FunctionArg name value type)})
                                         ctx])) [0 {} ctx]))
          block (-> node .block)
          functions (:functions ctx)
          copy-ctx (-> ctx
                       (assoc :op [])
                       (assoc :functions (assoc functions name (->Function name label [] args label-save label-load scope-name))))
          copy-ctx (-> copy-ctx
                       (asm/set-label label)
                       VisitorImpl.
                       (.visit block)
                       .-context
                       (asm/append-asm (asm/jump return-label)))
          used-regs (loop [next (:next-reg copy-ctx)
                           r 6
                           used []]
                      (if (<= next r)
                        used
                        (recur next (inc r) (conj used r))))
          [save-ops load-ops] (asm/generate-save-and-load-regs label-save label-load args used-regs)
          fun-op (-> copy-ctx
                     :op
                     (into load-ops)
                     (into save-ops))
          function (->Function name label fun-op args label-save label-load scope-name)]
      (-> ctx
          VisitorImpl.
          scope/drop-scope
          (assoc :next-cell (:next-cell copy-ctx))
          (assoc :strings-to-load (:strings-to-load copy-ctx))
          (assoc :functions (assoc functions name function))
          VisitorImpl.)))

  (visitBlock [_ node]
    (let [block-size (-> node .expr .size)]
      (-> (reduce (fn [[i visitor] expr]
                    [(inc i) (if (and (= (inc i) block-size) (is-item-expr expr))
                               (-> visitor .-context
                                   (assoc :load-item true)
                                   VisitorImpl.
                                   (.visit expr)
                                   .-context
                                   (assoc :load-item false)
                                   VisitorImpl.)
                               (.visit visitor expr))]) [0 _] (.expr node)) second)))

  (visitIf [_ node]
    (let [visit-branch (fn [ctx branch]
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
                               .-context)))
          ctx ( -> _
                   .-context
                   (visit-branch (-> node .cond)))
          else-label (scope/generate-scope-name "else" "if")
          end-label (scope/generate-scope-name "end" "if")]
      (-> ctx
          (asm/jump-if-zero x22 else-label)
          (visit-branch (->> node .-if_branch))
          (asm/append-asm (asm/jump end-label))
          (asm/set-label else-label)
          ((fn [ctx]
             (if (->> node .-else_branch nil?)
               ctx
               (visit-branch ctx (->> node .-else_branch)))))
          (asm/set-label end-label)
          VisitorImpl.)))

  (visitExpr [_ node]
    (.visit _ (.getChild node 0)))

  (visitProg [_ node]
    (.visitChildren _ node))

  (visitTerminal [visitor _] visitor)

  (visitErrorNode [visitor _] visitor)

  (visitId [_ node]
    (let [ctx (-> _ .-context)
          load-item (-> ctx :load-item)
          var-name (->> node
                        .ID
                        .getText)
          {:keys [data-type]} (vars/get-var ctx var-name node)]
      (-> (if load-item
            (-> _ .-context
                (vars/load-var var-name x22 node)
                VisitorImpl.)
            _)
          (set-list-item var-name :var)
          (set-return data-type var-name)
          VisitorImpl.)))


  (visitInt [_ node]
    (let [load-item (-> _ .-context :load-item)
          int-val (->> node
                       .getText
                       parse-long)]
      (-> (if load-item
            (-> _ .-context
                (asm/append-asm (asm/li x22 int-val))
                VisitorImpl.)
            _)
          (set-list-item int-val :int)
          (set-return :int int-val)
          VisitorImpl.)))

  (visitNegativeInt [_ node]
    (let [load-item (-> _ .-context :load-item)
          int-val (->> node
                       .INT
                       .getText
                       parse-long
                       (* -1))]
      (-> (if load-item
            (-> _ .-context
                (asm/append-asm (asm/li x22 int-val))
                VisitorImpl.)
            _)
          (set-list-item int-val :int)
          (set-return :int int-val)
          VisitorImpl.)))

  (visitStr [_ node]
    (let [load-item (-> _ .-context :load-item)
          str (->> node .getText)
          str (-> str (subs 1 (-> str count dec)))
          [addr ctx] (-> _
                         (set-list-item str :str)
                         (strings/add-string-on-load str))]
      (-> (if load-item
            (-> ctx
                (asm/append-asm (asm/li x22 addr)))
            ctx)
          (set-return :str addr)
          VisitorImpl.)))

  (visitBool [_ node]
    (let [load-item (-> _ .-context :load-item)
          bool (->> node .getText (= "true"))
          val (if bool 1 0)]
      (-> (if load-item
            (-> _ .-context
                (asm/append-asm (asm/li x22 val))
                VisitorImpl.)
            _)
          (set-list-item val :int)
          (set-return :int val)
          VisitorImpl.)))

  (visitList [_ node]
    (let [symbol (op/get-symbol node)
          ctx _
          items (reduce (fn [values expr]
                          (if (is-item-expr expr)
                            (let [visited (.visit ctx expr)
                                  list-item (-> visited get-list-item)]
                              (when (and (not (contains? op/string-ops symbol)) (= :str (:type list-item)))
                                (errors/bad-type-for-std-function expr symbol (:type list-item) [:int :bool]))
                              (conj values list-item))
                            (conj values (->ListItem :expr expr)))) [] (.expr node))
          vars (vars/get-vars-map (-> ctx .-context))
          process-num-list (fn [items by simplifier]
                             (let [{:keys [lead simplified ns vars]} (simplifier items vars)
                                   ctx _
                                   visitor (case (:type lead)
                                             :int (-> ctx
                                                      (op/start-nested-op lead simplified by ns vars (.expr node 0))
                                                      (op/append-vars-op by vars)
                                                      VisitorImpl.)
                                             :var (-> ctx
                                                      (op/start-nested-op lead simplified by ns vars (.expr node 0))
                                                      (op/append-vars-op by vars)
                                                      VisitorImpl.)
                                             :expr (-> ctx
                                                       (.visit (:value lead)) .-context
                                                       VisitorImpl.
                                                       (op/start-nested-op lead simplified by ns vars (.expr node 0))
                                                       (op/append-vars-op by vars)
                                                       VisitorImpl.))]
                               (if (empty? ns)
                                 (let []
                                   (-> visitor .-context
                                       (asm/set-nested-res acc-reg)
                                       VisitorImpl.))
                                 (let [[reg ctx] (scope/next-reg visitor)
                                       visitor (-> ctx (asm/set-reg-by-acc reg) VisitorImpl.)
                                       visitor (reduce (fn [ctx item]
                                                         (-> ctx
                                                             (.visit item) .-context
                                                             (asm/append-asm (asm/op-mapper by x20 reg x22))
                                                             (asm/set-reg-by-acc reg)
                                                             (asm/set-nested-res acc-reg)
                                                             VisitorImpl.)) visitor ns)]
                                   visitor))))

          visit-comparison-operand (fn [ctx item]
                                     (let [[reg ctx] (scope/next-reg ctx)]
                                       (case (:type item)
                                         :str [reg (let [str-addr (strings/get-string-addr ctx (:value item))
                                                         [str-addr ctx] (if (nil? str-addr)
                                                                          (strings/add-string-on-load ctx (:value item))
                                                                          [str-addr ctx])]
                                                     (-> ctx
                                                         (asm/append-asm (asm/li reg str-addr))
                                                         VisitorImpl.))]

                                         :int [reg (-> ctx
                                                       (asm/append-asm (asm/li reg (:value item)))
                                                       VisitorImpl.)]
                                         :var [reg (-> ctx
                                                       (vars/load-var (:value item) reg node)
                                                       VisitorImpl.)]
                                         :expr (let [ctx (-> ctx (.visit (:value item)) .-context)
                                                     [reg ctx] (-> ctx VisitorImpl. scope/next-reg)
                                                     ctx (-> ctx
                                                             (asm/copy-reg-to nested-res-reg reg)
                                                             VisitorImpl.)]
                                                 [reg ctx]))))

          process-comparison-list (fn [items by]
                                    (let [f (logic/comparison-fun-mapper by)
                                          {:keys [current ns]} (logic/simplify-comparison items f vars)]
                                      (if (-> current nil? not)
                                        (-> (.-context ctx)
                                            (asm/set-nested-res-scalar (if current 1 0))
                                            VisitorImpl.)
                                        (let [end-label (->> (scope/generate-scope-name by "end")
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
                                                                (op/comparison-op-mapper by nested-res-reg reg-op1 reg-op2)
                                                                (asm/jump-if-zero nested-res-reg end-label)
                                                                VisitorImpl.)]
                                                    (recur ns (first ns) ctx))))
                                              .-context
                                              (asm/set-label end-label)
                                              VisitorImpl.)))))

          visit-logic-item (fn [ctx item]
                             (case (:type item)
                               :var (-> ctx .-context
                                        (vars/load-var (:value item) nested-res-reg node)
                                        VisitorImpl.)
                               :expr (.visit ctx (:value item))))

          check-logic-item (fn [ctx by end-label]
                             (-> (case by
                                   :and (-> ctx .-context (asm/jump-if-zero nested-res-reg end-label))
                                   :or (-> ctx .-context (asm/jump-if-not-zero nested-res-reg end-label)))
                                 VisitorImpl.))

          process-logic-list (fn [items by]
                               (let [f (logic/logic-fun-mapper by)
                                     {:keys [current ns]} (logic/simplify-logic items f vars)]
                                 (if (empty? ns)
                                   (-> ctx .context
                                       (asm/set-nested-res-scalar current)
                                       VisitorImpl.)
                                   (let [end-label (->> (scope/generate-scope-name by "end")
                                                        (drop 1)
                                                        (apply str))]
                                     (-> (loop [ns ns
                                                item (first ns)
                                                ctx ctx]
                                           (if (empty? ns)
                                             ctx
                                             (let [ns (drop 1 ns)
                                                   next (first ns)]
                                               (recur ns next (-> ctx
                                                                  (visit-logic-item item)
                                                                  (check-logic-item by end-label))))))

                                         .-context
                                         (asm/set-label end-label)
                                         VisitorImpl.)))))
          process-string-list (fn [prepend]
                                (let [items (if (nil? prepend)
                                              items
                                              (conj items (->ListItem :str prepend)))
                                      visitor ctx
                                      ctx (.-context visitor)
                                      vars (vars/string-vars-map ctx)
                                      simplified (strings/simplify-str-sum items vars)
                                      new-str [[(count simplified) nil]]
                                      [str-map ctx] (->> simplified
                                                         (filter #(-> % :type (= :str)))
                                                         (map (fn [{:keys [value]}] value))
                                                         (reduce (fn [[res ctx] item] (let [[addr ctx] (strings/add-string-on-load ctx item)]
                                                                                        [(assoc res item addr) ctx])) [{} ctx]))
                                      [ctx new-str] (->> simplified
                                                         (reduce (fn [[ctx new-str] {:keys [type data-type value]}]
                                                                   (let [[addr ctx replaced] (case type
                                                                                               :var (if (= data-type :int)
                                                                                                      (let [[ctx addr] (-> ctx
                                                                                                                           (vars/load-var value x27 node)
                                                                                                                           (vars/allocate 4))
                                                                                                            ctx (-> ctx
                                                                                                                    (asm/append-asm (asm/li x26 addr))
                                                                                                                    asm/append-sum-str-int)]
                                                                                                        [addr ctx])
                                                                                                      (let [{:keys [value pure-value]} (vars/get-var ctx value node)]
                                                                                                        (if (nil? value)
                                                                                                          [pure-value ctx]
                                                                                                          [value ctx true])))
                                                                                               :str [(get str-map value) ctx]
                                                                                               :expr (let [visitor (-> ctx (assoc :load-item true) VisitorImpl. (.visit value))
                                                                                                           ctx (-> visitor .-context (assoc :load-item false))
                                                                                                           {:keys [type]} (:return ctx)]
                                                                                                       (if (= type :int)
                                                                                                         (let [[ctx addr] (-> ctx (vars/allocate 4))
                                                                                                               ctx (-> ctx
                                                                                                                       (asm/append-asm (asm/add x27 x0 x22))
                                                                                                                       (asm/append-asm (asm/li x26 addr))
                                                                                                                       asm/append-sum-str-int)]
                                                                                                           [addr ctx])
                                                                                                         (let [[ctx addr] (vars/allocate ctx)]
                                                                                                           [addr (-> ctx
                                                                                                                     (asm/append-asm (asm/sw x1 addr x22))) true]))))]
                                                                     [ctx (conj new-str [(* -1 addr) replaced])])) [ctx new-str]))]
                                  (let [size (-> new-str first first)
                                        [ctx addr] (vars/allocate ctx (inc size))
                                        [_ ctx] (reduce (fn [[addr ctx] [item replaced]]
                                                          [(dec addr) (-> (if replaced
                                                                            (-> ctx
                                                                                (asm/append-asm (asm/lw x1 (* -1 item)))
                                                                                (asm/append-asm (asm/li x2 -1))
                                                                                (asm/append-asm (asm/mul x1 x1 x2)))
                                                                            (asm/append-asm ctx (asm/li x1 item)))
                                                                          (asm/append-asm (asm/sw x2 addr x1)))]) [addr ctx] new-str)]
                                    [addr ctx])))

          process-print (fn [[addr ctx]]
                          (-> ctx
                              (asm/append-asm (asm/li x22 0))
                              (asm/append-asm (asm/li x25 addr))
                              (asm/invoke print-str-label)
                              VisitorImpl.))]

      (case symbol
        :add (process-num-list items :add math/simplify-sum)
        :sub (process-num-list items :sub math/simplify-sub)
        :mul (process-num-list items :mul math/simplify-multiplication)
        :div (process-num-list items :div math/simplify-division)
        (:eq :gr :gr-eq :less :less-eq) (process-comparison-list items symbol)
        (:and :or) (process-logic-list items symbol)

        :recur (let [visitor ctx
                     scopes (-> ctx .-context :scope)
                     scope (-> scopes ((fn [scopes]
                                         (loop [scope (first scopes)
                                                scopes (drop 1 scopes)]
                                           (if (not= (:type scope) :no-recursion)
                                             scope
                                             (if (empty? scopes)
                                               {}
                                               (recur (first scopes) (drop 1 scopes))))))))
                     {:keys [label type]} scope]
                 (if (or (empty? scopes) (nil? label))
                   (errors/unknown-id node "recur")
                   (let [ctx (let [{:keys [vars]} scope]
                               (when (not= (count vars) (-> node .expr .size))
                                 (errors/bad-arguments-count node "recur" (count vars) (-> node .expr .size)))
                               (loop [i 0
                                      ctx (-> visitor .-context)]
                                 (if (>= i (count vars))
                                   ctx
                                   (let [expr (-> node (.expr i))
                                         var (get vars i)
                                         {:keys [type value]} (-> ctx :vars (get var) :stack first)
                                         processed (if (is-item-expr expr)
                                                     (-> ctx
                                                         (assoc :load-item true)
                                                         VisitorImpl.
                                                         (.visit expr)
                                                         .-context
                                                         (assoc :load-item false))
                                                     (-> ctx VisitorImpl. (.visit expr) .-context))]

                                     (recur (inc i) (case type
                                                      :memory (-> processed
                                                                  (asm/append-asm (asm/sw x1 value x22)))
                                                      :reg (-> processed
                                                               (asm/append-asm (asm/add value x0 x22)))))))))]
                     (case type
                       :no-recursion visitor
                       :invocation (-> ctx
                                       (asm/append-asm (asm/jump label))
                                       VisitorImpl.)
                       :simple (-> ctx
                                   (asm/append-asm (asm/jump label))
                                   VisitorImpl.)))))

        :id (let [visitor ctx
                  func-name (-> node .-symbol .getText)
                  {:keys [args label label-load label-save scope-name]} (-> visitor .-context :functions (get func-name))
                  _ (when (nil? label)
                      (errors/unknown-id node func-name))
                  ctx (.-context visitor)
                  ctx (if (= (-> ctx :scope first :name) scope-name)
                        (-> ctx (asm/invoke label-save))
                        ctx)
                  visitor (VisitorImpl. ctx)
                  count-args (count args)
                  _ (when (not= count-args (-> node .expr .size))
                      (errors/bad-arguments-count node func-name count-args (-> node .expr .size)))
                  visitor (loop [i 0
                                 visitor visitor]
                            (if (>= i count-args)
                              visitor
                              (let [expr (.expr node i)
                                    {:keys [addr type]} (get args i)
                                    ctx (.-context visitor)
                                    ctx (if (is-item-expr expr)
                                          (-> ctx
                                              (assoc :load-item true)
                                              VisitorImpl.
                                              (.visit expr)
                                              .-context
                                              (assoc :load-item false))
                                          (-> ctx VisitorImpl. (.visit expr) .-context))
                                    return-type (-> ctx :return :type)]
                                (when (not= return-type type)
                                  (errors/bad-type-for-function-invocation expr func-name type return-type))
                                (recur (inc i) (-> ctx
                                                   (asm/append-asm (asm/sw x1 addr x22))
                                                   VisitorImpl.)))))]
              (if (= (-> ctx :scope first :name) scope-name)
                (-> visitor .-context
                    (asm/invoke label)
                    (asm/invoke label-load)
                    VisitorImpl.)
                (-> visitor .-context
                    (asm/invoke label)
                    VisitorImpl.)))

        :add-str (let [[addr ctx] (process-string-list nil)]
                   (-> ctx
                       (asm/append-asm (asm/li x22 addr))
                       VisitorImpl.))

        :print (-> (process-string-list nil)
                   (process-print))

        :println (-> (process-string-list "\n")
                     (process-print)))))


  (visitNot_list [_ node]
    (-> _
        (.visit (-> node .expr))
        .-context
        (asm/jump-if-zero nested-res-reg 2)
        (asm/set-nested-res-scalar 0)
        (asm/append-asm (asm/jump 1))
        (asm/set-nested-res-scalar 1)
        VisitorImpl.)))

(defn sum-functions [functions]
  (reduce (fn [acc [_ {:keys [op]}]]
            (-> acc
                (into op)
                (conj "\n\n"))) [] functions))

(defn visit [^ParseTree tree]
  (let [ctx (-> (->Context nil
                           min-op-reg
                           (- memory-size max-stack ascii str-op-buffer)
                           {}
                           nil
                           []
                           '()
                           '()
                           {}
                           false
                           nil
                           [])
                VisitorImpl.
                (.visit tree)
                .-context)
        functions (sum-functions (:functions ctx))
        strings-loader (strings/sum-strings (:strings-to-load ctx))
        op ["# Load recursion regs"
            (asm/li x31 memory-size)
            (asm/li x5 (:next-cell ctx))
            "\n\n"
            "# Load string literals"]; (generate-system v)
        ctx (assoc ctx :op (-> op
                               (into strings-loader)
                               (into ["# Program start"])
                               (into (:op ctx))
                               (conj "ebreak\n\n# Function declarations")
                               (into functions)))]
    ctx))



