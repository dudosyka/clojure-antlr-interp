(ns visitor-project
  (:require [clojure.string :as str]
            [math]
            [op]
            [logic]
            [asm]
            [asm :refer [x0 x1 x2 x3 x5 x20 x21 x22 x25 x26 x27 x28 x29 x31]]
            [strings]
            [errors])
  (:import (com.grammar GrammarParser GrammarParser$IdContext GrammarVisitor)
           (org.antlr.runtime.tree ParseTree)))

(def zero-reg x0)
(def nested-res-reg x22)
(def acc-reg x20)
(def memory-size 65535)
(def str-op-buffer 1000)
(def max-stack 1000)
(def ascii 128)
(def ascii-num-char-start 48)
(def min-op-reg 6)

(def invoke-label "invoke")
(def return-label "return")
(def sum-str-int-label "sum_str_int")
(def print-str-label "print_str")

(defrecord VarStackEntry [type value data-type pure-value])
(defrecord Var [name stack])
(defrecord ListItem [type value])
(defrecord Scope [name vars label type next-cell next-reg])
(defrecord Function [name label op args label-save label-load scope-name])
(defrecord FunctionArg [name addr])
(defrecord StringToLoad [string start-cell])
(defrecord Return [type value])
(defrecord Context [value next-reg next-cell vars
                    list-item op stack scope
                    functions load-item return
                    strings-to-load])

(def symbol-mapper {GrammarParser/ADD      :add
                    GrammarParser/MUL      :mul
                    GrammarParser/DIV      :div
                    GrammarParser/SUB      :sub
                    GrammarParser/EQ       :eq
                    GrammarParser/GR       :gr
                    GrammarParser/GREQ     :gr-eq
                    GrammarParser/LESS     :less
                    GrammarParser/LESSEQ   :less-eq
                    GrammarParser/AND      :and
                    GrammarParser/OR       :or
                    GrammarParser/NOT      :not
                    GrammarParser/PRINT    :print
                    GrammarParser/PRINTLN  :println
                    GrammarParser/RECUR    :recur
                    GrammarParser/ADDSTR   :add-str
                    GrammarParser/ID       :id})

(def string-ops #{:add-str :print :println :eq :recur :id})

(def type-mapper {GrammarParser/STR_TYPE :str
                  GrammarParser/INT_TYPE :int
                  GrammarParser/BOOL_TYPE :int})

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

(defn get-var [ctx var]
  (-> ctx :vars (get var) :stack first))

(defn load-var [ctx var reg]
  (let [{:keys [value type]} (get-var ctx var)]
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

(defn allocate
  ([ctx] (allocate ctx 1))
  ([ctx cell-count]
   (let [next-addr (:next-cell ctx)]
     [(assoc ctx :next-cell (- next-addr cell-count)) next-addr])))

(defn update-var-stack [vars var-name stack-entry]
  (-> vars
      (get var-name)
      (assoc :stack (conj (-> vars (get var-name) :stack) stack-entry))))

(defn create-var [visitor var-name type value data-type pure-value]
  (let [ctx (-> visitor .-context)
        vars (-> ctx :vars)
        exist? (-> vars (get var-name) nil? not)
        [ctx val type] (case type
                         :memory (-> ctx allocate (conj :memory))
                         :value [ctx value :value]
                         :reg (let [[reg ctx] (next-reg-ctx ctx)]
                                [ctx reg :reg]))
        vars (:vars ctx)
        stack-entry (->VarStackEntry type val data-type pure-value)
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

(defn string-vars-map [ctx]
  (->> ctx :vars
       (map (fn [[name {:keys [stack]}]]
              (let [{:keys [pure-value data-type]} (-> stack first)]
               {name (strings/->VarItem data-type pure-value)})))
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
  (let [ {:keys [next-cell next-reg]} ctx
        name (->> ctx :scope count (str name))
        scope (-> ctx :scope (conj (->Scope name {} label type next-cell next-reg)))]
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
        {:keys [next-cell next-reg]} (-> ctx :scope first)
        scoped-vars (map second (-> ctx :scope first :vars))]
    (reduce (fn [ctx name]
              (let [vars (-> ctx :vars)
                    var (-> vars (get name))
                    var (assoc var :stack (drop 1 (:stack var)))
                    vars (assoc vars name var)
                    scopes (:scope ctx)]
                (-> ctx
                    (assoc :scope (drop 1 scopes))
                    (assoc :vars vars)))) ctx scoped-vars)))

(defn set-reg-limit [ctx limit]
  (assoc ctx :value limit))

(defn dec-reg-limit [ctx]
  (let [reg-limit (:value ctx)]
    (assoc ctx :value (dec reg-limit))))

(defn generate-scope-name [prefix suffix]
  (str (str/replace prefix #"-" "_") "_" (long (rand 10000)) (System/currentTimeMillis) (long (rand 10000)) "_" suffix))

(defn set-return [ctx type value]
  (assoc ctx :return (->Return type value)))

(defn get-string-addr [ctx str]
  (-> (filter #(-> % :string (= str)) (:strings-to-load ctx)) first :start-cell))

(defn add-string-on-load [ctx str]
  (let [cur (:strings-to-load ctx)
        next-addr (:next-cell ctx)
        len (-> str count (+ 3) (quot 2) (+ 1))
        exists (-> (filter #(-> % :string (= str)) cur) first)]
    (if (-> exists nil? not)
      [(:start-cell exists) ctx]
      [(dec next-addr) (-> ctx
                           (assoc :strings-to-load (conj cur (->StringToLoad str (dec next-addr))))
                           (assoc :next-cell (- next-addr len)))])))

(defn invoke [ctx label]
  (-> ctx
      (append-asm (asm/li x1 -1))
      (append-asm (asm/li x29 label))
      (append-asm (asm/jump invoke-label x28))))

(defn append-sum-str-int [ctx]
  (invoke ctx sum-str-int-label))

(defn generate-save-and-load-regs [label-save label-load args used-regs]
  (let [[save-ops load-ops] (loop [save-ops []
                                   load-ops []
                                   i 0
                                   args args]
                              (if (empty? args)
                                [save-ops load-ops]
                                (let [{:keys [addr]} (get args i)
                                      save-ops (-> save-ops
                                                   (conj (asm/addi x5 x5 -1))
                                                   (into (asm/lw x2 addr))
                                                   (into [(asm/sw x5 x2)]))
                                      load-ops (-> []
                                                   (conj (asm/lw-reg x2 x5))
                                                   (into (asm/sw x3 addr x2))
                                                   (conj (asm/addi x5 x5 1))
                                                   (into load-ops))]
                                  (recur save-ops load-ops (inc i) (dissoc args i)))))
        [save-reg-ops load-reg-ops] (loop [save-ops []
                                           load-ops []
                                           regs used-regs]
                                      (if (empty? regs)
                                        [save-ops load-ops]
                                        (let [reg (str "x" (first regs))
                                              save-ops (-> save-ops
                                                           (conj (asm/addi x5 x5 -1))
                                                           (conj (asm/sw x5 reg)))
                                              load-ops (-> []
                                                           (conj (asm/lw-reg reg x5))
                                                           (conj (asm/addi x5 x5 1))
                                                           (into load-ops))]
                                          (recur save-ops load-ops (drop 1 regs)))))]
    (let [save-ops (into save-ops save-reg-ops)
          load-ops (into load-reg-ops load-ops)
          save-ops (into [(str label-save ":")] save-ops)
          load-ops (into [(str label-load ":")] load-ops)]
      [(-> save-ops
           (conj (asm/jump return-label x1)))
       (-> load-ops
           (conj (asm/jump return-label x1)))])))

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
                                  {:keys [value type data-type pure-value]} (->> node .item .ID .getText (get-var ctx))]
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
                          [[name {:keys [value type]}] ctx] (create-var visitor id var-type value type pure-value)
                          ctx (add-scoped-var ctx name i)]
                      [(inc i) (case type
                                 :value (VisitorImpl. ctx)
                                 :memory (-> ctx
                                             (append-asm (asm/sw x1 value x22))
                                             VisitorImpl.)
                                 :reg (-> ctx
                                          (append-asm (asm/add value x0 x22))
                                          dec-reg-limit
                                          VisitorImpl.))])) [0 _] (-> node .binding))
          second)))

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
          name (-> node .ID .getText)
          label (generate-scope-name name "user_defined_function")
          label-save (generate-scope-name name "user_defined_function_save_regs")
          label-load (generate-scope-name name "user_defined_function_load_regs")
          ctx (create-scope ctx name label :invocation)
          scope-name (-> ctx :scope first :name)
          [_ args ctx] (->> node
                            .arg
                            (reduce (fn [[i bindings ctx] arg]
                                      (let [name (-> arg .ID .getText)
                                            type (->> arg .type .getType (get type-mapper))
                                            [[_ {:keys [value]}] ctx] (create-var (VisitorImpl. ctx) name :memory nil type nil)
                                            ctx (add-scoped-var ctx name i)]
                                        [(inc i)
                                         (into bindings {i (->FunctionArg name value)})
                                         ctx])) [0 {} ctx]))
          block (-> node .block)
          functions (:functions ctx)
          copy-ctx (-> ctx
                       (assoc :op [])
                       (assoc :functions (assoc functions name (->Function name label [] args label-save label-load scope-name))))
          copy-ctx (-> copy-ctx
                       (set-label label)
                       VisitorImpl.
                       (.visit block)
                       .-context
                       (append-asm (asm/jump return-label)))
          used-regs (loop [next (:next-reg copy-ctx)
                           r 6
                           used []]
                      (if (<= next r)
                        used
                        (recur next (inc r) (conj used r))))
          [save-ops load-ops] (generate-save-and-load-regs label-save label-load args used-regs)
          fun-op (-> copy-ctx
                     :op
                     (into load-ops)
                     (into save-ops))
          function (->Function name label fun-op args label-save label-load scope-name)]
      (-> ctx
          VisitorImpl.
          drop-scope
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
          else-label (generate-scope-name "else" "if")
          end-label (generate-scope-name "end" "if")]
      (-> ctx
          (jump-if-zero x22 else-label)
          (visit-branch (->> node .-if_branch))
          (append-asm (asm/jump end-label))
          (set-label else-label)
          ((fn [ctx]
             (if (->> node .-else_branch nil?)
               ctx
               (visit-branch ctx (->> node .-else_branch)))))
          (set-label end-label)
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
          {:keys [data-type]} (get-var ctx var-name)]
      (-> (if load-item
            (-> _ .-context
                (load-var var-name x22)
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
                (append-asm (asm/li x22 int-val))
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
                (append-asm (asm/li x22 int-val))
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
                         (add-string-on-load str))]
      (-> (if load-item
            (-> ctx
                (append-asm (asm/li x22 addr)))
            ctx)
          (set-return :str addr)
          VisitorImpl.)))

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
          (set-return :int val)
          VisitorImpl.)))

  (visitList [_ node]
    (let [symbol (get-symbol node)
          ctx _
          items (reduce (fn [values expr]
                          (if (is-item-expr expr)
                            (let [visited (.visit ctx expr)
                                  list-item (-> visited get-list-item)]
                              (when (and (not (contains? string-ops symbol)) (= :str (:type list-item)))
                                (errors/syntax visited expr))
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
                                         :str [reg (let [str-addr (get-string-addr ctx (:value item))
                                                         [str-addr ctx] (if (nil? str-addr)
                                                                          (add-string-on-load ctx (:value item))
                                                                          [str-addr ctx])]
                                                     (-> ctx
                                                         (append-asm (asm/li reg str-addr))
                                                         VisitorImpl.))]

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
                                             (let [ns (drop 1 ns)
                                                   next (first ns)]
                                               (recur ns next (-> ctx
                                                                  (visit-logic-item item)
                                                                  (check-logic-item by end-label))))))

                                         .-context
                                         (set-label end-label)
                                         VisitorImpl.)))))
          process-string-list (fn [prepend]
                                (let [items (if (nil? prepend)
                                              items
                                              (conj items (->ListItem :str prepend)))
                                      visitor ctx
                                      ctx (.-context visitor)
                                      vars (string-vars-map ctx)
                                      simplified (strings/simplify-str-sum items vars)
                                      new-str [[(count simplified) nil]]
                                      [str-map ctx] (->> simplified
                                                         (filter #(-> % :type (= :str)))
                                                         (map (fn [{:keys [value]}] value))
                                                         (reduce (fn [[res ctx] item] (let [[addr ctx] (add-string-on-load ctx item)]
                                                                                        [(assoc res item addr) ctx])) [{} ctx]))
                                      [ctx new-str] (->> simplified
                                                         (reduce (fn [[ctx new-str] {:keys [type data-type value]}]
                                                                   (let [[addr ctx replaced] (case type
                                                                                               :var (if (= data-type :int)
                                                                                                      (let [[ctx addr] (-> ctx
                                                                                                                           (load-var value x27)
                                                                                                                           (allocate 4))
                                                                                                            ctx (-> ctx
                                                                                                                    (append-asm (asm/li x26 addr))
                                                                                                                    append-sum-str-int)]
                                                                                                        [addr ctx])
                                                                                                      (let [{:keys [value pure-value]} (get-var ctx value)]
                                                                                                        (if (nil? value)
                                                                                                          [pure-value ctx]
                                                                                                          [value ctx true])))
                                                                                               :str [(get str-map value) ctx]
                                                                                               :expr (let [visitor (-> ctx (assoc :load-item true) VisitorImpl. (.visit value))
                                                                                                           ctx (-> visitor .-context (assoc :load-item false))
                                                                                                           {:keys [type]} (:return ctx)]
                                                                                                       (if (= type :int)
                                                                                                         (let [[ctx addr] (-> ctx (allocate 4))
                                                                                                               ctx (-> ctx
                                                                                                                       (append-asm (asm/add x27 x0 x22))
                                                                                                                       (append-asm (asm/li x26 addr))
                                                                                                                       append-sum-str-int)]
                                                                                                           [addr ctx])
                                                                                                         (let [[ctx addr] (allocate ctx)]
                                                                                                           [addr (-> ctx
                                                                                                                     (append-asm (asm/sw x1 addr x22))) true]))))]
                                                                     [ctx (conj new-str [(* -1 addr) replaced])])) [ctx new-str]))]
                                  (let [size (-> new-str first first)
                                        [ctx addr] (allocate ctx (inc size))
                                        [_ ctx] (reduce (fn [[addr ctx] [item replaced]]
                                                          [(dec addr) (-> (if replaced
                                                                            (-> ctx
                                                                                (append-asm (asm/lw x1 (* -1 item)))
                                                                                (append-asm (asm/li x2 -1))
                                                                                (append-asm (asm/mul x1 x1 x2)))
                                                                            (append-asm ctx (asm/li x1 item)))
                                                                          (append-asm (asm/sw x2 addr x1)))]) [addr ctx] new-str)]
                                    [addr ctx])))

          process-print (fn [[addr ctx]]
                          (-> ctx
                              (append-asm (asm/li x22 0))
                              (append-asm (asm/li x25 addr))
                              (invoke print-str-label)
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
                 (if (empty? scopes)
                   visitor
                   (let [ctx (let [{:keys [vars]} scope]
                               (when (not= (count vars) (-> node .expr .size))
                                 (errors/syntax visitor node))
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
                                                                  (append-asm (asm/sw x1 value x22)))
                                                      :reg (-> processed
                                                               (append-asm (asm/add value x0 x22)))))))))]
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
                  {:keys [args label label-load label-save scope-name]} (-> visitor .-context :functions (get func-name))
                  ctx (.-context visitor)
                  ctx (if (= (-> ctx :scope first :name) scope-name)
                        (-> ctx (invoke label-save))
                        ctx)
                  visitor (VisitorImpl. ctx)
                  count-args (count args)
                  visitor (loop [i 0
                                 visitor visitor]
                            (if (>= i count-args)
                              visitor
                              (let [expr (.expr node i)
                                    {:keys [addr]} (get args i)
                                    ctx (.-context visitor)
                                    ctx (if (is-item-expr expr)
                                          (-> ctx
                                              (assoc :load-item true)
                                              VisitorImpl.
                                              (.visit expr)
                                              .-context
                                              (assoc :load-item false))
                                          (-> ctx VisitorImpl. (.visit expr) .-context))]
                                (recur (inc i) (-> ctx
                                                   (append-asm (asm/sw x1 addr x22))
                                                   VisitorImpl.)))))]
              (if (= (-> ctx :scope first :name) scope-name)
                (-> visitor .-context
                    (invoke label)
                    (invoke label-load)
                    VisitorImpl.)
                (-> visitor .-context
                    (invoke label)
                    VisitorImpl.)))

        :add-str (let [[addr ctx] (process-string-list nil)]
                   (-> ctx
                       (append-asm (asm/li x22 addr))
                       VisitorImpl.))

        :print (-> (process-string-list nil)
                   (process-print))

        :println (-> (process-string-list "\n")
                     (process-print)))))


  (visitNot_list [_ node]
    (-> _
        (.visit (-> node .expr))
        .-context
        (jump-if-zero nested-res-reg 2)
        (set-nested-res-scalar 0)
        (append-asm (asm/jump 1))
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
  (reduce (fn [acc [_ {:keys [op]}]]
            (-> acc
                (into op)
                (conj "\n\n"))) [] functions))

(defn sum-strings [in]
  (reduce (fn [acc cur]
            (let [{:keys [string start-cell]} cur]
              (-> acc
                  (into (strings/to-asm start-cell (-> string
                                                       (str/replace #"\\n" "\n")
                                                       (str/replace #"\\t" "\t"))))
                  (conj "\n\n")))) [] in))

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
        strings-loader (sum-strings (:strings-to-load ctx))
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




