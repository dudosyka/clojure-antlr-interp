(ns asm)

(def x0 "x0")
(def x1 "x1")
(def x2 "x2")
(def x3 "x3")
(def x5 "x5")
(def x20 "x20")
(def x21 "x21")
(def x22 "x22")
(def x25 "x25")
(def x26 "x26")
(def x27 "x27")
(def x28 "x28")
(def x29 "x29")
(def x31 "x31")

(def zero-reg x0)
(def nested-res-reg x22)
(def acc-reg x20)
(def memory-size 65535)
(def str-op-buffer 1000)
(def max-stack 1000)
(def ascii 128)

(def invoke-label "invoke")
(def sum-str-int-label "sum_str_int")
(def return-label "return")
(def print-str-label "print_str")

(defn li [rd int]
  (str "li " rd ", " int))

(defn lw [rd addr]
  (if (< addr (Math/pow 2 11))
    (str "lw " rd ", x0, " addr)
    [(li x1 addr)
     (str "lw " rd ", " x1 ", 0")]))

(defn lw-reg [rd r1]
  (str "lw " rd ", " r1 ", 0"))

(defn sw
  ([dest-reg val-reg]
   (str "sw " dest-reg ", 0, " val-reg))
  ([buffer addr val]
   [(li buffer addr)
    (str "sw " buffer ", 0, " val)]))

(defn slt [rd r1 r2]
  (str "slt " rd ", " r1 ", " r2))

(defn seq [rd r1 r2]
  (str "seq " rd ", " r1 ", " r2))

(defn sge [rd r1 r2]
  (str "sge " rd ", " r1 ", " r2))

(defn sne [rd r1 r2]
  (str "sne " rd ", " r1 ", " r2))

(defn add [rd r1 r2]
  (str "add " rd ", " r1 ", " r2))

(defn addi [rd r1 value]
  (str "addi " rd ", " r1 ", " value))

(defn mul [rd r1 r2]
  (str "mul " rd ", " r1 ", " r2))

(defn div [rd r1 r2]
  (str "div " rd ", " r1 ", " r2))

(defn sub [rd r1 r2]
  (str "sub " rd ", " r1 ", " r2))

(defn jump
  ([addr] (jump addr x1))
  ([addr reg] (str "jal " reg ", " addr)))

(defn beq [r1 r2 target]
  (str "beq " r1 ", " r2 ", " target))

(defn bne [r1 r2 target]
  (str "bne " r1 ", " r2 ", " target))

(defn op-mapper
  ([op op1 op2]
   (op-mapper op op1 op1 op2))
  ([op rd op1 op2]
   (case op
     :add (add rd op1 op2)
     :mul (mul rd op1 op2)
     :div (div rd op1 op2)
     :sub (sub rd op1 op2)
     :eq (seq rd op1 op2))))

(defn append-asm [ctx cmd]
  (let [op (:op ctx)]
    (if (string? cmd)
      (assoc ctx :op (conj op cmd))
      (assoc ctx :op (into op cmd)))))

(defn set-reg-by-acc [ctx reg]
  (append-asm ctx (op-mapper :add reg zero-reg acc-reg)))

(defn copy-reg-to [ctx from to]
  (append-asm ctx (op-mapper :add to zero-reg from)))

(defn set-nested-res [ctx val]
  (-> ctx
      (append-asm (op-mapper :add nested-res-reg zero-reg val))))

(defn set-nested-res-scalar [ctx val]
  (-> ctx
      (append-asm (li nested-res-reg val))))

(defn jump-if-zero [ctx reg target]
  (append-asm ctx (beq reg zero-reg target)))

(defn jump-if-not-zero [ctx reg target]
  (append-asm ctx (bne reg zero-reg target)))

(defn set-label [ctx label]
  (append-asm ctx (str label ":")))

(defn invoke [ctx label]
  (-> ctx
      (append-asm (li x1 -1))
      (append-asm (li x29 label))
      (append-asm (jump invoke-label x28))))

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
                                                   (conj (addi x5 x5 -1))
                                                   (into (lw x2 addr))
                                                   (into [(sw x5 x2)]))
                                      load-ops (-> []
                                                   (conj (lw-reg x2 x5))
                                                   (into (sw x3 addr x2))
                                                   (conj (addi x5 x5 1))
                                                   (into load-ops))]
                                  (recur save-ops load-ops (inc i) (dissoc args i)))))
        [save-reg-ops load-reg-ops] (loop [save-ops []
                                           load-ops []
                                           regs used-regs]
                                      (if (empty? regs)
                                        [save-ops load-ops]
                                        (let [reg (str "x" (first regs))
                                              save-ops (-> save-ops
                                                           (conj (addi x5 x5 -1))
                                                           (conj (sw x5 reg)))
                                              load-ops (-> []
                                                           (conj (lw-reg reg x5))
                                                           (conj (addi x5 x5 1))
                                                           (into load-ops))]
                                          (recur save-ops load-ops (drop 1 regs)))))]
    (let [save-ops (into save-ops save-reg-ops)
          load-ops (into load-reg-ops load-ops)
          save-ops (into [(str label-save ":")] save-ops)
          load-ops (into [(str label-load ":")] load-ops)]
      [(-> save-ops
           (conj (jump return-label x1)))
       (-> load-ops
           (conj (jump return-label x1)))])))
