(ns asm)

(def x0 "x0")
(def x1 "x1")
(def x2 "x2")
(def x20 "x20")
(def x21 "x21")
(def x22 "x22")
(def x31 "x31")


(defn li [rd int]
  (str "li " rd ", " int))

(defn lw [rd addr]
  (if (< addr (Math/pow 2 11))
    (str "lw " rd ", x0, " addr)
    [(li x1 addr)
     (str "lw " rd ", " x1 ", 0")]))

(defn sw [buffer addr val]
  (if (< addr (Math/pow 2 11))
    (str "sw buffer, " addr ", " val)
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


(defn jump [addr]
  (str "jal " x1 ", " addr))

(defn beq [r1 r2 target]
  (str "beq " r1 ", " r2 ", " target))

(defn bne [r1 r2 target]
  (str "bne " r1 ", " r2 ", " target))