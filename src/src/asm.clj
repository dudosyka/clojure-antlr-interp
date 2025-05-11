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