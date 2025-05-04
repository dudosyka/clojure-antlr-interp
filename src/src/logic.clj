(ns logic)

(defrecord Simplified [current ns])
(defrecord ListItem [type value])

(defn- is-int? [item]
  (-> item :type (= :int)))

(defn- is-var? [item]
  (-> item :type (= :var)))

(defn- is-str? [item]
  (-> item :type (= :str)))

(defn- try-var-simplify [item var-map]
  (if (and (is-var? item) (-> var-map (get (:value item)) nil? not))
    (->ListItem :int (get var-map (:value item)))
    item))

(defn- parse-bool [v]
  (-> v :value (not= 0)))

(defn comparison-fun-mapper [symbol]
  (case symbol
    :eq =
    :gr >
    :gr-eq >=
    :less <
    :less-eq <=))

(defn simplify-comparison [arr f var-map]
  (apply ->Simplified (loop [arr arr
                             result []]
                        (if (<= (count arr) 1)
                          (if (empty? result)
                            [true result]
                            [nil result])
                          (let [op1 (-> arr first (try-var-simplify var-map))
                                op2 (-> arr second (try-var-simplify var-map))
                                arr (drop 1 arr)]
                            (if (and (is-int? op1) (is-int? op2))
                              (if (not (f (:value op1) (:value op2)))
                                [false []]
                                (recur arr result))
                              (recur arr (conj result [op1 op2]))))))))

;(simplify-comparison [(->ListItem :var "a") (->ListItem :var "b") (->ListItem :int 0)] > {"b" 1})

(defn drop-and [v] (= v 0))

(defn drop-or [v] (= v 1))

(defn logic-fun-mapper [symbol]
  (case symbol
    :and [op/and-fun drop-and 1]
    :or [op/or-fun drop-or 0]))

(defn simplify-logic [arr [f drop-f default] var-map]
  (apply ->Simplified (loop [arr arr
                             result []
                             v default]
                        (if (empty? arr)
                          [v result]
                          (let [op (-> arr first (try-var-simplify var-map))
                                arr (drop 1 arr)]
                            (if (is-int? op)
                              (let [r (f (parse-bool op) (not= v 0))]
                                (if (drop-f r)
                                  [r []]
                                  (recur arr result r)))
                              (recur arr (conj result op) v)))))))

;(simplify-logic [(->ListItem :var "a") (->ListItem :int 0) (->ListItem :int 0)] [op/or-fun drop-or 0] {"b" 2})
;(simplify-logic [(->ListItem :var "a") (->ListItem :int 1) (->ListItem :int 0)] [op/and-fun drop-and 1] {"b" 2})
