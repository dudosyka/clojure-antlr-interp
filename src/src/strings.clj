(ns strings)

(defrecord ListItem [type value])

(defn- is-var? [item]
  (-> item :type (= :var)))

(defn- parseable? [item]
  (let [type (:type item)]
    (or (= type :str) (= type :int))))

(defn- try-var-simplify [item var-map]
  (if (and (is-var? item) (-> var-map (get (:value item)) nil? not))
    (->ListItem :str (get var-map (:value item)))
    item))

(defn simplify-str-sum [arr vars]
  (loop [arr arr
         res '()
         prev-reduced false]
    (if (empty? arr)
      (reverse res)
      (let [cur (if prev-reduced (first res) "")
            item (-> arr first (try-var-simplify vars))]
        (if (parseable? item)
          (let [res (if prev-reduced (drop 1 res) res)]
            (recur (drop 1 arr) (conj res (str cur (:value item))) true))
          (recur (drop 1 arr) (conj res item) false))))))

(simplify-str-sum [(->ListItem :int 0) (->ListItem :var "a") (->ListItem :int 0) (->ListItem :int 0)] {"b" 2})
