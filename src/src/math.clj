(ns math)

(defn is-int? [item]
  (-> item :type (= :int)))

(defn is-var? [item]
  (-> item :type (= :var)))

(defn is-str? [item]
  (-> item :type (= :str)))

(defrecord Simplified [lead simplified ns vars])
(defrecord ListItem [type value])

(defn build-simplified [[lead simplified ns vars] f default]
  (if (and (is-int? lead) (empty? ns) (empty? vars))
    (->Simplified (->ListItem :int (f (:value lead) simplified)) default ns vars)
    (->Simplified lead simplified ns vars)))

(defn simplify [arr f s var-map]
  (let [num (first arr)
        den (subvec arr 1 (count arr))
        [not-simplified simplified variables] (loop [den den
                                                     not-simplified []
                                                     simplified s
                                                     variables []]
                                                (if (-> den count zero? not)
                                                  (let [cur (first den)
                                                        den (drop 1 den)]
                                                    (if (-> cur is-int? not)
                                                      (if (-> cur is-var?)
                                                        (let [var-name (:value cur)
                                                              var-value (get var-map var-name)]
                                                          (println var-value)
                                                          (if (-> var-value nil? not)
                                                            (recur den not-simplified (f simplified var-value) variables)
                                                            (recur den not-simplified simplified (conj variables var-name))))
                                                        (recur den (conj not-simplified (:value cur)) simplified variables))
                                                      (recur den not-simplified (f simplified (:value cur)) variables)))
                                                  [not-simplified simplified variables]))]
    [num simplified not-simplified variables]))

(defn simplify-first [num not-simplified with]
  (let [ns (loop [items not-simplified
                  result []]
             (if (empty? items)
               result
               (let [cur (first items)]
                 (if (= cur num)
                   (into result (drop 1 items))
                   (recur (drop 1 items) (conj result cur))))))
        num (if (= (count not-simplified) (count ns)) num with)]
    [num ns]))

(defn simplify-sum [arr var-map]
  (-> arr
      (simplify + 0 var-map)
      (build-simplified + 0)))

(defn simplify-sub [arr var-map]
  (let [[num simplified not-simplified vars] (simplify arr + 0 var-map)
        [num ns] (simplify-first num not-simplified (->ListItem :int 0))]
    (-> [num simplified ns vars]
        (build-simplified - 0))))

(defn simplify-division [arr var-map]
  (let [[num simplified not-simplified vars] (simplify arr * 1 var-map)
        [num ns] (simplify-first num not-simplified (->ListItem :int 1))]
    (-> [num simplified ns vars]
        (build-simplified / 1))))

(defn simplify-multiplication [arr var-map]
  (-> arr
      (simplify * 1 var-map)
      (build-simplified * 1)))

;(defn simplify-str-sum [arr]
;  (loop [arr arr
;         res '()
;         prev-reduced false]
;    (if (empty? arr)
;      (reverse res)
;      (let [cur (if prev-reduced (first res) "")
;            item (first arr)]
;        (println res cur (last res))
;        (if (is-str? item)
;          (let [res (if prev-reduced (drop 1 res) res)]
;            (recur (drop 1 arr) (conj res (str cur (:value item))) true))
;          (recur (drop 1 arr) (conj res (:value item)) false))))))