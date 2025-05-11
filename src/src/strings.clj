(ns strings
  (:require [asm]
            [asm :refer [x1 x2]]))


(defrecord ListItem [type data-type value])
(defrecord VarItem [type value])

(defn- is-var? [item]
  (-> item :type (= :var)))

(defn- parseable? [type value]
  (and (= type :int) (not= value nil)))

(defn- can-simplify? [type]
  (= type :str))

(defn- try-var-simplify [item var-map]
  (if (and (is-var? item) (-> var-map (get (:value item)) nil? not))
    (let [{:keys [type value]} (-> var-map (get (:value item)))]
      (if (parseable? type value)
        (->ListItem :str :str (str value))
        (->ListItem :var type (:value item))))
    item))

(defn simplify-str-sum [arr vars]
  (-> (loop [arr arr
             res '()
             prev-reduced false]
        (if (empty? arr)
          (reverse res)
          (let [cur (if prev-reduced (-> res first :value) "")
                item (-> arr first (try-var-simplify vars))]
            (if (can-simplify? (:type item))
              (let [res (if prev-reduced (drop 1 res) res)]
                (recur (drop 1 arr) (conj res (->ListItem :str :str (str cur (:value item)))) true))
              (recur (drop 1 arr) (conj res item) false)))))))

(defn string-to-ascii [on-parse]
  (loop [on-parse (mapv int on-parse)
         parsed []]
    (if (-> on-parse count zero?)
      parsed
      (let [length (count on-parse)
            part (if (> length 3)
                   3
                   length)]
        (recur
          (subvec on-parse part (-> on-parse count))
          (conj parsed (->> (subvec on-parse 0 part) reverse (mapv #(format "%03d" %)) (apply str))))))))

(defn- append-asm [op cmd]
  (if (string? cmd)
    (conj op cmd)
    (into op cmd)))

(defn to-asm [start-addr str]
  (let [addr-list (string-to-ascii str)
        size (count addr-list)
        addr-list (into [size] addr-list)
        [_ cmd] (reduce (fn [[addr cmd] val]
                          [(dec addr) (-> cmd
                                          (append-asm (asm/li x1 val))
                                          (append-asm (asm/sw x2 addr x1)))]) [start-addr []] addr-list)]
    cmd))