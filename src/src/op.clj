(ns op)

(defn- reduced-by [x next f]
  (if (reduce #(f %1 %2) x next) 1 0))

(defn eq [x & next]
  (reduced-by x next =))

(defn gr [x & next]
  (if (apply > (conj next x)) 1 0))

(defn gr-eq [x & next]
  (if (apply >= (conj next x)) 1 0))

(defn less [x & next]
  (if (apply < (conj next x)) 1 0))

(defn less-eq [x & next]
  (if (apply <= (conj next x)) 1 0))

(defn not-fun [x]
  (if (not x) 1 0))

(defn and-fun [x & next]
  (if (reduce #(and %1 %2) x next) 1 0))

(defn or-fun [x & next]
  (if (reduce #(or %1 %2) x next) 1 0))
