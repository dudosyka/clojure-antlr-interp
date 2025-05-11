(ns errors)

(defn syntax [visitor node]
  (throw (Exception. (str "Bad operand type " (str visitor) " " (str node)))))