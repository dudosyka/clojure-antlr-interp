(defn func [^int a]
  (let [var (not (= a 4))]
    (if var
      (recur (+ a 1))
      a)))

(let [a (func 1)
      b (+ a -3)]
  (loop [i 0
         a b]
    (if (< a 10)
      (recur (+ i 1) (+ a 1))
      (+ i a))))