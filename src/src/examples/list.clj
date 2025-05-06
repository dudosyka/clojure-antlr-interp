(let [a (+ 3 1)
      b (+ a 3)]
  (loop [i 0
         a b]
    (if (< a 10)
      (recur (+ i 1) (+ a 1))
      (+ i a))))