(let [a 1
      b "string"
      c true]
  (let [a (+ 1 (/ 8 2 2) a (* 4 5) (- 3 1 1))]
    (println a) ; 25
    (let [b (str a b c)]
      (println b (str a b c)) ; string 2525string11
      (let [c (< a 25 40)
            d (> a 10)
            e (>= a 25 50)
            f (<= a 25 24)
            g (= a 25)]
        (println c d e f g) ; 0 1 0 0 1
        (let [a (not c)
              b (and c d true)
              c (or c (and d e))]
          (println a b c)))))) ; 1 0 0