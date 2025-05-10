(let [a (let [d 5]
          (if (> d 3)
            3
            d))
      b a
      c (+ a b)]
  (+ a b c))