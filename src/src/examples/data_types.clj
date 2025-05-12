(let [a 10
      b "123"
      c true]
  (println "Convert types to string:" (str a b c))
  (let [b (+ a 2 (* 3 4) (- 1 2) (/ 4 2))]
    (if c
      (println "New b value after scope overlap:" b)
      (println a)))
  (println "After exit overlapping scope old b value return back:" b))
