(let [a 10
      b 12]
  (println (if (and (< a 5) true) ; (10 < 5) and true
             123)) ; Никогда не напечатается
  (println (if (or (< a 5) (> (+ a b) 20)) ; 10 < 5 or (10 + 12) > 20
             "True"))
  (println (if (> b a)  ; 12 > 10
             (let [a b ; a = 12
                   a (* a 2)] ; a = 24
               (str a)
               "else branch"))))
