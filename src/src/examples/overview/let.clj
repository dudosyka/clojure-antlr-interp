(let [a 123
      b (+ a 32)]
  (println a b) ; 123 150
  (let [b a
        a (* b 2)
        c 32]
    (println a b c)) ; 246 123 32
  ; Вложенный let закончился, так что контекст откатился
  ; к предыдущему состоянию:
  ; 123 150
  (println a b))
