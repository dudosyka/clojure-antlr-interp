; Работает с константами любого типа
(println "Some" "arguments" 1 true)

; Не добавит перенос строки в конце
(print "Simple print")

(let [a 123
      b "test"
      c true]
  ; Работает с переменными любого типа
  (println a b c))

; Вложенный print выведет 123, а внешний 0
(println (println 123))