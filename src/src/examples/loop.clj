(print "Демонстрация того, что любая конструкция языка - это выражение. Результат работы цикла:"
       (loop [i 0]
         (if (< i 10)
           (let [step-description (str "Текущий шаг: " i)]
             (print step-description)
             (recur (+ i 1)))
           i)))
