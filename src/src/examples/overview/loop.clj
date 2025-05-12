(println (loop [i 1
                a 0]
           (if (< i 10)
             (recur (+ i 1) (+ a (* i 2)))
             a)))
