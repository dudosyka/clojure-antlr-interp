(print (loop [i 0]
             (let [b (* i 2)]
                  (if (< b 3)
                    (recur (+ i 1))
                    i))))