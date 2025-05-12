(defn fib [^int n]
      (if (or (= n 1) (= n 2))
          1
          (+ (fib (- n 1)) (fib (- n 2)))))

(print (fib 10))