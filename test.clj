(defn test [a] a)

(print (test 123))

(defn second [a]
      (if (< a 3)
          (second 3)
          (test 3)))

(print (second 2))