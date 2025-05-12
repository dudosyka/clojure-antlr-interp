(print "Demonstration of `everything is expression` principle, use loop as expression for printing:"
       (loop [i 0]
         (if (< i 10)
           (let [step-description (str "Current step: " (+ i 1))]
             (println step-description)
             (recur (+ i 1)))
           i)))
