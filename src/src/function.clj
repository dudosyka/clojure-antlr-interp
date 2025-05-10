(ns function)

(defn string_to_ascii [on-parse]
  (loop [on-parse on-parse
         parsed []]
    (if (-> on-parse count zero?)
      parsed
      (let [length (count on-parse)
            part (if (> length 3)
                   3
                   length)
            remain (subs on-parse part (-> on-parse count))
            last (->> (subs on-parse 0 part) reverse (apply str))]
        (recur
          remain
          (conj parsed (reduce (fn [acc char]
                                 (let [ascii (-> char int)]
                                   (str acc (format "%03d" ascii)))) "" last)))))))

(println (string_to_ascii "\nStack overflow"))

