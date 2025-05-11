(ns interpret
  (:require [clojure.string :as str]
            [visitor]
            [visitor-project])
  (:import (com.grammar GrammarLexer GrammarParser)
           (java.io FileInputStream)
           (org.antlr.v4.runtime ANTLRInputStream CommonTokenStream)))
;(+ 1 (+ a 2))
(defn -main [args]
  (let [input-file (first args)
        tree (->> input-file
                  (new FileInputStream)
                  (new ANTLRInputStream)
                  (new GrammarLexer)
                  (new CommonTokenStream)
                  (new GrammarParser)
                  (.prog))]
    ;(println "Выполнение: " input-file)
    ;(visitor/visit tree)
    (let [ctx (visitor-project/visit tree)
          asm (->> ctx :op (str/join "\n"))]
      (println asm))))

;(let [a "abc"
;      b "def"
;      c 5
;      d (+ c 1)]
;  (custom-println (str "1234"
;                       c
;                       d
;                       b
;                       a
;                       (let [a "hehe"
;                             a "hehehe"]
;                         a)
;                       a
;                       (if (= a "abc")
;                         "hehe"
;                         "not-hehe")))
;  (custom-println "line")
;  (custom-println "new line"))
; (defn custom-println [^str string]
;  (print "\n" "\t" string "\n"))
;
;
;(custom-println "test")
;(custom-println "test")

(-main ["/Users/dudosyka/IdeaProjects/compilers/src/src/examples/list.clj"])
;(-main ["/Users/dudosyka/IdeaProjects/compilers/src/src/examples/fib.clj"])
;(-main ["/Users/dudosyka/IdeaProjects/compilers/src/src/examples/loop.clj"])
;(-main ["/Users/dudosyka/IdeaProjects/compilers/src/src/examples/data_types.clj"])