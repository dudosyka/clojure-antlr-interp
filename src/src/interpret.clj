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

(-main ["/Users/dudosyka/IdeaProjects/compilers/src/src/examples/list.clj"])
;(-main ["/Users/dudosyka/IdeaProjects/compilers/src/src/examples/fib.clj"])
;(-main ["/Users/dudosyka/IdeaProjects/compilers/src/src/examples/loop.clj"])
;(-main ["/Users/dudosyka/IdeaProjects/compilers/src/src/examples/data_types.clj"])