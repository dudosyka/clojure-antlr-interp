(ns interpret
  (:require [visitor])
  (:import (com.grammar GrammarLexer GrammarParser)
           (java.io FileInputStream)
           (org.antlr.v4.runtime ANTLRInputStream CommonTokenStream)))

(defn -main [args]
  (let [input-file (first args)
        tree (->> input-file
                  (new FileInputStream)
                  (new ANTLRInputStream)
                  (new GrammarLexer)
                  (new CommonTokenStream)
                  (new GrammarParser)
                  (.prog))]
    (println "Выполнение: " input-file)
    (visitor/visit tree)))

(-main ["/Users/dudosyka/IdeaProjects/compilers/src/src/examples/fib.clj"])
(-main ["/Users/dudosyka/IdeaProjects/compilers/src/src/examples/loop.clj"])
(-main ["/Users/dudosyka/IdeaProjects/compilers/src/src/examples/data_types.clj"])