(ns compiler
  (:gen-class)
  (:require [clojure.string :as str]
            [visitor]
            [errors]
            [visitor-asm])
  (:import (com.grammar GrammarLexer GrammarParser)
           (java.io FileInputStream)
           (errors ErrorListener)
           (org.antlr.v4.runtime ANTLRInputStream CommonTokenStream)))

(defn -main [input-file output-file]
  (swap! errors/got-errors errors/clear)
  (let [parser-error-listener (ErrorListener.)
        lexer-error-listener (ErrorListener.)
        lexer (->> input-file
                   (new FileInputStream)
                   (new ANTLRInputStream)
                   (new GrammarLexer))
        _ (.removeErrorListeners lexer)
        parser (->> lexer
                    (new CommonTokenStream)
                    (new GrammarParser))
        _ (.removeErrorListeners parser)
        _ (.addErrorListener parser parser-error-listener)
        _ (.addErrorListener lexer lexer-error-listener)
        tree (.prog parser)]
    (when @errors/got-errors
      (throw (Exception. "Compilation failed! Errors found")))
    (let [ctx (visitor-asm/visit tree)
          asm (->> ctx :op (str/join "\n"))]
      (spit output-file asm))))

;(-main "/Users/dudosyka/IdeaProjects/compilers/src/src/examples/string_test.clj" "/Users/dudosyka/IdeaProjects/compilers/src/out.asm")
(-main "/Users/dudosyka/IdeaProjects/compilers/src/src/examples/overview/defn.clj" "/Users/dudosyka/IdeaProjects/compilers/src/out.asm")