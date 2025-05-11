(ns errors
  (:require [clojure.string :as str])
  (:import (org.antlr.v4.runtime ANTLRErrorListener BaseErrorListener)))

(defrecord Description [msg node line char])

(def types-mapper {:int "int"
                   :str "string"
                   :bool "bool"})

(def op-mapper {:add     "+"
                :mul     "*"
                :div     "/"
                :sub     "-"
                :eq      "="
                :gr      ">"
                :gr-eq   ">="
                :less    "<"
                :less-eq "<="
                :and     "and"
                :or      "or"
                :not     "not"
                :print   "print"
                :println "println"
                :recur   "recur"
                :add-str "str"})

(defn- parse [msg node]
  (let [token (.getStart node)
        line (.getLine token)
        char (.getCharPositionInLine token)]
    (->Description msg node line char)))

(defn- pretty-throw [description]
  (let [{:keys [msg line char]} description]
    (throw (Exception. (str "Compilation error!" "\n[" line ":" char "] " msg)))))

(defn bad-type-for-std-function [node op-name got-type supported]
  (let [supported (str/join ", " (map #(get types-mapper %) supported))
        msg (str "Unexpected type: function '" (get op-mapper op-name) "' doesnt support " (get types-mapper got-type) ", supported types: " supported)
        description (parse msg node)]
    (pretty-throw description)))

(defn bad-type-for-function-invocation [node function-name expected-type provided-type]
  (let [msg (str "Unexpected argument: function '" function-name "' need " (get types-mapper expected-type) ", but " (get types-mapper provided-type) " provided")
        description (parse msg node)]
    (pretty-throw description)))

(defn bad-arguments-count [node function-name expected-count provided-count]
  (let [msg (str "Wrong number of args (" provided-count ")" " passed to: " function-name ", need " expected-count)
        description (parse msg node)]
    (pretty-throw description)))

(defn unknown-id [node name]
  (let [msg (str "Unable to resolve symbol: " name " in this context")
        description (parse msg node)]
    (pretty-throw description)))

(defn duplicate-declaration [node name]
  (let [msg (str "Duplicate declaration of function '" name "'")
        description (parse msg node)]
    (pretty-throw description)))

(def replacements {#"LOOP"      "loop"
                   #"IF"        "if"
                   #"LET"       "let"
                   #"RECUR"     "recur"
                   #"PRINT"     "print"
                   #"PRINTLN"   "println"
                   #"DEFN"      "defn"

                   #"MUL"       "mul"
                   #"DIV"       "div"
                   #"ADD"       "add"
                   #"ADDSTR"    "str"
                   #"EQ"        "="
                   #"GR"        ">"
                   #"GREQ"      ">="
                   #"LESS"      "<"
                   #"LESSEQ"    "<="
                   #"AND"       "and"
                   #"OR"        "or"
                   #"NOT"       "not"

                   #"INT"       "number"
                   #"BOOL"      "true / false"
                   #"STR"       "string"

                   #"STR_TYPE"  "^str"
                   #"INT_TYPE"  "^int"
                   #"BOOL_TYPE" "^bool"


                   #"ID"        "symbol"
                   #"NEWLINE"   "new line"
                   #"'\('"      "expression"
                   #"']'"       "end of list"})

(def keywords (->> replacements
                   (map #(re-pattern (second %)))
                   (reduce (fn [acc item]
                             (conj acc item)) #{})))

(defn replace-terminals [msg]
  (reduce (fn [acc [terminal replacement]]
            (str/replace acc terminal replacement)) msg replacements))

; God bless us
(def got-errors (atom false))

(def found (constantly true))

(def clear (constantly false))

(deftype ErrorListener []
  ANTLRErrorListener

  (syntaxError [_ recognizer symbol line char msg error]
    (swap! got-errors found)
    (let [msg (str "[" line ":" char "] " (if (str/includes? msg "token recognition error")
                                            (str/replace msg #"token recognition error" "Unexpected token")
                                            (if (str/includes? msg "input")
                                              (let [[title description] (str/split msg #"\sexpecting\s")
                                                    description (replace-terminals description)]
                                                (if (some (fn [regex] (not= (str/replace title regex "") title)) keywords)
                                                  (str title " expecting " description ", you cant use keywords in such way")
                                                  (str title " expecting " description)))
                                              msg)))]
      (println msg)))

  (reportAmbiguity [_ _ _ _ _ _ _ _])
  (reportAttemptingFullContext [_ _ _ _ _ _ _])
  (reportContextSensitivity [_ _ _ _ _ _ _]))

(def error-listener (ErrorListener.))
