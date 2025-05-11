(ns op
  (:require [asm]
            [vars]
            [asm :refer [x1 x2 x21 acc-reg zero-reg nested-res-reg]])
  (:import (com.grammar GrammarParser)))

(def symbol-mapper {GrammarParser/ADD      :add
                    GrammarParser/MUL      :mul
                    GrammarParser/DIV      :div
                    GrammarParser/SUB      :sub
                    GrammarParser/EQ       :eq
                    GrammarParser/GR       :gr
                    GrammarParser/GREQ     :gr-eq
                    GrammarParser/LESS     :less
                    GrammarParser/LESSEQ   :less-eq
                    GrammarParser/AND      :and
                    GrammarParser/OR       :or
                    GrammarParser/NOT      :not
                    GrammarParser/PRINT    :print
                    GrammarParser/PRINTLN  :println
                    GrammarParser/RECUR    :recur
                    GrammarParser/ADDSTR   :add-str
                    GrammarParser/ID       :id})

(def string-ops #{:add-str :print :println :eq :recur :id})

(def type-mapper {GrammarParser/STR_TYPE :str
                  GrammarParser/INT_TYPE :int
                  GrammarParser/BOOL_TYPE :int})

(defn get-symbol [ctx]
  (->> ctx
       .symbol
       .getType
       (get symbol-mapper)))

(defn comparison-op-mapper [ctx op rd r1 r2]
  (case op
    :less (asm/append-asm ctx (asm/slt rd r1 r2))
    :eq (asm/append-asm ctx (asm/seq rd r1 r2))
    :gr-eq (asm/append-asm ctx (asm/sge rd r1 r2))
    :neq (asm/append-asm ctx (asm/sne rd r1 r2))
    :less-eq (-> ctx
                 (comparison-op-mapper :less x1 r1 r2)
                 (comparison-op-mapper :eq x2 r1 r2)
                 (asm/append-asm (asm/op-mapper :add rd x1 x2))
                 (asm/append-asm (asm/li x1 2))
                 (comparison-op-mapper :eq rd rd x1))
    :gr (-> ctx
            (comparison-op-mapper :gr-eq x1 r1 r2)
            (comparison-op-mapper :neq x2 r1 r2)
            (asm/append-asm (asm/op-mapper :add rd x1 x2))
            (asm/append-asm (asm/li x1 2))
            (comparison-op-mapper :eq rd rd x1))))

(defn and-fun [x & next]
  (if (reduce #(and %1 %2) x next) 1 0))

(defn or-fun [x & next]
  (if (reduce #(or %1 %2) x next) 1 0))

(defn append-vars-op
  ([ctx op vars]
   (if (empty? vars)
     ctx
     (append-vars-op ctx op vars (first vars))))
  ([ctx op vars var]
   (if (empty? vars)
     ctx
     (let [var (-> ctx :vars (get var))
           {:keys [value type]} (-> var :stack first)
           vars (drop 1 vars)
           var (first vars)]
       (-> (case type
             :value (-> ctx
                        (asm/append-asm (asm/li x21 value))
                        (asm/append-asm (asm/op-mapper op acc-reg x21)))
             :memory (-> ctx
                         (asm/append-asm (asm/lw x21 value))
                         (asm/append-asm (asm/op-mapper op acc-reg x21)))
             :reg (asm/append-asm ctx (asm/op-mapper op acc-reg value)))
           (recur op vars var))))))

(defn start-nested-op [visitor lead simplified op ns vars node]
  (let [ctx (-> visitor .-context)
        {:keys [value type]} lead]
    (case type
      :int (if (and (empty? ns) (empty? vars))
             (asm/append-asm ctx (asm/li acc-reg value))
             (-> ctx
                 (asm/append-asm (asm/li acc-reg value))
                 (asm/append-asm (asm/li x21 simplified))
                 (asm/append-asm (asm/op-mapper op acc-reg x21))))
      :var (-> ctx
               (vars/load-var value acc-reg node)
               (asm/append-asm (asm/li x21 simplified))
               (asm/append-asm (asm/op-mapper op acc-reg x21)))
      :expr (-> ctx
                (asm/append-asm (asm/op-mapper :add acc-reg zero-reg nested-res-reg))
                (asm/append-asm (asm/li x21 simplified))
                (asm/append-asm (asm/op-mapper op acc-reg x21))))))
