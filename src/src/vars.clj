(ns vars
  (:require [asm]
            [scope]
            [strings]
            [errors]
            [asm :refer [x0]]))

(defrecord VarStackEntry [type value data-type pure-value])
(defrecord Var [name stack])

(defn allocate
  ([ctx] (allocate ctx 1))
  ([ctx cell-count]
   (let [next-addr (:next-cell ctx)]
     [(assoc ctx :next-cell (- next-addr cell-count)) next-addr])))

(defn update-var-stack [vars var-name stack-entry]
  (-> vars
      (get var-name)
      (assoc :stack (conj (-> vars (get var-name) :stack) stack-entry))))

(defn create-var [visitor var-name type value data-type pure-value]
  (let [ctx (-> visitor .-context)
        vars (-> ctx :vars)
        exist? (-> vars (get var-name) nil? not)
        [ctx val type] (case type
                         :memory (-> ctx allocate (conj :memory))
                         :value [ctx value :value]
                         :reg (let [[reg ctx] (scope/next-reg-ctx ctx)]
                                [ctx reg :reg]))
        vars (:vars ctx)
        stack-entry (->VarStackEntry type val data-type pure-value)
        var (if (not exist?)
              (->Var var-name (conj '() stack-entry))
              (update-var-stack vars var-name stack-entry))]
    [[var-name stack-entry] (assoc ctx :vars (assoc vars var-name var))]))

(defn get-vars-map [ctx]
  (->> ctx :vars
       (filter (fn [[_ {:keys [stack]}]]
                 (let [{:keys [value type]} (first stack)]
                   (and (= type :value) (-> value nil? not)))))
       (map (fn [[name {:keys [stack]}]]
              {name (-> stack first :value)}))
       (reduce into {})))

(defn string-vars-map [ctx]
  (->> ctx :vars
       (map (fn [[name {:keys [stack]}]]
              (let [{:keys [pure-value data-type]} (-> stack first)]
                {name (strings/->VarItem data-type pure-value)})))
       (reduce into {})))

(defn get-var [ctx var node]
  (let [found (-> ctx :vars (get var) :stack first)]
    (when (nil? found)
      (errors/unknown-id node var))
    found))

(defn load-var [ctx var reg node]
  (let [{:keys [value type]} (get-var ctx var node)]
    (case type
      :memory (asm/append-asm ctx (asm/lw reg value))
      :value (asm/append-asm ctx (asm/li reg value))
      :reg (asm/append-asm ctx (asm/add reg x0 value)))))
