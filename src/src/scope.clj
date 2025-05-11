(ns scope
  (:require [clojure.string :as str]))

(defrecord Scope [name vars label type next-cell next-reg])

(defn next-reg-ctx [ctx]
  (let [next-reg (:next-reg ctx)]
    [(str "x" next-reg) (assoc ctx :next-reg (inc next-reg))]))

(defn next-reg [visitor]
  (-> visitor .-context next-reg-ctx))

(defn create-scope [ctx name label type]
  (let [ {:keys [next-cell next-reg]} ctx
        name (->> ctx :scope count (str name))
        scope (-> ctx :scope (conj (->Scope name {} label type next-cell next-reg)))]
    (assoc ctx :scope scope)))

(defn add-scoped-var [ctx name i]
  (let [scope (-> ctx :scope first)
        scope-stack (->> ctx :scope (drop 1))
        vars (-> scope :vars
                 (assoc i name))
        scope (assoc scope :vars vars)]
    (assoc ctx :scope (conj scope-stack scope))))

(defn drop-scope [visitor]
  (let [ctx (-> visitor .-context)
        scoped-vars (map second (-> ctx :scope first :vars))]
    (reduce (fn [ctx name]
              (let [vars (-> ctx :vars)
                    var (-> vars (get name))
                    var (assoc var :stack (drop 1 (:stack var)))
                    vars (assoc vars name var)
                    scopes (:scope ctx)]
                (-> ctx
                    (assoc :scope (drop 1 scopes))
                    (assoc :vars vars)))) ctx scoped-vars)))

(defn set-reg-limit [ctx limit]
  (assoc ctx :value limit))

(defn dec-reg-limit [ctx]
  (let [reg-limit (:value ctx)]
    (assoc ctx :value (dec reg-limit))))

(defn generate-scope-name [prefix suffix]
  (str (str/replace prefix #"-" "_") "_" (long (rand 10000)) (System/currentTimeMillis) (long (rand 10000)) "_" suffix))