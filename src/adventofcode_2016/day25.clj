(ns adventofcode-2016.day25
  (:require [utils :as u]
            [adventofcode-2016.day12 :as bunny]))

(def input (u/line-seq-input *ns*))

(def impl
  {:out (fn [cpu x]
          (update cpu :out conj (get cpu x)))})

(defn init-cpu [input]
  (-> (bunny/init-cpu input)
      (update :impl merge impl)
      (assoc :out [])))

(defn get-outs [cpu a]
  (->> (assoc cpu :a a)
       (bunny/steps)
       (drop-while (comp (partial > 8) count :out))
       (first)
       (:out)))

(defn part-1-solver [input]
  (->> (range)
       (map (juxt identity (partial get-outs (init-cpu input))))
       (drop-while (comp (complement #{[0 1 0 1 0 1 0 1]}) second))
       (ffirst)))
