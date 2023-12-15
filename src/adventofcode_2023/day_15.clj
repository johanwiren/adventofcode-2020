(ns adventofcode-2023.day-15
  (:require [utils :as u]
            [clojure.string :as str])
  (:refer-clojure :exclude [hash]))

(def input (first (u/line-seq-input *ns*)))

(defn hash [s]
  (reduce (fn [acc x] (mod (* (+ acc x) 17) 256)) 0 (map int s)))

(defn part-1-solver [input]
  (->> (str/split input #",")
       (map hash)
       (reduce +)))

(defn part-2-solver [input]
  (->> (str/split input #",")
       (reduce (fn [acc lens-op]
                 (let [[_ label op val] (re-matches #"([^-=]+)([-=])(\d+)?" lens-op)]
                   (cond-> acc
                     (= "=" op)
                     (assoc-in [(hash label) label] (parse-long val))

                     (= "-" op)
                     (update (hash label) dissoc label))))
               {})
       (map (fn [[box-nr box]]
              (->> (vals box)
                   (map-indexed (fn [lens-nr val]
                                  (* (inc box-nr)
                                     (inc lens-nr)
                                     val)))
                   (reduce +))))
       (reduce +)))
