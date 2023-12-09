(ns adventofcode-2023.day-09
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-input [input]
  (mapv (comp (partial mapv parse-long)
              (partial re-seq #"[-\d]+"))
        input))

(defn next-val [xs]
  (->> xs
       (iterate (fn [xs] (map - (rest xs) xs)))
       (take-while (partial not-every? zero?))
       (map last)
       (reduce +)))

(defn part-1-solver [input]
  (->> (parse-input input)
       (map next-val)
       (reduce +)))

(defn part-2-solver [input]
  (->> (parse-input input)
       (map (comp vec reverse))
       (map next-val)
       (reduce +)))
