(ns adventofcode-2023.day-09
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-input [input]
  (for [line input]
    (->> line
         (re-seq #"[-\d]+")
         (map parse-long))))

(defn next-val [xs]
  (transduce (comp (take-while (partial not-every? zero?))
                   (map last))
             +
             (iterate (fn [xs] (map - (rest xs) xs)) xs)))

(defn extrapolation-sum [xs]
  (transduce (map next-val) + xs))

(defn part-1-solver [input]
  (extrapolation-sum (parse-input input)))

(defn part-2-solver [input]
  (->> (parse-input input)
       (map reverse)
       (extrapolation-sum)))
