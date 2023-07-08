(ns adventofcode-2016.day06
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn solver
  ([input]
   (solver input <))
  ([input cmp]
   (->> input
        (apply map vector)
        (map frequencies)
        (map (fn [freqs]
               (first (last (sort-by second cmp freqs)))))
        (apply str))))

(defn part-1-solver [input]
  (solver input))

(defn part-2-solver [input]
  (solver input >))
