(ns adventofcode-2016.day03
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (map parse-long (re-seq #"\d+" line)))

(defn parse-input [input]
  (map parse-line input))

(defn valid-triangles [triangles]
  (->> triangles
       (map sort)
       (filter (fn [[a b c]]
                 (< c (+ a b))))
       (count)))

(defn part-1-solver [input]
  (->> (parse-input input)
       (valid-triangles)))

(defn part-2-solver [input]
  (->> (parse-input input)
       (partition 3)
       (mapcat (partial apply map vector))
       (valid-triangles)))
