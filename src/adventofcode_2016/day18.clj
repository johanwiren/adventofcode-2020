(ns adventofcode-2016.day18
  (:require [utils :as u]))

(def input (first (u/line-seq-input *ns*)))

(defn parse-input [input]
  (mapv {"^" true "." false} (re-seq #"." input)))

(defn next-row [row]
  (into (vector-of :boolean)
        (map (fn [i]
               (let [left (get row (dec i) false)
                     center (get row i)
                     right (get row (inc i) false)]
                 (or (and left center (not right))
                     (and center right (not left))
                     (and left (not center) (not right))
                     (and right (not center) (not left))))))
        (range (count row))))

(defn generate-tiles [row]
  (->> row
       (vector)
       (iterate (fn [rows]
                  (let [last-row (peek rows)
                        new-row (next-row last-row)]
                    [last-row new-row])))
       (map last)))

(defn solver [row n]
  (->> row
       (generate-tiles)
       (take n)
       (map (fn [row] (count (filter false? row))))
       (reduce +)))

(defn part-1-solver [input]
  (solver (parse-input input) 40))

(defn part-2-solver [input]
  (time (solver (parse-input input) 400000)))
