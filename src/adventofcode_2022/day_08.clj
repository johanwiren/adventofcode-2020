(ns adventofcode-2022.day-08
  (:require [utils :as u]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (mapv (comp parse-long str) line))

(defn parse-input [input]
  (mapv parse-line input))

(defn col [matrix n]
  (mapv #(nth % n) matrix))

(defn visible? [trees [y x :as pos]]
  (let [col         (col trees x)
        row         (get trees y)
        tree-height (get-in trees pos)
        shorter?    (partial > tree-height)
        left        (subvec row 0 x)
        right       (subvec row (inc x))
        up          (subvec col 0 y)
        down        (subvec col (inc y))]
    (or (every? shorter? left)
        (every? shorter? right)
        (every? shorter? up)
        (every? shorter? down))))

(defn all-positions [matrix]
  (for [y (range (count matrix))
        x (range (count (first matrix)))]
    [y x]))

(defn part-1-solver [input]
  (let [trees (parse-input input)]
    (->> (all-positions trees)
         (filter (partial visible? trees))
         (count))))

(defn score [trees [y x :as pos]]
  (let [col         (col trees x)
        row         (get trees y)
        tree-height (get-in trees pos)
        left        (reverse (subvec row 0 x))
        right       (subvec row (inc x))
        up          (reverse (subvec col 0 y))
        down        (subvec col (inc y))]
    (->> [left right up down]
         (map (fn [trees]
                (let [[directly-visible blocking-trees]
                      (split-with (partial > tree-height) trees)]
                  (+ (count directly-visible)
                     (if (seq blocking-trees) 1 0)))))
         (apply *))))

(defn part-2-solver [input]
  (let [trees (parse-input input)]
    (->> (all-positions trees)
         (map (partial score trees))
         (apply max))))

(t/deftest part-1-test
  (t/is (= 1840 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 405769 (time (part-2-solver input)))))
