(ns adventofcode-2022.day-08
  (:require [adventofcode-2022.utils :as u]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (mapv (comp parse-long str) line))

(defn parse-input [input]
  (mapv parse-line input))

(defn visible? [trees [y x :as pos]]
  (let [col         (mapv #(nth % x) trees)
        row         (get trees y)
        tree-height (get-in trees pos)
        shorter?    (partial > tree-height)]
    (or (some zero? pos)
        (= y (dec (count col)))
        (= x (dec (count row)))
        (let [left  (subvec row 0 x)
              right (subvec row (inc x))
              up    (subvec col 0 y)
              down  (subvec col (inc y))]
          (or (every? shorter? left)
              (every? shorter? right)
              (every? shorter? up)
              (every? shorter? down))))))

(defn part-1-solver [input]
  (let [trees (parse-input input)
        rows  (count trees)
        cols  (count (first trees))]
    (->> (for [y (range rows)
               x (range cols)]
           [y x])
         (filter (partial visible? trees))
         count)))

(defn score [trees [y x :as pos]]
  (let [col         (mapv #(nth % x) trees)
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
  (let [trees (parse-input input)
        rows  (count trees)
        cols  (count (first trees))]
    (->> (for [y (range rows)
               x (range cols)]
           [y x])
         (map (partial score trees))
         (sort)
         (last))))

(t/deftest part-1-test
  (t/is (= 1840 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 405769 (time (part-2-solver input)))))
