(ns adventofcode-2021.day-05
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))

(def input (->> "2021/day_05.txt"
                (io/resource)
                (io/reader)
                (line-seq)
                (map (partial re-seq #"\d+"))
                (map (partial map #(Integer/parseInt %)))
                (map (partial partition 2))))

(defn plain-line? [[[x1 y1] [x2 y2]]]
  (or (= x1 x2)
      (= y1 y2)))

(defn steps [start end]
  (if (< start end)
    (range start (inc end))
    (range start (dec end) -1)))

(defn plain-line [[[x1 y1] [x2 y2]]]
  (for [x (steps x1 x2)
        y (steps y1 y2)]
    [x y]))

(defn diagonal-line [[[x1 y1] [x2 y2]]]
  (let [xs (steps x1 x2)
        ys (steps y1 y2)]
    (map vector xs ys)))

(defn line [line-def]
  (if (plain-line? line-def)
    (plain-line line-def)
    (diagonal-line line-def)))

(defn count-overlaps [line-defs]
  (->> line-defs
       (mapcat line)
       (frequencies)
       (filter (comp (partial < 1) second))
       (count)))

(defn part-1-solver [input]
  (count-overlaps (filter plain-line? input)))

(def part-2-solver count-overlaps)

(t/deftest part-1
  (t/is (= 7468 (part-1-solver input))))

(t/deftest part-2
  (t/is (= 22364 (part-2-solver input))))
