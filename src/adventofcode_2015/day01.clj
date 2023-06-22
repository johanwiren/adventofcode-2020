(ns adventofcode-2015.day01
  (:require [clojure.test :as t]
            [clojure.java.io :as io]))

(def input (slurp (io/resource "2015/day-01.txt")))

(defn part-1-solver [input]
  (->> input
       frequencies
       (into (sorted-map))
       (vals)
       (apply -)))

(defn to-num [char]
  (case char
    \( 1
    \) -1))

(defn part-2-solver [input]
  (loop [floor 0
         i 0
         [step & more] (map to-num input)]
    (if (neg? floor)
      i
      (recur (+ floor step) (inc i) more))))

(t/deftest part-1-test
  (t/is (= 138 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 1771 (part-2-solver input))))
