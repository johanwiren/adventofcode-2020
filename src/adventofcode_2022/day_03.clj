(ns adventofcode-2022.day-03
  (:require [adventofcode-2022.utils :as u]
            [clojure.set :as set]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn priority [item]
  (let [ord (int item)]
    (if (< ord 96)
      (- ord 38)
      (- ord 96))))

(defn common-item [item-lists]
  (->> item-lists
       (map set)
       (apply set/intersection)
       (first)))

(defn compartments [rucksack]
  (split-at (/ (count rucksack) 2)
            rucksack))

(defn part-1-solver [input]
  (->> input
       (map (comp priority common-item compartments))
       (reduce +)))

(defn part-2-solver [input]
  (->> input
       (partition 3)
       (map (comp priority common-item))
       (reduce +)))

(t/deftest part-1-test
  (t/is (= 7817 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 2444 (time (part-2-solver input)))))
