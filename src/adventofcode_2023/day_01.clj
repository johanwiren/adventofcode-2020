(ns adventofcode-2023.day-01
  (:require [clojure.test :as t]
            [utils :as u]))

(def input (u/line-seq-input *ns*))

(def first-last (juxt first last))

(defn parse-line [line]
  (re-seq #"\d" line))

(defn parse-input [input]
  (map parse-line input))

(defn part-1-solver [input]
  (->> (parse-input input)
       (map (comp parse-long
                  (partial apply str)
                  first-last))
       (reduce +)))

(defn parse-line-2 [line]
  (->> (range (count line))
       (map (partial subs line))
       (keep (partial re-find #"^(?:one|two|three|four|five|six|seven|eight|nine|\d)"))))

(def digit {"nine" 9
            "eight" 8
            "seven" 7
            "six" 6
            "five" 5
            "four" 4
            "three" 3
            "two" 2
            "one" 1})

(defn number [ns]
  (->> ns
       (map #(digit % %))
       (apply str)
       (parse-long)))

(defn part-2-solver [input]
  (->> input
       (map (comp number first-last parse-line-2))
       (reduce +)))

(t/deftest part-1-test
  (t/is (= 56397 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 55701 (part-2-solver input))))
