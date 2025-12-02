(ns adventofcode-2025.day-02
  (:require
   [clojure.test :as t]
   [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse [input]
  (->> (re-seq #"\d+" (first input))
       (map parse-long)
       (partition 2)))

(defn solver [input re-pattern]
  (->> (parse input)
       (mapcat (fn [[a b]] (range a (inc b))))
       (map str)
       (filter #(re-matches re-pattern %))
       (map parse-long)
       (reduce +)))

(defn part-1-solver [input]
  (solver input #"(\d+)\1"))

(defn part-2-solver [input]
  (solver input #"(\d+)(\1)+"))

(t/deftest part-1-test
  (t/is (= 26255179562 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 31680313976 (part-2-solver input))))
