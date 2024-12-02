(ns adventofcode-2024.day-02
  (:require
   [clojure.string :as str]
   [clojure.test :as t]
   [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse [input]
  (map #(map parse-long (str/split % #" ")) input))

(defn safe? [report]
  (let [deltas (map - report (rest report))]
    (or (every? #{1 2 3} deltas)
        (every? #{-1 -2 -3} deltas))))

(defn part-1-solver [input]
  (->> (parse input)
       (filter safe?)
       (count)))

(defn safe-v2? [report]
  (->> (range (count report))
       (map (fn [n]
              (concat (take n report) (drop (inc n) report))))
       (some safe?)))

(defn part-2-solver [input]
  (->> (parse input)
       (filter safe-v2?)
       (count)))

(t/deftest part-1-test
  (t/is (= 279 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 343 (part-2-solver input))))
