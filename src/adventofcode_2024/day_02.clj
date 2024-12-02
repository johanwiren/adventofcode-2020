(ns adventofcode-2024.day-02
  (:require
   [clojure.string :as str]
   [clojure.test :as t]
   [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse [input]
  (map #(map parse-long (str/split % #" ")) input))

(defn safe? [report]
  (let [deltas (map (fn [[x y]] (- x y)) (partition 2 1 report))]
    (or (every? #{1 2 3} deltas)
        (every? #{-1 -2 -3} deltas))))

(defn part-1-solver [input]
  (->> (parse input)
       (filter safe?)
       (count)))

(defn safe-v2? [report]
  (let [permutations (map (fn [n]
                            (let [[left right] (split-at n report)]
                              (concat left (rest right))))
                          (range (count report)))]
    (some safe? permutations)))

(defn part-2-solver [input]
  (->> (parse input)
       (filter safe-v2?)
       (count)))

(t/deftest part-1-test
  (t/is (= 279 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 343 (part-2-solver input))))
