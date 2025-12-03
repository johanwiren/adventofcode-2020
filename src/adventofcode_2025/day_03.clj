(ns adventofcode-2025.day-03
  (:require
   [clojure.math :as math]
   [clojure.test :as t]
   [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (->> (re-seq #"\d" line)
       (map parse-long)))

(defn parse [input]
  (map parse-line input))

(defn to-long [digits]
  (->> (iterate inc 0)
       (map #(math/pow 10 %))
       (map * (reverse digits))
       (reduce +)
       (long)))

(defn turn-on [cells]
  (->> (range 1 (count cells))
       (map (fn [n]
              (split-at n cells)))
       (map (fn [partitions]
              (map (partial apply max) partitions)))
       (map to-long)
       (sort)
       (last)))

(let [n 2]
  (u/search
   {:pos 0
    :q [1 2 3 4 5]
    :best-digits (vec (repeat 12 0))}
   (fn [{:keys [q] :as state}]
     (when (empty? q)
       state))
   (fn [state]
     (update state :q pop))))

(defn part-1-solver [input]
  (->> (parse input)
       (map turn-on)
       (reduce +)))

(defn part-2-solver [input]
  (parse input))

(t/deftest part-1-test
  (t/is (= 17281 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 31680313976 (part-2-solver input))))
