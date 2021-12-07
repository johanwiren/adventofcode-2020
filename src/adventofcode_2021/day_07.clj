(ns adventofcode-2021.day-07
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))

(def input (->> "2021/day_07.txt"
                (io/resource)
                (slurp)
                (re-seq #"\d+")
                (map #(Integer/parseInt %))))

(def ref-input [16,1,2,0,4,2,7,1,2,14])

(defn align-cost [crabs n]
  (->> crabs
       (map (fn [[pos count]]
              (* count (Math/abs (- n pos)))))
       (reduce +)))

(defn nth-triangle [n]
  (/ (* n (inc n)) 2))

(defn align-exp-cost [crabs n]
  (->> crabs
       (map (fn [[pos count]]
              (* count (-> (- n pos)
                           Math/abs
                           nth-triangle))))
       (reduce +)))

(defn solver [align-fn input]
  (let [max (apply max input)
        min (apply min input)
        crabs (frequencies input)]
    (->> (range min (inc max))
         (map (juxt identity (partial align-fn crabs)))
         (sort-by second)
         (first)
         (second))))

(def part-1-solver (partial solver align-cost))

(def part-2-solver (partial solver align-exp-cost))

(t/deftest part-1
  (t/is (= 335330 (part-1-solver input))))

(t/deftest part-2
  (t/is (= 92439766 (part-2-solver input))))
