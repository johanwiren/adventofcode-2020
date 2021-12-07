(ns adventofcode-2021.day-07
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))

(def input (->> "2021/day_07.txt"
                (io/resource)
                (slurp)
                (re-seq #"\d+")
                (map #(Integer/parseInt %))))

(def ref-input [16,1,2,0,4,2,7,1,2,14])

(defn nth-triangle [n]
  (/ (* n (inc n)) 2))

(defn median [xs]
  (nth (sort xs) (quot (count xs) 2)))

(defn part-1-solver [input]
  (let [median (median input)]
    (->> input
         (map (fn [pos]
                (Math/abs (- median pos))))
         (reduce +))))

(defn part-2-solver [input]
  (let [mean (quot (reduce + input) (count input))]
    (->> input
         (map (fn [pos]
                (nth-triangle (Math/abs (- mean pos)))))
         (reduce +))))

(t/deftest part-1
  (t/is (= 335330 (time (part-1-solver input)))))

(t/deftest part-2
  (t/is (= 92439766 (time (part-2-solver input)))))
