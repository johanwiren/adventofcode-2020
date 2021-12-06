(ns adventofcode-2021.day-06
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :as t]))

(def input (->> "2021/day_06.txt"
                (io/resource)
                (slurp)
                (re-seq #"\d")
                (map #(Integer/parseInt %))))

(defn start-gen [input]
  (frequencies input))

(defn age [fish]
  (set/rename-keys fish {0 8
                         1 0
                         2 1
                         3 2
                         4 3
                         5 4
                         6 5
                         7 6
                         8 7}))

(defn spawn [fish]
  (let [new-spawn (get fish 0 0)]
    (update fish 7 (fnil + 0) new-spawn)))

(def generation (comp age spawn))

(defn solver [n input]
  (->> input
       (start-gen)
       (iterate generation)
       (drop n)
       (first)
       (vals)
       (reduce +)))

(def part-1-solver (partial solver 80))

(def part-2-solver (partial solver 256))

(t/deftest part-1
  (t/is (= 343441 (part-1-solver input))))

(t/deftest part-2
  (t/is (= 1569108373832 (time (part-2-solver input)))))
