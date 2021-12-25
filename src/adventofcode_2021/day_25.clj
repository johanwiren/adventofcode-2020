(ns adventofcode-2021.day-25
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))

(def input (->> "2021/day_25.txt"
                (io/resource)
                (io/reader)
                (line-seq)))

(defn parse-line [line]
  (mapv identity line))

(defn parse-input [input]
  (mapv parse-line input))

(defn do-move [dir grid]
  (let [max-x (count (first grid))
        max-y (count grid)
        positions (for [y (range max-y)
                        x (range max-x)]
                    [y x])]
    (reduce (fn [grid' [y x :as pos]]
              (case [dir (get-in grid pos)]
                [:east \>] (let [next-x (if (= (dec max-x) x) 0 (inc x))]
                             (case (get-in grid [y next-x])
                               \. (-> grid'
                                      (assoc-in [y x] \.)
                                      (assoc-in [y next-x] \>))
                               grid'))
                [:south \v] (let [next-y (if (= (dec max-y) y) 0 (inc y))]
                              (case (get-in grid [next-y x])
                                \. (-> grid'
                                       (assoc-in [y x] \.)
                                       (assoc-in [next-y x] \v))
                                grid'))
                grid'))
            grid
            positions)))

(def move (comp (partial do-move :south) (partial do-move :east)))

(defn move-all [grid]
  (->> (iterate move grid)
       (partition 2)
       (take-while (partial apply not=))
       (mapcat identity)))

(defn part-1-solver [input]
  (->> input
       parse-input
       move-all
       count))

(t/deftest part-1
  (t/is (= 482 (part-1-solver input))))
