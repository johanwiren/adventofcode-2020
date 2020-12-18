(ns adventofcode-2020.day-17
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [clojure.set :as set]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn cube [[c & more]]
  (let [n (map (partial + c) [-1 0 1])]
    (if (seq more)
      (mapcat (fn [x] (map #(cons % x) n))
              (cube more))
      (map list n))))

(defn active? [coords c]
  (let [n-neighbours (-> (set (cube c))
                         (disj c)
                         (set/intersection coords)
                         count)]
    (if (coords c)
      (case n-neighbours
        (2 3) true
        false)
      (= 3 n-neighbours))))

(defn generation [coords]
  (into #{}
        (comp (mapcat cube)
              (distinct)
              (filter (partial active? coords)))
        coords))

(defn parse-input [in]
  (let [rows (mapv (comp vec seq) in)
        n (count rows)]
    (for [y (range n)
          x (range n)
          :when (= \# (get-in rows [y x]))]
      [y x])))

(defn grow-to [coords n]
  (map #(into % (repeat (- n 2) 0)) coords))

(defn solve-dimension [coords d]
  (->> (grow-to coords d)
       (set)
       (iterate generation)
       (drop 6)
       first
       count))

(defn part-1-solver [in]
  (-> (parse-input in)
      (solve-dimension 3)))

(defn part-2-solver [in]
  (-> (parse-input in)
      (solve-dimension 4)))

(t/deftest part-1-test
  (t/is (= 112 (part-1-solver reference-input)))
  (t/is (= 271 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 848 (time (part-2-solver reference-input))))
  (t/is (= 2064 (time (part-2-solver input)))))
