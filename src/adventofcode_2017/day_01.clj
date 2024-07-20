(ns adventofcode-2017.day-01
  (:require [utils :as u]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn parse-input [input]
  (->> input
       (first)
       (mapv (comp parse-long str))))

(defn solver [xs offset]
  (let [len (count xs)
        match? (fn [[i x]] (= (get xs (mod (+ i offset) len)) x))]
    (->> xs
         (map-indexed vector)
         (filter match?)
         (map second)
         (reduce +))))

(defn part-1-solver [input]
  (let [xs (parse-input input)]
    (solver xs 1)))

(defn part-2-solver [input]
  (let [xs (parse-input input)]
    (solver xs (quot (count xs) 2))))


(t/deftest part-1-test
  (t/is (= 1171 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 1024 (part-2-solver input))))
