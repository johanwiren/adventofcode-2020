(ns adventofcode-2020.day-10
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [clojure.edn :as edn]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn parse-input [in]
  (map edn/read-string in))

(defn part-1-solver [in]
  (let [adapters (vec (sort (parse-input in)))
        max (last adapters)
        all-adapters (conj adapters (+ max 3))]
    (->> all-adapters
         (reduce (fn [acc x]
                   (let [[j _] (last acc)]
                     (conj acc [x (- j x)])))
                 [[0 0]])
         (drop 1)
         (map second)
         (frequencies)
         (vals)
         (apply *))))

(defn part-2-solver [in]
  (let [adapters (sort (parse-input in))
        max (last adapters)
        all-adapters (conj adapters (+ max 3))
        paths (->> all-adapters
                   (reduce (fn [acc n]
                             (assoc acc n (+ (get acc (- n 1) 0)
                                             (get acc (- n 2) 0)
                                             (get acc (- n 3) 0))))
                           {0 1}))]
    (get paths max)))

(t/deftest part-1-test
  (t/is (= 220 (part-1-solver reference-input)))
  (t/is (= 1755 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 19208 (part-2-solver reference-input)))
  (t/is (= 4049565169664 (part-2-solver input))))
