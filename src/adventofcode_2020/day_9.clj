(ns adventofcode-2020.day-9
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [clojure.edn :as edn]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn parse-input [in]
  (map edn/read-string in))

(defn valid [psize ns]
  (let [ps (set (take psize ns))
        sums (for [x ps
                   y ps]
               (+ x y))
        n (first (drop psize ns))]
    (first (filter (set [n]) sums))))

(defn part-1-solver [in psize]
  (->> (parse-input in)
       (iterate (partial drop 1))
       (take-while (comp (partial < psize) count))
       (remove (partial valid psize))
       first
       (drop psize)
       first))

(defn find-sum [n ns]
  (reduce (fn [[sum ns'] n']
            (let [sum' (+ sum n')]
              (cond
                (< n sum') (reduced nil)
                (= n sum') (reduced (+ (apply max ns') (apply min ns')))
                :else [sum' (conj ns' n')])))
          [0 []]
          ns))

(defn part-2-solver [in n]
  (->> (parse-input in)
       (iterate rest)
       (take-while next)
       (keep (partial find-sum n))
       (first)))

(t/deftest part-1-test
  (t/is (= 127 (part-1-solver reference-input 5)))
  (t/is (= 23278925 (part-1-solver input 25))))

(t/deftest part-2-test
  (t/is (= 62 (part-2-solver reference-input 127)))
  (t/is (= 4011064 (part-2-solver input 23278925))))
