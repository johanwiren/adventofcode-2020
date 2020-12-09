(ns adventofcode-2020.day-8
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [adventofcode-2020.gcvm :as gcvm]
            [clojure.string :as str]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn parse-line [line]
  (let [[op arg] (str/split line #" ")]
    [(keyword op) (Integer/parseInt arg)]))

(defn parse-input [in]
  (map parse-line in))

(defn part-1-solver [in]
  (->> (parse-input in)
       gcvm/cpu
       gcvm/run
       :acc))

(defn fix-op [ops n]
  (let [[op arg] (nth ops n)]
    (case op
      :jmp (assoc ops n [:nop arg])
      :nop (assoc ops n [:jmp arg])
      nil)))

(defn bug-fixes [ops]
  (->> (range (count ops))
       (keep (partial fix-op ops))))

(defn part-2-solver [in]
  (->> (parse-input in)
       vec
       bug-fixes
       (map (comp gcvm/run gcvm/cpu))
       (filter (comp #{:finished} :state))
       first
       :acc))

(comment

  (parse-input reference-input)

  (part-1-solver reference-input)

  (part-1-solver input)

  (part-2-solver reference-input)

  (part-2-solver input)

  )

(t/deftest part-1-test
  (t/is (= 5 (part-1-solver reference-input)))
  (t/is (= 1723 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 8 (part-2-solver reference-input)))
  (t/is (= 846 (part-2-solver input))))

