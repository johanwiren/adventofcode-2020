(ns adventofcode-2020.day-1
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [clojure.edn :as edn]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn parse-input [in]
  (map edn/read-string in))

(defn part-1-solver [in]
  (let [nums (parse-input in)]
    (->> (for [x nums
               y nums]
           (when (= 2020 (+ x y))
             (* x y)))
         (remove nil?)
         first)))

(defn part-2-solver [in]
  (let [nums (parse-input in)]
    (->> (for [x (sort nums)
               y (reverse (sort nums))
               z nums]
           (when (= 2020 (+ x y z))
             (* x y z)))
         (remove nil?)
         first)))

(comment

  (part-1-solver reference-input)

  (part-1-solver input)

  (time (part-2-solver input))

  )

(t/deftest part-1-test
  (t/is (= 514579 (part-1-solver reference-input))))

(t/deftest part-2-test
  (t/is (= 241861950 (part-2-solver reference-input))))
