(ns adventofcode-2020.day-6
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn unique-answers [group]
  (into #{}
        (mapcat seq)
        group))

(defn count-all-yes-answers [group]
  (let [n (count group)]
    (->> group
         (apply concat)
         (frequencies)
         (filter (comp #{n} second))
         (count))))

(defn parse-input [in]
  (->> (partition-by #{""} in)
       (remove #{[""]})))

(defn part-1-solver [in]
  (->> (parse-input in)
       (map unique-answers)
       (apply concat)
       (count)))

(defn part-2-solver [in]
  (->> (parse-input in)
       (map count-all-yes-answers)
       (apply +)))

(comment

  (part-1-solver reference-input)

  (part-1-solver input)

  (part-2-solver reference-input)

  (part-2-solver input)

  )

(t/deftest part-1-test
  (t/is (= 11 (part-1-solver reference-input)))
  (t/is (= 6799 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 6 (part-2-solver reference-input)))
  (t/is (= 3354 (part-2-solver input))))
