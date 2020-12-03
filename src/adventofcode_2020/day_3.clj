(ns adventofcode-2020.day-3
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn parse-line [line]
  (->> line
       seq
       repeat
       flatten))

(defn parse-input [in]
  (map parse-line in))

(defn step-range [step]
  (iterate (partial + step) 0))

(defn slope [right-step down-step]
  (map vector
       (step-range down-step)
       (step-range right-step)))

(defn walk [slope trees]
  (->> slope
       (drop 1) ;; Ignore starting point
       (map #(-> trees
                 (nth (first %) nil)
                 (nth (second %) nil)))
       (take-while identity)))

(defn part-1-solver [in]
  (->> (parse-input in)
       (walk (slope 3 1))
       (filter #{\#})
       (count)))

(defn part-2-solver [in]
  (let [trees (parse-input in)
        slopes [(slope 1 1)
                (slope 3 1)
                (slope 5 1)
                (slope 7 1)
                (slope 1 2)]]
    (->> slopes
         (map #(walk % trees))
         (map (partial filter #{\#}))
         (map count)
         (apply *))))

(comment

  (parse-input input)

  (parse-input reference-input)

  (->> (slope 3 1)
       (take 10))

  (part-1-solver reference-input)

  (part-1-solver input)

  (part-2-solver reference-input)

  (time (part-2-solver input))

  )

(t/deftest part-1-test
  (t/is (= 7 (part-1-solver reference-input))))

(t/deftest part-2-test
  (t/is (= 336 (part-2-solver reference-input))))
