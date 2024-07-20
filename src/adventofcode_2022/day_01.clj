(ns adventofcode-2022.day-01
  (:require [utils :as u]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn parse-input [input]
  (->> input
       (map parse-long)
       (partition-by nil?)
       (remove #{[nil]})))

(defn part-1-solver [input]
  (->> input
       (parse-input)
       (map (partial reduce +))
       (apply max)))

(defn part-2-solver [input]
  (->> input
       (parse-input)
       (map (partial reduce +))
       (sort)
       (take-last 3)
       (reduce +)))

(t/deftest part-1-test
  (t/is (= 66306 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 195292 (time (part-2-solver input)))))
