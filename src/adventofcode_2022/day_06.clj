(ns adventofcode-2022.day-06
  (:require [utils :as u]
            [clojure.test :as t]))

(def input (-> (u/line-seq-input *ns*)
               (first)))

(defn find-marker-pos [str marker-size]
  (->> (partition marker-size 1 str)
       (take-while (complement (partial apply distinct?)))
       (count)
       (+ marker-size)))

(defn part-1-solver [input]
  (find-marker-pos input 4))

(defn part-2-solver [input]
  (find-marker-pos input 14))

(t/deftest part-1-test
  (t/is (= 1093 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 3534 (time (part-2-solver input)))))
