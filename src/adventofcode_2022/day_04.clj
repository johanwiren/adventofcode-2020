(ns adventofcode-2022.day-04
  (:require [utils :as u]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn parse-input [input]
  (->> input
       (map (comp (partial map parse-long)
                  (partial re-seq #"\d+")))))

(defn full-overlap? [[a b x y]]
  (or (<= x a b y)
      (<= a x y b)))

(defn has-overlap? [[a b x y]]
  (or (<= a x b)
      (<= a y b)
      (<= x a y)
      (<= x b y)))

(defn part-1-solver [input]
  (->> (parse-input input)
       (filter full-overlap?)
       (count)))

(defn part-2-solver [input]
  (->> (parse-input input)
       (filter has-overlap?)
       (count)))

(t/deftest part-1-test
  (t/is (= 526 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 886 (time (part-2-solver input)))))
