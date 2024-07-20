(ns adventofcode-2022.day-03
  (:require [utils :as u]
            [clojure.set :as set]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn priority [x]
  (let [ord (int x)]
    (if (< ord 96)
      (- ord 38)
      (- ord 96))))

(defn common-item [coll]
  (->> coll
       (map set)
       (apply set/intersection)
       (first)))

(defn split-half [coll]
  (split-at (/ (count coll) 2)
            coll))

(defn solve [coll]
  (reduce + (map (comp priority common-item) coll)))

(defn part-1-solver [input]
  (->> input
       (map split-half)
       (solve)))

(defn part-2-solver [input]
  (->> input
       (partition 3)
       (solve)))

(t/deftest part-1-test
  (t/is (= 7817 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 2444 (time (part-2-solver input)))))
