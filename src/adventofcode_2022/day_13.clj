(ns adventofcode-2022.day-13
  (:require [utils :as u]
            [clojure.edn :as edn]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (edn/read-string line))

(defn parse-input [input]
  (->> input
       (remove #{""})
       (map parse-line)))

(defn cmp [x y]
  (cond
    (every? vector? [x y])
    (let [x-len (count x)
          y-len (count y)
          res (first (keep #{-1 1} (map cmp x y)))]
      (if res
        res
        (compare x-len y-len)))

    (every? number? [x y])
    (compare x y)

    :else
    (apply cmp (mapv #(if (vector? %) % (vector %)) [x y]))))

(defn part-1-solver [input]
  (->> (parse-input input)
       (partition 2)
       (map-indexed (fn [i v] [(inc i) (apply cmp v)]))
       (filter (comp neg? second))
       (map first)
       (reduce +)))

(defn part-2-solver [input]
  (->> (conj (parse-input input) [[2]] [[6]])
       (sort cmp)
       (map-indexed (fn [i v] [(inc i) v]))
       (filter (comp #{[[2]] [[6]]} second))
       (map first)
       (reduce *)))

(t/deftest part-1-test
  (t/is (= 5330 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 27648 (time (part-2-solver input)))))
