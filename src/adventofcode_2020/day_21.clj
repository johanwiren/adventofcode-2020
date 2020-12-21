(ns adventofcode-2020.day-21
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [adventofcode-2020.day-16 :as day-16]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn parse-line [line]
  (let [[_ ingredients allergens] (re-matches #"(.*) \(contains (.*)\)" line)
        ingredients (str/split ingredients #" ")
        allergens (str/split allergens #", ")]
    [ingredients allergens]))

(defn parse-input [in]
  (map parse-line in))

(defn part-1-solver [in]
  (let [foods (parse-input in)
        bad-ingr (->> foods
                      (mapcat (fn [[ingr alls]]
                                (map #(hash-map % (set ingr)) alls)))
                      (apply merge-with set/intersection)
                      (vals)
                      (mapcat identity)
                      (set))
        all-ingr (mapcat first foods)]
    (->> all-ingr
         (remove bad-ingr)
         count)))

(defn allergens [foods]
  (->> foods
       (mapcat (fn [[ingr alls]]
                 (map #(hash-map % (set (map keyword ingr))) alls)))
       (apply merge-with set/intersection)))

(defn part-2-solver [in]
  (let [foods (parse-input in)
        allergens (allergens foods)
        ks (keys allergens)
        vs (->> allergens
                (vals)
                (vec)
                (day-16/shrink))]
    (str/join "," (->> (zipmap ks vs)
                       (sort-by first)
                       (map (comp name second))))))

(t/deftest part-1-test
  (t/is (= 5 (part-1-solver reference-input)))
  (t/is (= 2230 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= "mxmxvkd,sqjhc,fvjkl" (part-2-solver reference-input)))
  (t/is (= "qqskn,ccvnlbp,tcm,jnqcd,qjqb,xjqd,xhzr,cjxv" (part-2-solver input))))
