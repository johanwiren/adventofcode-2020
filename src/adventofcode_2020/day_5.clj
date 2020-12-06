(ns adventofcode-2020.day-5
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [clojure.set :as set]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn to-digit [c]
  (case c
    (\B \R) \1
    (\F \L) \0))

(defn parse-line [line]
  (let [binary-str (->> (seq line)
                        (map to-digit)
                        (apply str))]
    (Integer/parseInt binary-str 2)))

(defn parse-input [in]
  (map parse-line in))

(defn part-1-solver [in]
  (->> (parse-input in)
       sort
       reverse
       first))

(defn is-front-row? [seat]
  (zero? (quot seat 8)))

(defn part-2-solver [in]
  (let [seats (parse-input in)
        available (set/difference (set (range 1024))
                                  (set seats))]
    (->> available
         sort
         (remove is-front-row?)
         (first))))

(comment

  (part-1-solver input)

  (part-2-solver input)

  (part-1-solver reference-input)


  )

(t/deftest part-1-test
  (t/is (= 820 (part-1-solver reference-input))))
