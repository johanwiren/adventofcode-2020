(ns adventofcode-2016.day20
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (mapv parse-long (re-seq #"\d+" line)))

(defn parse-input [input]
  (map parse-line input))

(defn part-1-solver [input]
  (->> (parse-input input)
       (sort-by second)
       (reduce (fn [allowed [low high]]
                 (if (<= low allowed high)
                   (inc high)
                   allowed))
               0)))

(defn part-2-solver [input]
  (->> (parse-input input)
       (sort-by first)
       (reduce (fn [ranges [mn mx]]
                 (let [[last-mn last-mx] (peek ranges)]
                   (cond
                     (<= last-mn mn mx last-mx)
                     ranges

                     (<= last-mn mn (inc last-mx) mx)
                     (conj (pop ranges) [last-mn mx])

                     :else
                     (conj ranges [mn mx]))))
               [[0 0]])
       (partition 2 1)
       (map (juxt (comp first second) (comp second first)))
       (map (partial apply (comp dec -)))
       (reduce +)))
