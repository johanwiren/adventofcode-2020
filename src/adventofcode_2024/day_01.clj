(ns adventofcode-2024.day-01
  (:require
   [clojure.string :as str]
   [clojure.test :as t]
   [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse [input]
  (map #(map parse-long (str/split % #" +")) input))

(defn part-1-solver [input]
  ;; Parse the input string into a sequence of vectors
  (->> (parse input)
       ;; Transpose the sequence of vectors to swap rows with columns
       (u/transpose)
       ;; Sort each column in ascending order
       (map sort)
       ;; For each pair of elements from corresponding columns,
       ;; compute the absolute difference and negate it
       (apply map (comp abs - ))
       ;; Sum up all the negated differences to get the final result
       (reduce +)))

(defn part-2-solver [input]
  (let [[xs ys] (u/transpose (parse input))
        ;; Count the frequency of each character at each position
        freqs (frequencies ys)]
    (->> xs
         ;; For each list of characters, multiply the length by the
         ;; frequency of the first character in that list
         (map #(* % (get freqs % 0)))
         ;; Sum all the scores to get the final result
         (reduce +))))

(t/deftest part-1-test
  (t/is (= 3246517 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 29379307 (part-2-solver input))))
