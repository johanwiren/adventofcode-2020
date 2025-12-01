(ns adventofcode-2025.day-01
  (:require
   [clojure.test :as t]
   [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (let [[_ sign n] (re-matches #"(R|L)(\d+)" line)
        n (parse-long n)]
    (case sign
      "R" n
      "L" (- n))))

(defn parse [input]
  (map parse-line input))

(defn part-1-solver [input]
  (->> (parse input)
       (reductions (comp #(mod % 100) +) 50)
       (filter zero?)
       (count)))

(defn expand-turn [n]
  (repeat (abs n) (if (pos? n) 1 -1)))

(defn part-2-solver [input]
  (->> (parse input)
       (remove zero?)
       (mapcat expand-turn)
       (reductions (comp #(mod % 100) +) 50)
       (filter zero?)
       (count)))

(t/deftest part-1-test
  (t/is (= 964 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 5872 (part-2-solver input))))

