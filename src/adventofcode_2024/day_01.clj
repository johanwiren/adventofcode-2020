(ns adventofcode-2024.day-01
  (:require
   [clojure.string :as str]
   [clojure.test :as t]
   [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse [input]
  (map #(map parse-long (str/split % #" +")) input))

(defn part-1-solver [input]
  (->> (parse input)
       (u/transpose)
       (map sort)
       (apply map (comp abs - ))
       (reduce +)))

(defn part-2-solver [input]
  (let [[xs ys] (u/transpose (parse input))
        freqs (frequencies ys)]
    (->> xs
         (map #(* % (get freqs % 0)))
         (reduce +))))

(t/deftest part-1-test
  (t/is (= 3246517 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 29379307 (part-2-solver input))))
