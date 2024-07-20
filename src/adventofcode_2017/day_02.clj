(ns adventofcode-2017.day-02
  (:require [utils :as u]
            [clojure.test :as t]
            [clojure.string :as str]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (->> (str/split line #"\t")
       (mapv parse-long)))

(defn parse-input [input]
  (mapv parse-line input))

(defn row-chksum [row]
  (let [sorted (sort row)
        min (first sorted)
        max (last sorted)]
    (- max min)))

(defn part-1-solver [input]
  (let [rows (parse-input input)]
    (reduce + (map row-chksum rows))))

(defn divisors [row]
  (for [x (range (count row))
        y (range (count row))
        :let [n1 (get row x)
              n2 (get row y)]
        :when (and (not= x y)
                   (zero? (rem n1 n2)))]
    [n1 n2]))

(defn part-2-solver [input]
  (->> (parse-input input)
       (map (comp (partial apply /) first divisors))
       (reduce +)))

(t/deftest part-1-test
  (t/is (= 54426 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 333 (part-2-solver input))))
