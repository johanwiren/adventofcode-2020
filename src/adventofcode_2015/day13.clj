(ns adventofcode-2015.day13
  (:require [clojure.java.io :as io]
            [adventofcode-2015.day09 :as day09]))

(def input (line-seq (io/reader (io/resource "2015/day-13.txt"))))

(def ref-input ["Alice would gain 54 happiness units by sitting next to Bob."
                "Alice would lose 79 happiness units by sitting next to Carol."
                "Alice would lose 2 happiness units by sitting next to David."
                "Bob would gain 83 happiness units by sitting next to Alice."
                "Bob would lose 7 happiness units by sitting next to Carol."
                "Bob would lose 63 happiness units by sitting next to David."
                "Carol would lose 62 happiness units by sitting next to Alice."
                "Carol would gain 60 happiness units by sitting next to Bob."
                "Carol would gain 55 happiness units by sitting next to David."
                "David would gain 46 happiness units by sitting next to Alice."
                "David would lose 7 happiness units by sitting next to Bob."
                "David would gain 41 happiness units by sitting next to Carol."])


(defn parse-line [line]
  (let [[p1 verb pts p2] (rest (re-matches #"(.*) would (.*) (.*) happiness units by sitting next to (.*)\." line))
        pts-int (Integer/parseInt pts)]
    {(set [p1 p2]) (if (= "gain" verb) pts-int (- pts-int))}))

(defn parse-input [input]
  (map parse-line input))

(defn score [scores seating]
  (let [pairs (concat (partition 2 seating)
                      (partition 2 (drop 1 seating))
                      [[(first seating) (last seating)]])]
    (->> pairs
         (map set)
         (map #(get scores % 0))
         (reduce +))))

(defn part-1-solver [input]
  (let [scores (apply (partial merge-with +) (parse-input input))
        seatings (day09/permutations (set (mapcat identity (keys scores))))]
    (->> seatings
         (pmap (partial score scores))
         (sort)
         (last))))

(defn part-2-solver [input]
  (let [scores (apply (partial merge-with +) (parse-input input))
        seatings (day09/permutations (conj (set (mapcat identity (keys scores))) "me"))]
    (->> seatings
         (pmap (partial score scores))
         (sort)
         (last))))

(comment

  (sort (range 10))

  (part-2-solver input)

  (part-1-solver input)

  (parse-input input))
