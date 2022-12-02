(ns adventofcode-2022.day-02
  (:require [adventofcode-2022.utils :as u]
            [clojure.string :as str]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (->> (str/split line #" ")
       (mapv keyword)))

(defn parse-input [input]
  (map parse-line input))

(def score-play
  {[:A :A] 3
   [:A :B] 6
   [:A :C] 0
   [:B :A] 0
   [:B :B] 3
   [:B :C] 6
   [:C :A] 6
   [:C :B] 0
   [:C :C] 3})

(def item-score {:A 1, :B 2, :C 3})

(defn score [play]
  (+ (score-play play)
     (item-score (last play))))

(def strat->play {:X :A, :Y :B, :Z :C})

(defn part-1-solver [input]
  (->> (parse-input input)
       (map (fn [[opp strat]]
              [opp (strat->play strat)]))
       (map score)
       (reduce +)))

(def p2-strat
  {[:A :X] :C
   [:A :Y] :A
   [:A :Z] :B
   [:B :X] :A
   [:B :Y] :B
   [:B :Z] :C
   [:C :X] :B
   [:C :Y] :C
   [:C :Z] :A})

(defn part-2-solver [input]
  (->> (parse-input input)
       (map (juxt first p2-strat))
       (map score)
       (reduce +)))

(t/deftest part-1-test
  (t/is (= 13221 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 13131 (time (part-2-solver input)))))
