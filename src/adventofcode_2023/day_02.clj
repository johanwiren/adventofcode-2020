(ns adventofcode-2023.day-02
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (let [[_ game & turns] (str/split line #"Game |: |; ")]
    {:game (parse-long game)
     :turns (map (fn [turn]
                   (->> (str/split turn #", | ")
                        (partition 2)
                        (map (fn [[n color]]
                               [(keyword color) (parse-long n)]))
                        (into {})))
                 turns)}))

(defn parse-input [input]
  (map parse-line input))

(defn valid-turn? [turn]
  (->> turn
       (merge-with - {:red 12 :green 13 :blue 14})
       (vals)
       (not-any? neg?)))

(def valid-game?
  (comp (partial every? valid-turn?) :turns))

(defn part-1-solver [input]
  (->> (parse-input input)
       (filter valid-game?)
       (map :game)
       (reduce +)))

(def fewest-valid-cubes
  (comp (partial apply merge-with max) :turns))

(def cubes-total-power
  (comp (partial reduce *) vals))

(defn part-2-solver [input]
  (->> (parse-input input)
       (map fewest-valid-cubes)
       (map cubes-total-power)
       (reduce +)))

(t/deftest part-1-test
  (t/is (= 2256 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 74229 (part-2-solver input))))
