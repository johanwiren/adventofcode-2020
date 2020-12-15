(ns adventofcode-2020.day-15
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn init-game [ns]
  (reduce (fn [acc [i n]]
            (-> (update acc n (comp (partial cons (inc i))
                                    (partial take 1)))
                (assoc :spoken n
                       :turn (inc i))))
          {}
          (map-indexed (fn [i n]
                         [i n])
                       ns)))

(defn play-turn [{:keys [turn spoken] :as game}]
  (let [this-turn (inc turn)
        n (max 0
               (->> (get game spoken [0])
                    (apply -)))
        prev (take 1 (get game n))]
    (assoc! game
            :spoken n
            :turn this-turn
            n (cons this-turn prev))))

(defn parse-input [in]
  (init-game
   (map edn/read-string
        (-> in
            first
            (str/split #",")))))

(defn play-game [{:keys [turn] :as game} turns]
  (->> (transient game)
       (iterate play-turn)
       (drop (- turns turn))
       (first)
       (:spoken)))

(defn part-1-solver [in]
  (let [game (parse-input in)]
    (play-game game 2020)))

(defn part-2-solver [in]
  (let [game (parse-input in)]
    (play-game game 30000000)))

(t/deftest part-1-test
  (t/is (= 436 (part-1-solver reference-input)))
  (t/is (= 253 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 13710 (time (part-2-solver input)))))
