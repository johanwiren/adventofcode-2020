(ns adventofcode-2021.day-21
  (:require [clojure.test :as t]))

(def input [1 6])

(defn new-game [positions]
  (-> {:players (mapv (fn [x] {:score 0
                               :pos (dec x)})
                      positions)}
      (assoc :turn 0)))

(defn is-won? [{:keys [players]}]
  (some (partial <= 1000) (map :score players)))

(defn with-deterministic-die [game]
  (assoc game :die (flatten (repeat (range 1 (inc 100))))))

(defn roll [{:keys [die] :as game}]
  (-> game
      (assoc :roll (take 3 die))
      (update :die (partial drop 3))))

(defn current-player [{:keys [turn]}]
  (mod turn 2))

(defn move [{:keys [roll] :as game}]
  (let [player (current-player game)
        steps (apply + roll)]
    (update-in game
               [:players player :pos]
               (fn [pos] (mod (+ pos steps) 10)))))

(defn score-player [{:keys [pos] :as player}]
  (update player :score + (inc pos)))

(defn score [game]
  (let [player (current-player game)]
    (update-in game [:players player] score-player)))

(defn complete-turn [game]
  (update game :turn inc))

(defn play-game [game]
  (iterate (comp complete-turn score move roll) game))

(defn until-won [games]
  (first (drop-while (complement is-won?) games)))

(defn part-1-solver [input]
  (let [{:keys [turn players]}
        (-> (new-game input)
            (with-deterministic-die)
            (play-game)
            (until-won))
        looser-score (:score (apply (partial min-key :score) players))
        die-rolls (* turn 3)]
    (* looser-score die-rolls)))

(t/deftest part-1
  (t/is (= 604998 (part-1-solver input))))
