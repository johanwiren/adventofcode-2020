(ns adventofcode-2021.day-04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]))

(def input (->> "2021/day_04.txt"
                (io/resource)
                (io/reader)
                (line-seq)))

(defn board [rows]
  (->> rows
       (map (partial re-seq #"\d+"))
       (map (partial map #(Integer/parseInt %)))))

(defn parse-input [input]
  (let [calls (->> (first input)
                   (re-seq #"\d+")
                   (map #(Integer/parseInt %)))
        boards (->> input
                    (drop 2)
                    (partition-by #{""})
                    (remove #{[""]})
                    (map board))]
    {:calls calls :boards boards}))

(defn transpose [xs]
  (apply (partial map vector) xs))

(defn has-bingo? [board]
  (some true? (->> board
                   (concat (transpose board))
                   (map (partial not-any? identity)))))

(defn mark-row [call row]
  (replace (hash-map call nil) row))

(defn mark [call board]
  (map (partial mark-row call) board))

(defn call [{:keys [calls] :as game}]
  (let [call (first calls)]
    (-> game
        (update :calls (partial drop 1))
        (update :boards (partial map (partial mark call)))
        (assoc :last-call call))))

(defn check-bingo [{:keys [boards] :as game}]
  (let [winning-boards (filter has-bingo? boards)]
    (cond-> game
      (seq winning-boards) (->
                            (update :winning-boards concat winning-boards)
                            (update :boards (partial remove has-bingo?))))))

(defn play-until [pred game]
  (->> game
       (iterate (comp check-bingo call))
       (drop-while (complement pred))
       (first)))

(defn play-bingo [game]
  (play-until :winning-boards game))

(defn part-1-solver [input]
  (let [game (parse-input input)
        finished-game (play-bingo game)
        board-score (->> (:winning-boards finished-game)
                         (first)
                         (flatten)
                         (filter identity)
                         (reduce +))]
    (* board-score (:last-call finished-game))))

(defn everybody-wins [game]
  (play-until (comp empty? :boards) game))

(defn part-2-solver [input]
  (let [game (parse-input input)
        finished-game (everybody-wins game)
        board-score (->> (:winning-boards finished-game)
                         (last)
                         (flatten)
                         (filter identity)
                         (reduce +))]
    (* board-score (:last-call finished-game))))

(t/deftest part-1
  (t/is (= 38913 (part-1-solver input))))

(t/deftest part-2
  (t/is (= 16836 (part-2-solver input))))
