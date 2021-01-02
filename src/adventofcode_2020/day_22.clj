(ns adventofcode-2020.day-22
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defmethod print-method clojure.lang.PersistentQueue [q, w]
  (print-method (seq q) w))

(defn cards [_ & cards]
  cards)

(defn queue [xs]
  (into clojure.lang.PersistentQueue/EMPTY xs))

(defn parse-input [in]
  (->> (partition-by #{""} in)
       (remove #{[""]})
       (map next)
       (map (partial map #(Integer/parseInt %)))
       (map queue)
       (vec)))

(defn play-cards [game]
  [(mapv peek game)
   (mapv pop game)])

(defn turn [game]
  (let [[[card1 card2 :as cards] [p1 p2]] (play-cards game)]
    (if (< card1 card2)
      [p1 (into p2 (reverse cards))]
      [(into p1 cards) p2])))

(defn answer [game]
  (->> game
       (remove empty?)
       (first)
       (reverse)
       (map-indexed (fn [i card]
                      (* (inc i) card)))
       (apply +)))

(defn part-1-solver [in]
  (->> (parse-input in)
       (iterate turn)
       (drop-while (partial every? peek))
       (first)
       (answer)))

(defn rec-game [hands]
  {:hands (mapv queue hands)
   :rounds #{}})

(declare play-rec-game)

(defn winner [{:keys [hands loop]}]
  (if (or loop
          (not (peek (second hands))))
    0
    1))

(defn rec-turn [{:keys [hands rounds] :as game}]
  (if (rounds (hash hands))
    (assoc game :loop true)
    (let [[[card1 card2 :as cards] [p1 p2]] (play-cards hands)
          winner (cond
                   (and (<= card1 (count p1))
                        (<= card2 (count p2)))
                   (winner (play-rec-game (rec-game [(take card1 p1) (take card2 p2)])))

                   :else
                   (if (< card2 card1) 0 1))
          new-hands (if (= 0 winner)
                      [(into p1 cards) p2]
                      [p1 (into p2 (reverse cards))])]
      (-> game
          (assoc :hands new-hands)
          (update :rounds conj (hash hands))))))

(defn unfinished? [{:keys [hands loop]}]
  (and (not loop)
       (every? peek hands)))

(defn play-rec-game [game]
  (->> (iterate rec-turn game)
       (drop-while unfinished?)
       first))

(defn part-2-solver [in]
  (->> (play-rec-game (rec-game (parse-input in)))
       (:hands)
       answer))

(t/deftest part-1-test
  (t/is (= 306 (part-1-solver reference-input)))
  (t/is (= 31957 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 291 (time (part-2-solver reference-input))))
  (t/is (= 33212 (time (part-2-solver input)))))
