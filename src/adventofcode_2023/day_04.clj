(ns adventofcode-2023.day-04
  (:require [clojure.string :as str]
            [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (let [[[card & winning] numbers]
        (->> (str/split line #"\|")
             (map (comp (partial map parse-long)
                        #(re-seq #"\d+" %))))]
    {:card card
     :copies 1
     :numbers numbers
     :winning (set winning)}))

(defn parse-input [input]
  (map parse-line input))

(defn score [{:keys [numbers winning]}]
  (let [wins (count (filter winning numbers))
        bsl (dec wins)]
    (if (pos? wins)
      (bit-shift-left 1 bsl)
      0)))

(defn part-1-solver [input]
  (->> (parse-input input)
       (map score)
       (reduce +)))

(defn part-2-solver [input]
  (let [cards (vec (parse-input input))]
    (->> (iterate (fn [{:keys [pos cards tot-cards] :as state}]
                    (let [{:keys [winning numbers copies]} (get cards pos)
                          score (count (filter winning numbers))
                          bumped-cards (reduce (fn [cards bump-pos]
                                                 (update-in cards [bump-pos :copies] + copies))
                                               cards
                                               (range (inc pos)
                                                      (min tot-cards (+ (inc pos) score))))]
                      (-> state
                          (assoc :cards bumped-cards)
                          (update :pos inc))))
                  {:cards cards
                   :pos 0
                   :tot-cards (count cards)})
         (drop-while (fn [{:keys [tot-cards pos]}]
                       (< pos tot-cards)))
         (first)
         (:cards)
         (map :copies)
         (reduce +))))
