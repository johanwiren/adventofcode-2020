(ns adventofcode-2023.day-07
  (:require [clojure.string :as str]
            [utils :as u]))

(def input (u/line-seq-input *ns*))

(def card-score {:A 14 :Q 12 :4 4 :J 11 :T 10 :7 7 :8 8 :9 9 :2 2 :5 5 :3 3 :6 6 :K 13})

(defn parse-line [line]
  (let [[cards bid] (str/split line #" ")]
    {:cards (map (comp keyword str) cards)
     :bid (parse-long bid)}))

(defn parse-input [input]
  (map parse-line input))

(defn hand-score [card-freqs]
  (->> card-freqs
       (.indexOf [[1 1 1 1 1] [2 1 1 1] [2 2 1] [3 1 1] [3 2] [4 1] [5]])
       inc))

(defn add-freqs [{:keys [cards] :as hand}]
  (let [freqs (frequencies cards)]
    (assoc hand
           :freqs freqs
           :freq-vals (sort > (vals freqs)))))

(defn add-scores [{:keys [freq-vals cards] :as hand}]
  (assoc hand
         :hand-score (hand-score freq-vals)
         :card-scores (mapv card-score cards)))

(defn add-ctx [hand]
  (-> hand
      (add-freqs)
      (add-scores)))

(defn solver [input add-context]
  (->> (parse-input input)
       (map add-context)
       (sort-by (juxt :hand-score :card-scores))
       (map-indexed (fn [i x] [(inc i) x]))
       (map (juxt first (comp :bid second)))
       (map (partial apply *))
       (reduce +)))

(defn part-1-solver [input]
  (solver input add-ctx))

(def joker-card-score
  (assoc card-score :J 1))

(defn add-joker-freq-vals [{:keys [cards] :as hand}]
  (let [j-count (count (filter #{:J} cards))
        freq-vals (->> (remove #{:J} cards)
                       (frequencies)
                       (vals)
                       (sort >)
                       (vec))]
    (assoc hand :freq-vals (update freq-vals 0 (fnil + 0) j-count))))

(defn add-joker-card-score [{:keys [cards] :as hand}]
  (assoc hand :card-scores (mapv joker-card-score cards)))

(defn add-ctx-with-jokers [hand]
  (-> hand
      (add-joker-freq-vals)
      (add-scores)
      (add-joker-card-score)))

(defn part-2-solver [input]
  (solver input add-ctx-with-jokers))
