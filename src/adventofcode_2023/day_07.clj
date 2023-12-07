(ns adventofcode-2023.day-07
  (:require [clojure.string :as str]
            [utils :as u]))

(def input (u/line-seq-input *ns*))

(def card-score (into {}
                      (->> [:2 :3 :4 :5 :6 :7 :8 :9 :T :J :Q :K :A]
                           (map-indexed (fn [i k] [k (+ 2 i)])))))

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
  (into {}
        (->> [:J :2 :3 :4 :5 :6 :7 :8 :9 :T :Q :K :A]
             (map-indexed (fn [i k] [k (+ 2 i)])))))

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
