(ns adventofcode-2016.day09
  (:require [utils :as u]
            [clojure.string :as str]))

(def input (first (u/line-seq-input *ns*)))

(defn next-token [s]
  (first (re-seq #"\([^)]+\)|." s)))

(defn process-marker [s marker]
  (let [s (subs s (count marker))
        [chars n] (map parse-long (rest (re-matches #"\((\d+)x(\d+)\)" marker)))]
    [(apply str (repeat n (subs s 0 chars))) (subs s chars)]))

(defn uncompress [s]
  (->> s
       (vector "")
       (iterate (fn [[res in]]
                  (let [token (next-token in)]
                    (if (= 1 (count token))
                      [(str res token) (subs in 1)]
                      (let [[res' in] (process-marker in token)]
                        [(str res res') in])))))
       (drop-while (comp not-empty second))
       (ffirst)
       (count)))

(declare uncompress-p2)

(defn process-marker-p2 [s marker]
  (let [s (subs s (count marker))
        [chars n] (map parse-long (rest (re-matches #"\((\d+)x(\d+)\)" marker)))]
    [(* n (uncompress-p2 (subs s 0 chars))) (subs s chars)]))

(defn uncompress-p2 [s]
  (->> s
       (vector 0)
       (iterate (fn [[res in]]
                  (let [token (next-token in)]
                    (if (= 1 (count token))
                      [(inc res) (subs in 1)]
                      (let [[res' in] (process-marker-p2 in token)]
                        [(+ res res') in])))))
       (drop-while (comp not-empty second))
       (ffirst)))

(defn part-1-solver [input]
  (uncompress input))

(defn part-2-solver [input]
  (uncompress-p2 input))
