(ns adventofcode-2017.day-06
  (:require [clojure.string :as str]
            [utils :as u]))

(def input (first (u/line-seq-input *ns*)))

(defn parse-input [input]
  (->> #"\t"
       (str/split input)
       (mapv parse-long)))

(defn most-blocks [banks]
  (->> (rseq banks)
       (map-indexed (fn [i x]
                      [(- (dec (count banks)) i) x]))
       (apply max-key second)
       (first)))

(defn balance [banks]
  (let [busy-bank (most-blocks banks)
        n-banks (count banks)
        blocks (get banks busy-bank)]
    (loop [banks (assoc banks busy-bank 0)
           blocks blocks
           bank (inc busy-bank)]
      (if (zero? blocks)
        banks
        (recur (update banks (mod bank n-banks) inc)
               (dec blocks)
               (inc bank))))))

(defn part-1-solver [input]
  (->> (parse-input input)
       (iterate balance)
       (reduce (fn [seen x]
                 (if (seen x)
                   (reduced (count seen))
                   (conj seen x)))
               #{})))

(defn part-2-solver [input]
  (->> (parse-input input)
       (iterate balance)
       (reduce (fn [seen x]
                 (if (seen x)
                   (reduced x)
                   (conj seen x)))
               #{})
       (iterate balance)
       (reduce (fn [seen x]
                 (if (seen x)
                   (reduced (count seen))
                   (conj seen x)))
               #{})))

