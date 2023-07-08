(ns adventofcode-2016.day15
  (:require [utils :as u]
            [clojure.math :as math]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (let [[disc positions _ position] (map parse-long (re-seq #"\d+" line))]
    {:disc disc
     :positions positions
     :position position}))

(defn parse-input [input]
  (map parse-line input))

(defn congruence [a m]
  [a m])

(defn mod-inv [a b]
  (.modInverse (biginteger a) (biginteger b)))

(defn crt [congruences]
  (let [M (reduce * 1 (map second congruences))]
    (reduce (fn [acc [a m]]
              (let [M' (/ M m)
                    N (mod-inv M' m)]
                (mod (+ acc (* (mod (* a M') M) N)) M)))
            0
            congruences)))

(defn part-1-solver [input]
  (->> (parse-input input)
       (map (fn [{:keys [disc positions position]}]
              (congruence (mod (- positions (+ position disc)) positions)
                          positions)))
       (crt)))

(defn part-2-solver [input]
  (->> (parse-input input)
       (cons {:disc 7 :positions 11 :position 0})
       (map (fn [{:keys [disc positions position]}]
              (congruence (mod (- positions (+ position disc)) positions)
                          positions)))
       (crt)))
