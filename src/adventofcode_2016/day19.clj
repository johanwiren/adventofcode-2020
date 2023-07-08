(ns adventofcode-2016.day19
  (:require [clojure.math :as math]))

(def input 3012210)

(defn powers-of-2 []
  (->> (range)
       (map (fn [i] (math/pow 2 i)))))

(defn nearest-power-of-2 [n]
  (->> (powers-of-2)
       (take-while (fn [i] (< i n)))
       (last)))

(defn part-1-solver [input]
  (let [nearest (nearest-power-of-2 input)]
    (inc (* (- input nearest) 2))))

(defn vq [n xs]
  (let [[left right] (split-at (math/round (/ n 2)) xs)]
    [(vec left) (into clojure.lang.PersistentQueue/EMPTY right)]))

(defn winner-p2-q [n-elves]
  (->> (range 1 (inc n-elves))
       (vq n-elves)
       (iterate (fn [[left right]]
                  (if (even? (+ (count left) (count right)))
                    [(cond-> (subvec left 1)
                       (-> right pop peek)
                       (conj (-> right pop peek)))
                     (-> right pop pop (conj (first left)))]
                    [(cond-> (subvec left 1 (dec (count left)))
                       (peek right)
                       (conj (peek right)))
                     (-> right pop (conj (first left)))])))
       (take-while (partial not-any? empty?))
       (last)
       (ffirst)))

(defn nearest-power-of-3 [n]
  (->> (range)
       (map (comp int (partial math/pow 3)))
       (take-while (fn [i] (< i n)))
       (last)))

(defn part-2-math [n]
  (let [po3 (nearest-power-of-3 n)]
    (cond
      (= n (* 3 po3))
      n

      (<= (* po3 2) n)
      (+ po3 (* (mod n po3) 2))

      :else
      (mod n po3))))

(defn part-2-solver [input]
  (part-2-math input))
