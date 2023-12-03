(ns adventofcode-2023.day-03
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn neighbours [[x y]]
  (for [x' (range (dec x) (+ 2 x))
        y' (range (dec y) (+ 2 y))
        :when (not= [x y] [x' y'])]
    [x' y']))

(defn parse-input [input]
  (let [n-col (count (first input))
        xy (fn [x] [(quot x n-col) (rem x n-col)])]
    (->> input
         (mapcat (partial re-seq #"\d+|."))
         (reduce (fn [[pos xs] x]
                   (let [positions (into #{} (map xy (range pos (+ pos (count x)))))]
                     [(+ pos (count x))
                      (->> positions
                           (map (fn [i]
                                  {:pos i
                                   :val (or (parse-long x) (first x))}))
                           (into xs))]))
                 [0 []])
         (second)
         (remove (comp #{\.} :val)))))

(defn numbers-by-pos [val-maps]
  (into {}
        (comp (filter (comp number? :val))
              (map (juxt :pos identity)))
        val-maps))

(defn solver [input filter-fn vals-fn]
  (let [val-maps (parse-input input)
        by-pos (numbers-by-pos val-maps)]
    (->> (filter filter-fn val-maps)
         (map :pos)
         (reduce (fn [acc pos]
                   (let [neighs (->> (neighbours pos)
                                     (keep by-pos))
                         vals (->> (map :val neighs)
                                   (into #{}))]
                     (+ acc (vals-fn vals))))
                 0))))

(defn part-1-solver [input]
  (solver input
          (comp (complement number?) :val)
          (fn [vals] (reduce + vals))))

(defn part-2-solver [input]
  (solver input
          (comp #{\*} :val)
          (fn [vals]
            (if (= 2 (count vals))
              (reduce * vals)
              0))))
