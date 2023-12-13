(ns adventofcode-2023.day-13
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-input [input]
  (->> input
       (partition-by #{""})
       (remove #{[""]})
       (map (partial mapv vec))))

(defn transpose [coll]
  (apply mapv vector coll))

(defn mirror-point [mat]
  (let [splits (->> (partition 2 1 mat)
                    (map (partial apply =))
                    (map-indexed vector)
                    (filter second)
                    (map (comp inc first)))]
    (or (some (fn [split]
                (let [u (subvec mat 0 split)
                      d (subvec mat split)]
                  (and (< split (count mat))
                       (every? true? (map = (rseq u) d))
                       split)))
              splits)
        0)))

(defn solve [input mirror-point-fn]
  (->> (parse-input input)
       (map (juxt mirror-point-fn (comp mirror-point-fn transpose)))
       (mapcat (partial map * [100 1]))
       (reduce +)))

(defn part-1-solver [input]
  (solve input mirror-point))

(defn mirror-point-2 [mat]
  (let [splits (range 1 (count mat))]
    (or (some (fn [split]
                (let [u (subvec mat 0 split)
                      d (subvec mat split)]
                  (and (< split (count mat))
                       (= 1 (->> (map = (flatten (rseq u)) (flatten d))
                                 (filter false?)
                                 (count)))
                       split)))
              splits)
        0)))

(defn part-2-solver [input]
  (solve input mirror-point-2))
