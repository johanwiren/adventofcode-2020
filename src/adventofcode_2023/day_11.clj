(ns adventofcode-2023.day-11
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-input [input]
  (let [n-col (count (first input))
        point (fn [i] [(quot i n-col) (rem i n-col)])]
    (->> input
         (mapcat (partial re-seq #"."))
         (map-indexed (fn [i x] [i x]))
         (filter (comp #{"#"} second))
         (map (comp point first))
         (into #{}))))

(defn bbox [points]
  (reduce (fn [[mn mx] point]
            [(mapv min mn point)
             (mapv max mx point)])
          [(first points) (first points)]
          points))

(defn growth [points]
  (let [[[min-x min-y] [max-x max-y]] (bbox points)
        xs (into #{} (map first points))
        ys (into #{} (map second points))
        grow-xs (remove xs (range min-x (inc max-x)))
        grow-ys (remove ys (range min-y (inc max-y)))]
    {:xs grow-xs
     :ys grow-ys}))

(defn distance [{:keys [xs ys factor]} [ax ay :as a] [bx by :as b]]
  (let [x-growth (->> xs
                      (filter (fn [x] (< (min ax bx) x (max ax bx))))
                      (count)
                      (* factor))
        y-growth (->> ys
                      (filter (fn [y] (< (min ay by) y (max ay by))))
                      (count)
                      (* factor))]
    (->> (mapv (comp abs -) a b)
         (reduce +)
         (abs)
         (+ x-growth y-growth))))

(defn solver [input growth-factor]
  (let [points (parse-input input)
        growth (growth points)
        pairs (u/pairs points)]
    (->> pairs
         (map (fn [[a b]] (distance (assoc growth :factor growth-factor) a b)))
         (reduce +))))

(defn part-1-solver [input]
  (solver input 1))

(defn part-2-solver [input]
  (solver input (dec 1000000)))
