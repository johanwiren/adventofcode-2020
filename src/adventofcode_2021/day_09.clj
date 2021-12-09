(ns adventofcode-2021.day-09
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))

(def input (->> "2021/day_09.txt"
                (io/resource)
                (io/reader)
                (line-seq)
                ))

(def ref-input ["2199943210"
                "3987894921"
                "9856789892"
                "8767896789"
                "9899965678"])

(defn parse-line [input]
  (mapv #(Integer/parseInt %) (re-seq #"\d" input)))

(defn parse-input [input]
  (mapv parse-line input))

(defn get-matrix [matrix [x y]]
  (some-> matrix
          (get x nil)
          (get y nil)))

(defn neighbours [[x y]]
  [[(dec x) y]
   [(inc x) y]
   [x (dec y)]
   [x (inc y)]])

(defn get-neighbours [matrix point]
  (->> (neighbours point)
       (map (partial get-matrix matrix))
       (filter identity)))

(defn find-low-points [matrix]
  (for [x (range (count matrix))
        y (range (count (first matrix)))
        :let [n (get-matrix matrix [x y])
              neighs (get-neighbours matrix [x y])]
        :when (every? (partial < n) neighs)]
    [x y]))

(defn part-1-solver [input]
  (let [matrix (parse-input input)]
    (->> matrix
         (find-low-points)
         (map (partial get-matrix matrix))
         (map inc)
         (reduce +))))

(defn basin [matrix point]
  (let [v (get-matrix matrix point)
        neighs (neighbours point)
        drain-points (filter
                      (fn [pos]
                        (when-let [v' (get-matrix matrix pos)]
                          (and (not= 9 v')
                               (< v v'))))
                      neighs)]
    (if-not (seq neighs)
      #{point}
      (conj (set (mapcat (partial basin matrix) drain-points)) point))))

(defn part-2-solver [input]
  (let [matrix (parse-input input)
        low-points (find-low-points matrix)
        basins (map (partial basin matrix) low-points)]
    (->> basins
         (sort-by count)
         (reverse)
         (take 3)
         (map count)
         (reduce *))))

(t/deftest part-1
  (t/is (= 486 (part-1-solver input))))

(t/deftest part-2
  (t/is (= 1059300 (time (part-2-solver input)))))
