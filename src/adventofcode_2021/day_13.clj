(ns adventofcode-2021.day-13
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))

(def input (->> "2021/day_13.txt"
                (io/resource)
                (io/reader)
                (line-seq)))

(defn- parse-points [points]
  (->> points
       (map (partial re-seq #"\d+"))
       (map (partial map #(Integer/parseInt %)))
       (set)))

(defn- parse-folds [folds]
  (map (fn [fold]
         (let [[dir x] (->> fold
                            (re-matches #".* (.)=(\d+)")
                            rest)]
           [dir (Integer/parseInt x)]))
       folds))

(defn parse-input [input]
  (let [[points _ folds] (->> input
                              (partition-by #{""}))]
    {:points (parse-points points)
     :folds (parse-folds folds)}))

(defn fold-point [dir n [x y :as point]]
  (case dir
    "y" (if (<= n y)
          [x (- n (- y n))]
          point)
    "x" (if (<= n x)
          [(- n (- x n)) y]
          point)))

(defn folder [dir n points]
  (into #{}
        (map (partial fold-point dir n))
        points))

(defn fold [{:keys [folds] :as state}]
  (let [[dir n] (first folds)]
    (-> state
        (update :folds next)
        (update :points (partial folder dir n)))))

(defn part-1-solver [input]
  (->> (parse-input input)
       (fold)
       (:points)
       (count)))

(defn transpose [xs]
  (apply (partial map vector) xs))

(defn to-strs [points]
  (let [[xs ys] (transpose points)
        max-x (apply max xs)
        max-y (apply max ys)
        matrix (mapv (constantly (apply (partial vector-of :char)
                                        (repeat (inc max-x) \.)))
                     (range (inc max-y)))]
    (->> points
         (reduce (fn [matrix point]
                   (assoc-in matrix (reverse point) \#))
                 matrix)
         (map (partial apply str)))))

(defn part-2-solver [input]
  (->> (parse-input input)
       (iterate fold)
       (drop-while :folds)
       (first)
       (:points)
       (to-strs)))

(t/deftest part-1
  (t/is (= 770 (part-1-solver input))))

(t/deftest part-2
  (t/is (= ["####.###..#..#.####.#....###..###..###."
            "#....#..#.#..#.#....#....#..#.#..#.#..#"
            "###..#..#.#..#.###..#....#..#.###..#..#"
            "#....###..#..#.#....#....###..#..#.###."
            "#....#....#..#.#....#....#....#..#.#.#."
            "####.#.....##..####.####.#....###..#..#"]
           (time (part-2-solver input)))))
