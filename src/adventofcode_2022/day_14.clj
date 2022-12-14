(ns adventofcode-2022.day-14
  (:require [adventofcode-2022.utils :as u]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn render-line [[head next & more]]
  (let [[[from-x from-y] [to-x to-y]] (sort-by (juxt first second) [head next])
        points (for [x (range from-x (inc to-x))
                     y (range from-y (inc to-y))]
                 [x y])]
    (if-not (seq more)
      points
      (into (set points) (render-line (cons next more))))))

(defn xs [line]
  (->> (re-seq #"\d+" line)
       (map parse-long)
       (partition 2)
       (render-line)))

(defn bbox [points]
  (let [[start-x start-y] (first points)]
    (->> points
         (reduce (fn [[min-x min-y max-x max-y] [x y]]
                   [(min min-x x)
                    (min min-y y)
                    (max max-x x)
                    (max max-y y)])
                 [start-x start-y start-x start-y])
         (partition 2)
         (mapv vec))))

(defn in-bbox? [[[min-x min-y] [max-x max-y]] [x y]]
  (and (<= min-x x max-x)
       (<= min-y y max-y)))

(defn do-drip [bbox [x y :as point] points]
  (when (and (in-bbox? bbox point)
             (not (points point)))
    (let [[dl b dr] (map points [[(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]])]
      (cond
        (not b)
        (do-drip bbox [x (inc y)] points)

        (and b (not dl))
        (do-drip bbox [(dec x) (inc y)] points)

        (and b (not dr))
        (do-drip bbox [(inc x) (inc y)] points)

        (and dl b dr)
        point))))

(defn drip [bbox points]
  (let [new-point (do-drip bbox [500 0] points)]
    (cond-> points
      new-point (conj new-point))))

(defn drip-all [bbox points]
  (->> points
       (iterate (partial drip bbox))
       (partition 2 1)
       (drop-while (partial apply not=))
       (second)
       (first)))

(defn parse-input [input]
  (->> input
       (into #{} (mapcat xs))))

(defn solve [bbox points]
  (- (count (drip-all bbox points))
     (count points)))

(defn part-1-solver [input]
  (let [walls (parse-input input)
        bbox (assoc-in (bbox walls) [0 1] 0)]
    (solve bbox walls)))

(defn part-2-solver [input]
  (let [walls (parse-input input)
        max-y (+ 2 (apply max (map second walls)))
        floor (->> (range (- 500 max-y) (inc (+ 500 max-y)))
                   (map #(vector % max-y)))
        with-floor (into walls floor)
        bbox (assoc-in (bbox with-floor) [0 1] 0)]
    (solve bbox with-floor)))

(t/deftest part-1-test
  (t/is (= 696 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 23610 (time (part-2-solver input)))))
