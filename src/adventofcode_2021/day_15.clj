(ns adventofcode-2021.day-15
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))

(def input (->> "2021/day_15.txt"
                (io/resource)
                (io/reader)
                (line-seq)))

(defn parse-line [line]
  (mapv #(Integer/parseInt %)
        (re-seq #"\d" line)))

(defn parse-input [input]
  (mapv parse-line input))

(defn neighbours [[x y]]
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])

(defn a-star [vs source target neighbours-fn len-fn h]
  (let [inf Double/POSITIVE_INFINITY]
    (loop [g-score (transient {source 0})
           came-from (transient {})
           u source
           Q (let [q (java.util.PriorityQueue. (count vs) (fn [x y]
                                                            (- (h y) (h x))))]
               (.add q source)
               q)]
      (if (or (= u target) (zero? (.size Q)) (nil? (get g-score u)))
        {:g-score (persistent! g-score)
         :came-from (persistent! came-from)}
        (let [[g-score came-from]
              (reduce (fn [[g-score came-from :as acc] v]
                        (let [alt (+ (get g-score u inf) (len-fn u v))]
                          (if (< alt (get g-score v inf))
                            (do
                              (.add Q v)
                              [(assoc! g-score v alt)
                               (assoc! came-from v u)])
                            acc)))
                      [g-score came-from]
                      (neighbours-fn u))]
          (.remove Q u)
          (recur g-score came-from (.peek Q) Q))))))

(defn least-cost [input]
  (let [es input
        square-size (dec (count (first es)))
        goal [square-size square-size]
        vs (for [x (range (inc square-size))
                 y (range (inc square-size))]
             [x y])
        neigh-fn (fn [v]
                   (filter (fn [[x y]]
                             (and (<= 0 x square-size)
                                  (<= 0 y square-size)))
                           (neighbours v)))
        h (fn [v]
            (apply + (map - goal v)))
        {:keys [came-from]} (a-star vs [0 0] goal neigh-fn (fn [_u v] (get-in es v)) h)]
    (->> (iterate came-from goal)
         (take-while identity)
         (map (partial get-in es))
         (butlast)
         (reduce +))))

(defn part-1-solver [input]
  (least-cost (parse-input input)))

(defn bump-line [line]
  (mapv (fn [x] (case (int x) 9 1 (inc x))) line))

(defn grow-right [lines]
  (mapv (fn [line] (vec (flatten (take 5 (iterate bump-line line))))) lines))

(defn grow-down [lines]
  (vec (apply concat (take 5 (iterate (partial mapv bump-line) lines)))))

(defn part-2-solver [input]
  (least-cost (grow-down (grow-right (parse-input input)))))

(t/deftest part-1-test
  (t/is (= 592 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 2897 (time (part-2-solver input)))))
