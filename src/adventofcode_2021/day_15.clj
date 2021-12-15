(ns adventofcode-2021.day-15
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))

(def input (->> "2021/day_15.txt"
                (io/resource)
                (io/reader)
                (line-seq)))

(def ref-input (->> "2021/day_15_ref.txt"
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

(defn dijkstra [vs source target neighbours-fn len-fn]
  (loop [dist (transient (assoc (zipmap vs (repeat Double/POSITIVE_INFINITY)) source 0))
         prev (transient {})
         u source
         Q (into #{} vs)]
    (if (or (empty? Q) (= u target) (= Double/POSITIVE_INFINITY (get dist u)))
      {:dist (persistent! dist)
       :prev (persistent! prev)}
      (let [[dist prev]
            (reduce (fn [[dist prev :as acc] v]
                      (let [alt (+ (get dist u) (len-fn u v))]
                        (if (< alt (get dist v))
                          [(assoc! dist v alt)
                           (assoc! prev v u)]
                          acc)))
                    [dist prev]
                    (filter Q (neighbours-fn u)))]
        (recur dist prev (apply min-key dist Q) (disj Q u))))))

(defn part-1-solver [input]
  (let [es (parse-input input)
        square-size (dec (count (first es)))
        goal [square-size square-size]
        vs (for [x (range (inc square-size))
                 y (range (inc square-size))]
             [x y])
        {:keys [prev]} (dijkstra vs [0 0] goal neighbours (fn [_u v] (get-in es v)))]
    (->> (iterate prev goal)
         (take-while identity)
         (map (partial get-in es))
         (butlast)
         (reduce +))) )

(t/deftest part-1
  (t/is (= 592 (time (part-1-solver input)))))
