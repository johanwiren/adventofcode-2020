(ns adventofcode-2022.day-12
  (:require [utils :as u]
            [clojure.test :as t])
  (:import (clojure.lang PersistentQueue)))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (mapv (fn [char]
          (let [v (int char)]
            (if (> 97 v) (+ 53 v) v)))
        line))

(defn parse-input [input]
  (mapv parse-line input))

(defn find-start-end [input]
  (into {}
        (for [x     (range (count input))
              y     (range (count (first input)))
              :let  [v (get-in input [x y])]
              :when (#{136 122} v)]
          [(keyword (str (char (- v 53)))) [x y]])))

(defn neighbours [[x y]]
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])

(defn bfs [root goal neigh-fn]
  (loop [q         (into PersistentQueue/EMPTY [root])
         came-from (transient {root :start})]
    (let [v (peek q)]
      (if (or (= v goal) (nil? v))
        (persistent! came-from)
        (let [neighs (remove came-from (neigh-fn v))]
          (recur (into (pop q) neighs)
                 (reduce (fn [came-from neigh]
                           (assoc! came-from neigh v))
                         came-from
                         neighs)))))))

(defn do-bfs [input start end]
  (let [es       input
        neigh-fn (fn [v]
                   (filter #(<= (get-in es % Double/POSITIVE_INFINITY)
                                (inc (get-in es v)))
                           (neighbours v)))]
    (bfs start end neigh-fn)))

(defn part-1-solver [input]
  (let [input         (parse-input input)
        {:keys [S E]} (find-start-end input)
        came-from     (do-bfs input S E)]
    (->> E
         (iterate came-from)
         (take-while (complement #{:start}))
         (count)
         (dec))))

(defn part-2-solver [input]
  (let [input         (parse-input input)
        {:keys [S E]} (find-start-end input)
        came-from     (do-bfs input S E)]
    (->> E
         (iterate came-from)
         (take-while (comp (partial not= 97) (partial get-in input)))
         (count))))

(t/deftest part-1-test
  (t/is (= 534 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 525 (time (part-2-solver input)))))
