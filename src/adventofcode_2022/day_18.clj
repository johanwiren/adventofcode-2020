(ns adventofcode-2022.day-18
  (:require [adventofcode-2022.utils :as u]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn neighbours [cube]
  (map (partial mapv + cube)
       [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]))

(defn bbox [points]
  (reduce (fn [[mn mx] point]
            [(mapv min mn point)
             (mapv max mx point)])
          [(first points) (first points)]
          points))

(defn cube [[[min-x min-y min-z] [max-x max-y max-z]]]
  (for [x (range min-x (inc max-x))
        y (range min-y (inc max-y))
        z (range min-z (inc max-z))]
    [x y z]))

(defn dfs [root neigh-fn]
  (loop [q (into [] [root])
         seen (transient (set [root]))]
    (let [v (peek q)]
      (if (nil? v)
        (persistent! seen)
        (let [neighs (remove seen (neigh-fn v))]
          (recur (into (pop q) neighs)
                 (reduce (fn [seen neigh]
                           (conj! seen neigh))
                         seen
                         neighs)))))))

(defn in-bbox? [[[min-x min-y min-z] [max-x max-y max-z]] [x y z]]
  (and (<= min-x x max-x)
       (<= min-y y max-y)
       (<= min-z z max-z)))

(defn parse-line [line]
  (mapv parse-long (re-seq #"\d+" line)))

(defn parse-input [input]
  (map parse-line input))

(defn solve [cubes]
  (let [cubes (set cubes)
        sides (* 6 (count cubes))]
    (reduce (fn [sides cube]
              (- sides (->> (neighbours cube)
                            (filter cubes)
                            (count))))
            sides
            cubes)))

(defn part-1-solver [input]
  (solve (parse-input input)))

(defn part-2-solver [input]
  (let [cubes (set (parse-input input))
        bbox (bbox cubes)
        outside (dfs (first bbox) (fn [cube]
                                    (->> (neighbours cube)
                                         (filter (partial in-bbox? bbox))
                                         (remove cubes))))
        droplet (->> (cube bbox)
                     (remove outside))]
    (solve droplet)))

(t/deftest part-1-test
  (t/is (= 4474 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 2518 (time (part-2-solver input)))))
