(ns adventofcode-2021.day-22
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :as t]))

(def input (->> "2021/day_22.txt"
                    (io/resource)
                    (io/reader)
                    (line-seq)))

(defn parse-line [line]
  (let [[toggle & coords] (rest (re-matches #"(.*) x=(.*)\.\.(.*),y=(.*)\.\.(.*),z=(.*)\.\.(.*)" line))]
    (-> (zipmap [:x :y :z]
                (->> coords
                     (map #(Integer/parseInt %))
                     (partition 2)))
        (assoc :toggle (keyword toggle)))))

(defn parse-input [input]
  (map parse-line input))

(defn init-action? [step]
  (every? (fn [[a b]] (and (<= -50 a 50)
                           (<= -50 b 50)))
          (vals (select-keys step [:x :y :z]))))

(defn inclusive-range [n1 n2]
  (range n1 (inc n2)))

(defn cube-points [xs ys zs]
  (for [x (apply inclusive-range xs)
        y (apply inclusive-range ys)
        z (apply inclusive-range zs)]
    [x y z]))

(defn boot [procedure]
  (reduce (fn [acc {:keys [toggle x y z]}]
            (case toggle
              :on (into acc (cube-points x y z))
              :off (set/difference acc (set (cube-points x y z)))))
          #{}
          procedure))

(defn part-1-solver [input]
  (->> (parse-input input)
       (filter init-action?)
       (boot)
       (count)))

(defn intersection [cube1 cube2]
  (reduce (fn [intersect [[c1min c1max] [c2min c2max]]]
            (let [mx (max c1min c2min)
                  mn (min c1max c2max)]
              (if (<= mx mn)
                (conj intersect [mx mn])
                (reduced nil))))
          []
          (map vector cube1 cube2)))

(defn volume [cube]
  (->> cube
       (map (fn [[n1 n2]]
              (- (inc n2) n1)))
       (apply *)))

(defn boot-2 [procedure]
  (reduce (fn [acc {:keys [toggle x y z]}]
            (cond-> (into acc (keep (fn [[onoff cube]]
                                      (when-let [intersection (intersection [x y z] cube)]
                                        [(* -1 onoff) intersection]))
                                    acc))
              (= :on toggle) (conj [1 [x y z]])))
          []
          procedure))

(defn part-2-solver [input]
  (->> (parse-input input)
       (boot-2)
       (map (fn [[mul cube]]
              (* mul (volume cube))))
       (reduce +)))

(t/deftest part-1
  (t/is (= 568000 (time (part-1-solver input)))))

(t/deftest part-2
  (t/is (= 1177411289280259 (time (part-2-solver input)))))
