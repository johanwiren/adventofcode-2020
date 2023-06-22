(ns adventofcode-2015.day06
  (:require [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.math BigInteger)
           (java.util Arrays)))


(def input (line-seq (io/reader (io/resource "2015/day-06.txt"))))

(defn parse-line [line]
  (let [[action from-x from-y to-x to-y] (rest (re-matches #".*(toggle|off|on) ([\d]+),([\d]+) through ([\d]+),([\d]+)" line))]
    [(keyword action) [(Integer/parseInt from-x) (Integer/parseInt from-y)] [(Integer/parseInt to-x) (Integer/parseInt to-y)]]))


(defn parse-input [input]
  (map parse-line input))

(defn doaction [f lights [from-x from-y] [to-x to-y]]
  (reduce (fn [lights pos]
            (update-in lights pos f))
          lights
          (for [x (range from-x (inc to-x))
                y (range from-y (inc to-y))]
            [x y])))

(defn part-1-solver [input]
  (let [lights (reduce (fn [lights [action from to]]
                         (case action
                           :toggle (doaction not lights from to)
                           :on (doaction (constantly true) lights from to)
                           :off (doaction (constantly false) lights from to)))
                       (vec (repeat 1000 (into (vector-of :boolean) (repeat 1000 false))))
                       (parse-input input))]
    (count (filter true? (flatten lights)))))

(defn lower [value]
  (max (dec value) 0))

(defn part-2-solver [input]
  (let [lights (reduce (fn [lights [action from to]]
                         (case action
                           :toggle (doaction (comp inc inc) lights from to)
                           :on (doaction inc lights from to)
                           :off (doaction lower lights from to)))
                       (vec (repeat 1000 (into (vector-of :int) (repeat 1000 0))))
                       (parse-input input))]
    (reduce + (flatten lights))))

(comment

  (lower 0)

  (update-in [1 2 3 [1 2 3]] [3 2] -)

  (part-2-solver input)

  *1

  (part-1-solver input)

  (parse-input input)

  (to-array)
  (into (boolean-array 100) [false true])

  (aset-boolean)
  (aclone)

  (Arrays/fill (boolean-array 1) 0 1 true)

  (type (first (split-at 2 (boolean-array 10))))

  (class (boolean-array 1))

  (aset-boolean)

  (part-1-solver input)

  *1

  (into #{} (map first (parse-input input)))

  (def lights (set (range 0 1000000)))

  (def i (set/intersection lights (set (range 0 500000))))

  (time (def m (into {} (map (fn [i] [i false]) (range 0 1000000)))))

  (time (def t (merge-with not m (hash-map (range 461550 (inc 564900)) (repeat false)))))

  (time (count (set  (range 0 1000000)))))
