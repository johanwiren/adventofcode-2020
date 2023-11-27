(ns adventofcode-2017.day-11
  (:require [clojure.string :as str]
            [utils :as u]))

(def input (first (u/line-seq-input *ns*)))

(def dir->move
  {:nw [-0.5  0.5]
   :n  [   0    1]
   :ne [ 0.5  0.5]
   :se [ 0.5 -0.5]
   :s  [ 0     -1]
   :sw [-0.5 -0.5]})

(defn parse-input [input]
  (->> (str/split input #",")
       (map (comp dir->move keyword))))

(defn dist [pos]
  (->> pos
       (map abs)
       (reduce +)))

(defn move [pos move]
  (mapv + pos move))

(defn part-1-solver [input]
  (->> (parse-input input)
       (reduce move [0 0])
       (dist)))

(defn part-2-solver [input]
  (->> (parse-input input)
       (reductions move [0 0])
       (map dist)
       (apply max)))
