(ns adventofcode-2015.day12
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]))

(def input (json/read-str (slurp (io/resource "2015/day-12.txt"))))


(defn walk [coll-or-val]
  (cond
    (sequential? coll-or-val) (map walk coll-or-val)
    (map? coll-or-val) (map walk (vals coll-or-val))
    :else coll-or-val))

(defn solver [walker input]
  (->> input
       (walker)
       flatten
       (filter number?)
       (reduce +)))

(defn part-1-solver [input]
  (solver walk input))

(defn walk-2 [coll-or-val]
  (cond
    (sequential? coll-or-val) (map walk-2 coll-or-val)
    (map? coll-or-val) (when-not (some #{"red"} (vals coll-or-val))
                         (map walk-2 (vals coll-or-val)))
    :else coll-or-val))

(defn part-2-solver [input]
  (solver walk-2 input))

(comment

  (part-1-solver input)

  (part-2-solver input)

  (flatten (walk input)))
