(ns adventofcode-2017.day-09
  (:require [clojure.string :as str]
            [utils :as u]))

(def input (first (u/line-seq-input *ns*)))

(defn solver [input]
  (loop [[char & more] input
         nest-level 0
         score 0
         in-garbage? false
         garbage-count 0]
    (if-not char
      {:part-1 score :part-2 garbage-count}
      (let [more (cond-> more
                   (= \! char) rest)
            score (cond-> score
                    (and (not in-garbage?)
                         (= \} char))
                    (+ nest-level))
            nest-level (cond-> nest-level
                         (and (not in-garbage?)
                              (= \} char))
                         dec
                         (and (not in-garbage?)
                              (= \{ char))
                         inc)
            garbage-count (cond-> garbage-count
                            (and in-garbage?
                                 (not (#{\! \>} char))) inc)
            in-garbage? (or (and in-garbage?
                                 (not= \> char))
                            (= \< char))]
        (recur more nest-level score in-garbage? garbage-count)))))
