(ns adventofcode-2015.day05
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(def input (line-seq (io/reader (io/resource "2015/day-05.txt"))))

(defn three-vowels [s]
  (->> s
       (filter (set "aeiou"))
       (count)
       (<= 3)))

(defn one-duplicate [s]
  (re-find #"(\w)\1" s))

(defn not-these [s]
  (not (boolean (re-seq #"(ab|cd|pq|xy)" s))))

(def part-1-valid? (every-pred three-vowels one-duplicate not-these))

(defn part-1-solver [input]
  (->> input
       (filter part-1-valid?)
       (count)))

(defn two-pairs [s]
  (re-matches #".*(..).*\1.*" s))

(defn efe [s]
  (re-matches #".*(.).\1.*" s))

(def part-2-valid? (every-pred two-pairs efe))

(defn part-2-solver [input]
  (->> input
       (filter part-2-valid?)
       (count)))

(comment
  (part-2-solver input)

  (not )

  (two-pairs "apaab")

  (efe "xfe")

  (boolean [])

  (not-these "asdf")

  (#{:a :b} :a)

  (three-vowels "apaa")

  (one-duplicate "apaa")

  (one-duplicate "apa")

  (set/intersection (set "apa") (set (seq "apabpacpa"))))

