(ns adventofcode-2017.day-04
  (:require [utils :as u]
            [clojure.string :as str]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (vec (str/split line #" ")))

(defn parse-input [input]
  (map parse-line input))

(defn valid? [passphrase]
  (= passphrase (distinct passphrase)))

(defn part-1-solver [input]
  (->> input
       (parse-input)
       (filter valid?)
       (count)))

(defn anagrams? [[s1 s2]]
  (and (= (count s1)
          (count s2))
       (= (set s1)
          (set s2))))

(defn no-anagrams? [passphrase]
  (not-any? anagrams?
            (u/pairs passphrase)))

(defn part-2-solver [input]
  (->> input
       (parse-input)
       (filter (every-pred no-anagrams? valid?))
       (count)))
