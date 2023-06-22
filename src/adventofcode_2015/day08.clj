(ns adventofcode-2015.day08
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def input (line-seq (io/reader (io/resource "2015/day-08.txt"))))

(defn read-string [s]
  (loop [res ""
         s (seq s)]
    (if-not (seq s)
      res
      (let [[new more] (cond
                         (re-matches #"^\\[^x].*" (apply str s)) ["." (drop 2 s)]
                         (re-matches #"^\\x[0-9a-f]{2}.*" (apply str s)) ["." (drop 4 s)]
                         :else [(first s) (rest s)])]
        (recur (str res new) more)))))

(defn parse-input [input]
  (map (comp (partial apply str)
             rest
             butlast)
       input))

(defn encode-str [s]
  (loop [res ""
         [s & more] s]
    (if-not s
      res
      (let [new (case s
                  \\ "\\\\"
                  \" "\\\""
                  s)]
        (recur (str res new) more)))))

(defn part-1-solver [input]
  (->> input
       (map (juxt identity (comp read-string
                                 (partial apply str)
                                 butlast
                                 rest)))
       (map (partial map count))
       (map (partial apply -))
       (reduce +)))

(defn part-2-solver [input]
  (->> input
       (map (juxt encode-str
                  (comp (partial apply str)
                        butlast
                        rest)))
       (map (partial map count))
       (map (partial apply -))
       (reduce +)))

(comment
  (part-1-solver input)

  (part-2-solver input)

  (map (juxt identity read-string) (parse-input input))

  (String. (.getBytes "\\x27" "ISO-8859-1"))

  (parse-input input)

  \u0027)
