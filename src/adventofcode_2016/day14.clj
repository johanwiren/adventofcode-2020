(ns adventofcode-2016.day14
  (:import (java.security MessageDigest))
  (:require [clojure.string :as str]))

(def input "jlmsuwbz")

(defn hex [ba]
  (apply str (map (partial format "%02x") ba)))

(defn md5 [s]
  (let [digest (.digest (MessageDigest/getInstance "MD5") (.getBytes s))]
    (hex digest)))

(defn stretched-md5 [s]
  (->> (iterate md5 s)
       (drop 2017)
       (first)))

(defn triple [s]
  (ffirst (re-seq #"(.)\1\1" s)))

(defn friples [s]
  (map first (re-seq #"(.)\1\1\1\1" s)))

(defn to-friple [triple]
  (apply str (repeat 5 (first triple))))

(defn solver [input hash-fn]
  (->> (range)
       (map (partial str input))
       (pmap hash-fn)
       (partition 1001 1)
       (map vector (range))
       (filter (comp triple first second))
       (filter (fn [[_ [hash & hashes]]]
                 (some (partial = (to-friple (triple hash)))
                       (mapcat friples hashes))))
       (map first)
       (drop 63)
       (first)))

(defn part-1-solver [input]
  (solver input md5))

(defn part-2-solver [input]
  (solver input stretched-md5))
