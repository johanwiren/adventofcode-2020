(ns adventofcode-2016.day05
  (:import (java.security MessageDigest))
  (:require [clojure.string :as str]))

(def input "uqwqemis")

(defn hex [ba]
  (apply str (map (partial format "%02x") ba)))

(defn md5 [s]
  (let [digest (.digest (MessageDigest/getInstance "MD5") (.getBytes s))]
    (hex digest)))

(defn part-1-solver [input]
  (->> (range)
       (partition 1000)
       (map (fn [part]
              (pmap (fn [i]
                      (md5 (str input i)))
                    part)))
       (flatten)
       (filter #(str/starts-with? % "00000"))
       (take 8)
       (map #(nth % 5))
       (apply str)))

(defn part-2-solver [input]
  (->> (range)
       (partition 1000)
       (map (fn [part]
              (pmap (fn [i]
                      (md5 (str input i)))
                    part)))
       (flatten)
       (filter #(str/starts-with? % "00000"))
       (filter (fn [hash]
                 (#{\0 \1 \2 \3 \4 \5 \6 \7} (nth hash 5))))
       (reduce (fn [password hash]
                 (let [pos (parse-long (str (nth hash 5)))
                       password (cond-> password
                                  (nil? (get password pos)) (assoc pos (nth hash 6)))]
                   (if (every? some? password)
                     (reduced password)
                     password)))
               [nil nil nil nil nil nil nil nil])

       (apply str)))
