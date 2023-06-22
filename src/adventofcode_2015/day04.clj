(ns adventofcode-2015.day04
  (:import (java.security MessageDigest)
           (java.math BigInteger))
  (:require [clojure.string :as str]
            [clojure.test :as t]))

(defn hex [ba]
  (apply str (map (partial format "%02x") ba)))

(defn md5 [s]
  (let [digest (.digest (MessageDigest/getInstance "MD5") (.getBytes s))]
    (hex digest)))



(defn part-1-solver [secret]
  (->> (range)
       (map (partial str secret))
       (map (juxt identity md5))
       (drop-while (comp (complement #(str/starts-with? % "00000")) second))
       (first)))

(defn part-2-solver [secret]
  (->> (range)
       (map (partial str secret))
       (pmap (juxt identity md5))
       (drop-while (comp (complement #(str/starts-with? % "000000")) second))
       (first)))

(t/deftest part-1-test
  (t/is (= ["iwrupvqb346386" "0000045c5e2b3911eb937d9d8c574f09"]
           (part-1-solver "iwrupvqb"))))

(t/deftest part-2-test
  (t/is (= ["iwrupvqb9958218" "00000094434e1914548b3a1af245fb27"]
           (part-2-solver "iwrupvqb"))))
