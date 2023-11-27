(ns utils
  (:require [clojure.java.io :as io]
            [clojure.math :as math])
  (:import (java.security MessageDigest)))

(defn line-seq-input [ns]
  (some->> (str ns)
           (re-matches #".*-(\d+)\.[^\d]*(\d+)")
           (rest)
           (apply format "%s/day-%s.txt")
           (io/resource)
           (io/reader)
           (line-seq)))

(defn xgcd
  "Extended Euclidean Algorithm. Returns [gcd(a,b) x y] where ax + by = gcd(a,b)."
  [a b]
  (if (= a 0)
    [b 0 1]
    (let [[g x y] (xgcd (mod b a) a)]
      [g (- y (* (math/floor-div b a) x)) x])))

(defn hex [ba]
  (apply str (map (partial format "%02x") ba)))

(defn md5 [s]
  (.digest (MessageDigest/getInstance "MD5") (.getBytes s)))

(def md5-hex (comp hex md5))

(def bfs-queue clojure.lang.PersistentQueue/EMPTY)

(def dfs-queue [])

(defn search [state exit next]
  (loop [state state]
    (if-let [result (exit state)]
      result
      (when-let [next (next state)]
        (recur next)))))

(defn pairs [xs]
  (let [v (vec xs)
        n (count v)]
    (for [i (range n)
          j (range (inc i) n)]
      [(get v i) (get v j)])))
