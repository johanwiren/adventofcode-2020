(ns adventofcode-2021.day-16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]))

(def input (->> "2021/day_16.txt"
                (io/resource)
                (io/reader)
                (slurp)
                (str/trim-newline)))

(defn parse-binary [s]
  (Integer/parseInt s 2))

(def digits
  {\0 "0000"
   \1 "0001"
   \2 "0010"
   \3 "0011"
   \4 "0100"
   \5 "0101"
   \6 "0110"
   \7 "0111"
   \8 "1000"
   \9 "1001"
   \A "1010"
   \B "1011"
   \C "1100"
   \D "1101"
   \E "1110"
   \F "1111"})

(defn parse-input [input]
  (apply str (mapcat digits input)))

(defn decode-header [packet]
  (let [[_ v t data] (re-matches #"^(\d{3})(\d{3})(.*)" packet)]
    (when (and v t (<= 5 (count data)))
      [{:v (parse-binary v) :t (parse-binary t)} data])))

(defn decode-literal [data]
  (let [[nibs last-nib _] (rest (re-matches #"((?:1\d{4})*)0(\d{4})(.*)" data))
        nibbles (conj (->> nibs
                           (partition 5)
                           (mapv (comp (partial apply str) rest)))
                      last-nib)
        n (Long/parseLong (apply str nibbles) 2)
        to-drop (* 5 (count nibbles))]
    [n (subs data to-drop)]))

(declare decode)

(defn decode-op-0 [data header]
  (let [len (parse-binary (subs data 1 16))
        packets (subs data 16 (+ len 16))]
    (into [[header (decode packets)]]
          (when (< (+ 16 len) (count data))
            (let [more (subs data (+ 16 len))]
              (decode more))))))

(defn decode-op-1
  [data header]
  (let [n (parse-binary (subs data 1 12))
        [children siblings] (split-at n (decode (subs data 12)))]
    (into [[header (vec children)]]
          siblings)))

(defn decode [packet]
  (when-let [[{:keys [t] :as header} data] (decode-header packet)]
    (case t
      4 (let [[val data] (decode-literal data)
              ret (assoc header :val val)]
          (if data
            (into [ret] (decode data))
            [ret]))
      (case (first data)
        \0 (decode-op-0 data header)
        \1 (decode-op-1 data header)
        (throw (Exception. (str {:packet packet
                                 :header header})))))))

(defn eq [x y]
  (if (= x y) 1 0))

(defn gt [x y]
  (if (> x y) 1 0))

(defn lt [x y]
  (if (< x y) 1 0))

(defprotocol Clojure
  (to-clj [x]))

(extend-protocol Clojure
  clojure.lang.IPersistentMap
  (to-clj [x]
    (case (:t x)
      0 '+
      1 '*
      2 'min
      3 'max
      4 (:val x)
      5 'gt
      6 'lt
      7 'eq))

  clojure.lang.IPersistentCollection
  (to-clj [x]
    (cond
      (and (map? (first x)) (symbol? (to-clj (first x))))
      (let [args (first (mapv to-clj (rest x)))]
        (list 'apply (to-clj (first x)) args))

      (seqable? x)
      (mapv to-clj x)

      :else
      x)))

(defn part-1-solver [input]
  (->> (parse-input input)
       decode
       flatten
       (keep :v)
       (reduce +)))

(defn part-2-solver [input]
  (->> (parse-input input)
       decode
       to-clj
       eval
       first))

(t/deftest part-1-test
  (t/is (= 1002 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 1673210814091 (part-2-solver input))))
