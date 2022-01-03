(ns adventofcode-2021.day-18
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.zip :as zip]
            [clojure.test :as t]
            [clojure.string :as str]))

(def input (->> "2021/day_18.txt"
                (io/resource)
                (io/reader)
                (line-seq)))

(defn add [sn1 sn2]
  (format "[%s,%s]" sn1 sn2))

(defn explode [sn]
  (let [len (count sn)]
    (reduce (fn [depth n]
              (cond
                (= 5 depth)
                (let [[d1 d2 right] (rest (re-matches #"(\d+),(\d+)](.*)" (subs sn n)))
                      left (subs sn 0 (dec n))
                      [_ left-n] (first (re-seq #".*[^\d](\d+)(?:[^\d]+)$" left))
                      replaced-left (if left-n
                                      (str/replace left #"\d+([^\d]+)$"
                                                   (str (+ (Integer/parseInt left-n)
                                                           (Integer/parseInt d1))
                                                        "$1"))
                                      left)
                      right-n (first (re-seq #"\d+" right))
                      replaced-right (if right-n
                                       (str/replace right
                                                    #"([^\d]+)\d+(.*)"
                                                    (format "$1%s$2"
                                                            (+ (Integer/parseInt right-n)
                                                               (Integer/parseInt d2))))
                                       right)]
                  (reduced (explode (str replaced-left "0" replaced-right))))

                (= len (inc n))
                (reduced sn)

                :else
                (case (get sn n)
                  \[ (inc depth)
                  \] (dec depth)
                  depth)))
            0
            (range len))))

(defn split [sn]
  (if-let [large-n (first (re-seq #"\d\d+" sn))]
    (let [n (/ (Integer/parseInt large-n) 2)
          d1 (int (Math/floor n))
          d2 (int (Math/ceil n))]
      (str/replace-first sn #"\d\d+" (format "[%d,%d]" d1 d2)))
    sn))

(defn reduce-sn [sn]
  (->> sn
       (iterate (comp split explode))
       (partition 2 1)
       (drop-while (partial apply not=))
       (ffirst)))

(defn calc-magnitude [sn-vec]
  (let [[d1 d2] sn-vec]
    (+ (* 3 (if (number? d1) d1 (calc-magnitude d1)))
       (* 2 (if (number? d2) d2 (calc-magnitude d2))))))

(defn magnitude [sn]
  (calc-magnitude (edn/read-string sn)))

(defn pairs [vec]
  (set (for [x (range (count vec))
             y (range (count vec))
             :when (not= x y)]
         #{(vec x) (vec y)})))

(defn part-1-solver [input]
  (->> input
       (reduce (comp reduce-sn add))
       (magnitude)))

(defn part-2-solver [input]
  (->> input
       vec
       pairs
       (map (comp magnitude reduce-sn (partial apply add)))
       (sort)
       (last)))

(t/deftest explode-test
  (doseq [[sn expected]
          [["[[[[[9,8],1],2],3],4]" "[[[[0,9],2],3],4]"]
           ["[7,[6,[5,[4,[3,2]]]]]" "[7,[6,[5,[7,0]]]]"]
           ["[[6,[5,[4,[3,2]]]],1]" "[[6,[5,[7,0]]],3]"]
           ["[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"]]]
    (t/is (= expected (explode sn)))))
