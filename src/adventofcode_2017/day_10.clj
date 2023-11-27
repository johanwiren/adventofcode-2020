(ns adventofcode-2017.day-10
  (:require [clojure.string :as str]
            [utils :as u]))

(def input (first (u/line-seq-input *ns*)))

(defn knot-hash [lengths rounds]
  (->> lengths
       (repeat rounds)
       (flatten)
       (reduce (fn [[lst pos skip] len]
                 (let [src (map #(mod % (count lst))
                                 (range pos (+ pos len)))
                       dst (reverse src)
                       lst (->> (map vector src dst)
                                (reduce (fn [lst' [src dst]]
                                          (assoc lst' dst (get lst src)))
                                        lst))]
                   [lst (mod (+ pos len skip) (count lst)) (inc skip)]))
               [(vec (range 256)) 0 0])
       (first)))

(defn part-1-solver [input]
  (let [lengths (->> (str/split input #",")
                     (mapv parse-long))]
    (->> (knot-hash lengths 1)
         (take 2)
         (apply *))))

(defn part-2-solver [input]
  (let [lengths (into (mapv int input) [17 31 73 47 23])]
    (->> (knot-hash lengths 64)
         (partition 16)
         (map (partial apply bit-xor))
         (map (partial format "%02x"))
         (apply str))))
