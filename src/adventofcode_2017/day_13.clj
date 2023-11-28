(ns adventofcode-2017.day-13
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (mapv parse-long (re-seq #"\d+" line)))

(defn parse-input [input]
  (map parse-line input))

(defn part-1-solver [input]
  (->> (parse-input input)
       (filter (comp zero? (fn [[i x]]
                             (mod i (- (* 2 x) 2)))))
       (map (partial apply *))
       (reduce +)))

(defn part-2-solver [input]
  ;; Brute force. Quick "enough". Looks like a CRT problem though..
  (let [input (parse-input input)
        can-pass? (fn [delay]
                    (not-any? (comp zero? (fn [[i x]]
                                            (mod (+ i delay) (- (* 2 x) 2))))
                              input))]
    (->> (range)
         (filter can-pass?)
         (first))))
