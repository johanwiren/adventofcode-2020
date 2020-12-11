(ns adventofcode-2020.day-11
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [clojure.test :as t])
  (:refer-clojure :exclude [empty?]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn adjacent-to [max-row max-col]
  (fn [[seat-row seat-col]]
    (->> (for [row (range (dec seat-row)
                          (+ 2 seat-row))
               col (range (dec seat-col)
                          (+ 2 seat-col))]
           [row col])
         (remove (some-fn (partial some neg?)
                          (comp (partial < max-row) first)
                          (comp (partial < max-col) second)
                          #{[seat-row seat-col]})))))

(defn empty? [seat]
  (= \L seat))

(defn occupied? [seat]
  (= \# seat))

(defn get-seat [seating [row col]]
  (-> (nth seating row)
      (nth col)))

(defn occupy? [seat my-neighbours]
  (and (empty? seat)
       (not-any? occupied? my-neighbours)))

(defn vacate? [seat my-neighbours]
  (and (occupied? seat)
       (->> (filter occupied? my-neighbours)
            (count)
            (< 3))))

(defn reseat [seating neighbours row col]
  (let [seat (get-seat seating [row col])
        my-neighbours (map (partial get-seat seating)
                           (get neighbours [row col]))]
    (cond
      (occupy? seat my-neighbours)
      \#

      (vacate? seat my-neighbours)
      \L

      :else
      seat)))

(defn generation [seating]
  (let [row-size (count seating)
        col-size (count (first seating))
        get-neighbours (adjacent-to (dec row-size) (dec col-size))
        neighbours (into {}
                         (for [row (range row-size)
                               col (range col-size)]
                           [[row col] (get-neighbours [row col])]))]
    (mapv (fn [row]
            (mapv (partial reseat seating neighbours row)
                  (range col-size)))
          (range row-size))))

(defn parse-input [in]
  (mapv (comp vec seq) in))

(defn until-stabilized [gen-fn seating]
  (->> seating
       (iterate gen-fn)
       (partition 2)
       (take-while (fn [[a b]] (not= a b)))
       last
       last))

(defn count-occupied [seating]
  (->> seating
       (mapcat (partial apply str))
       (filter occupied?)
       count))

(defn part-1-solver [in]
  (->> (parse-input in)
       (until-stabilized generation)
       (count-occupied)))

(defn part-2-solver [in]
  in)

(t/deftest part-1-test
  (t/is (= 37 (part-1-solver reference-input)))
  (t/is (= 2324 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= :FIXME (part-2-solver reference-input))))
