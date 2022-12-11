(ns adventofcode-2022.day-10
  (:require [adventofcode-2022.utils :as u]
            [clojure.string :as str]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (let [[op val] (str/split line #" ")]
    (cond-> [(keyword op)]
      val (conj (parse-long val)))))

(defn parse-input [input]
  (map parse-line input))

(defn xs [instrs]
  (->> instrs
       (reductions (fn [acc [instr val]]
                     (case instr
                       :noop [(last acc)]
                       :addx [(last acc) (+ (last acc) val)]))
                   [1])
       (flatten)))

(defn part-1-solver [input]
  (let [cycles (take 6 (iterate (partial + 40) 20))
        xs     (xs (parse-input input))]
    (->> cycles
         (map (fn [cycle]
                (* cycle (nth xs (dec cycle)))))
         (reduce +))))

(defn part-2-solver [input]
  (->> (parse-input input)
       (xs)
       (partition 40)
       (map (partial map-indexed (fn [i x] (if (<= (dec i) x (inc i)) "🟠" "⚫️"))))
       (map (partial apply str))))

(t/deftest part-1-test
  (t/is (= 14760 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= ["🟠🟠🟠🟠⚫️🟠🟠🟠🟠⚫️⚫️🟠🟠⚫️⚫️🟠🟠🟠🟠⚫️🟠🟠🟠⚫️⚫️🟠⚫️⚫️🟠⚫️🟠🟠🟠⚫️⚫️🟠🟠🟠🟠⚫️"
            "🟠⚫️⚫️⚫️⚫️🟠⚫️⚫️⚫️⚫️🟠⚫️⚫️🟠⚫️🟠⚫️⚫️⚫️⚫️🟠⚫️⚫️🟠⚫️🟠⚫️⚫️🟠⚫️🟠⚫️⚫️🟠⚫️🟠⚫️⚫️⚫️⚫️"
            "🟠🟠🟠⚫️⚫️🟠🟠🟠⚫️⚫️🟠⚫️⚫️⚫️⚫️🟠🟠🟠⚫️⚫️🟠⚫️⚫️🟠⚫️🟠⚫️⚫️🟠⚫️🟠⚫️⚫️🟠⚫️🟠🟠🟠⚫️⚫️"
            "🟠⚫️⚫️⚫️⚫️🟠⚫️⚫️⚫️⚫️🟠⚫️🟠🟠⚫️🟠⚫️⚫️⚫️⚫️🟠🟠🟠⚫️⚫️🟠⚫️⚫️🟠⚫️🟠🟠🟠⚫️⚫️🟠⚫️⚫️⚫️⚫️"
            "🟠⚫️⚫️⚫️⚫️🟠⚫️⚫️⚫️⚫️🟠⚫️⚫️🟠⚫️🟠⚫️⚫️⚫️⚫️🟠⚫️🟠⚫️⚫️🟠⚫️⚫️🟠⚫️🟠⚫️🟠⚫️⚫️🟠⚫️⚫️⚫️⚫️"
            "🟠🟠🟠🟠⚫️🟠⚫️⚫️⚫️⚫️⚫️🟠🟠🟠⚫️🟠🟠🟠🟠⚫️🟠⚫️⚫️🟠⚫️⚫️🟠🟠⚫️⚫️🟠⚫️⚫️🟠⚫️🟠🟠🟠🟠⚫️"]
           (time (part-2-solver input)))))
