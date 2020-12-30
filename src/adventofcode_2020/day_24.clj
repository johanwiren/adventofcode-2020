(ns adventofcode-2020.day-24
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input tt]]
            [clojure.string :as str]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn move [[x y] dir]
  (case dir
    \→  [(+ x 2) y]
    \↘  [(inc x) (dec y)]
    \↙  [(dec x) (dec y)]
    \←  [(- x 2) y]
    \↖  [(dec x) (inc y)]
    \↗  [(inc x) (inc y)]))

(defn walk [ref path]
  (reduce move ref path))

(defn to-single-dir [path]
  (-> path
      (str/replace "se" "↘")
      (str/replace "sw" "↙")
      (str/replace "ne" "↗")
      (str/replace "nw" "↖")
      (str/replace "e" "→")
      (str/replace "w" "←")))

(defn parse-input [in]
  (map to-single-dir in))

(defn neighbours [[x y]]
  [[(inc x) (inc y)]
   [(+ x 2) y]
   [(inc x) (dec y)]
   [(dec x) (dec y)]
   [(- x 2) y]
   [(dec x) (inc y)]])

(defn part-1-solver [in]
  (->> (parse-input in)
       (map (partial reduce move [0 0]))
       (frequencies)
       (filter (comp odd? second))
       (count)))

(defn generation [tiles]
  (->> (mapcat neighbours tiles)
       (set)
       (reduce (fn [acc pos]
                 (let [alive? (tiles pos)
                       neighs (neighbours pos)
                       live-neighbours (->> neighs
                                            (filter tiles)
                                            count)]
                   (if (or (and alive?
                                (pos? live-neighbours)
                                (<= live-neighbours 2))
                           (= 2 live-neighbours))
                     (cons pos acc)
                     acc)))
               (list))
       (set)))

(defn part-2-solver [in]
  (->> (parse-input in)
       (map (partial reduce move [0 0]))
       (frequencies)
       (filter (comp odd? second))
       (map first)
       (into #{})
       (iterate generation)
       (drop 100)
       (first)
       count))

(t/deftest part-1-test
  (t/is (= 10 (part-1-solver reference-input)))
  (t/is (= 386 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 2208 (time (part-2-solver reference-input))))
  (t/is (= 4214 (time (part-2-solver input)))))
