(ns adventofcode-2024.day-04
  (:require
   [clojure.string :as str]
   [clojure.test :as t]
   [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn diagonal [input direction]
  (let [padding [#(apply str (repeat % " "))
                 #(apply str (repeat (- (count input) (inc %)) " "))]
        [pad-left pad-right]
        (if (= :right direction)
          padding
          (reverse padding))]
    (->> (for [i (range (count input))]
           (str (pad-left i) (get input i) (pad-right i)))
         (apply map str)
         (map str/trim))))

(defn count-xmas [str]
  (+ (count (re-seq #"XMAS" str))
     (count (re-seq #"SAMX" str))))

(defn part-1-solver [input]
  (let [transposed (apply mapv str input)
        diagonal-r (diagonal input :right)
        diagonal-l (diagonal input :left)]
    (->> (concat input
                 transposed
                 diagonal-r
                 diagonal-l)
         (map count-xmas)
         (reduce +))))

(defn neighbours [[y x]]
  [[(dec y) (dec x)] [(dec y) (inc x)]
   [(inc y) (dec x)] [(inc y) (inc x)]])

(defn part-2-solver [input]
  (->> (for [x (range (count input))
             y (range (count (first input)))
             :when (and
                    (= \A (get-in input [x y]))
                    (#{[\M \M \S \S]
                       [\S \S \M \M]
                       [\M \S \M \S]
                       [\S \M \S \M]}
                     (map #(get-in input %)
                          (neighbours [x y]))))]
         1)
       (apply +)))

(t/deftest part-1-test
  (t/is (= 2397 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 1824 (part-2-solver input))))
