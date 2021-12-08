(ns adventofcode-2021.day-08
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :as t]))

(def input (->> "2021/day_08.txt"
                (io/resource)
                (io/reader)
                line-seq))

(defn parse-line [line]
  (let [[signals output] (str/split line #"\| ")]
    {:sig (map set (str/split signals #" "))
     :out (str/split output #" ")}))

(defn parse-input [input]
  (map parse-line input))

(defn part-1-solver [input]
  (->> (parse-input input)
       (mapcat :out)
       (filter (comp #{2 3 4 7} count))
       (count)))

(defn sig-lookup [sigs]
  (let [counts (group-by count sigs)
        six-segments (set (get counts 6))
        five-segments (set (get counts 5))
        [one] (get counts 2)
        [four] (get counts 4)
        [seven] (get counts 3)
        [eight] (get counts 7)
        [nine] (filter (partial set/subset? four) six-segments)
        [zero] (filter (partial set/subset? seven) (disj six-segments nine))
        six (first (disj six-segments nine zero))
        [five] (filter (partial set/superset? six) five-segments)
        [three] (filter (partial set/superset? nine) (disj five-segments five))
        two (first (disj five-segments five three))]
    {one 1 two 2 three 3 four 4 five 5 six 6 seven 7 eight 8 nine 9 zero 0}))

(defn decode [{:keys [sig out]}]
  (let [lookup (sig-lookup sig)]
    (->> out
         (map (comp lookup set))
         (apply str)
         (Integer/parseInt))))

(defn part-2-solver [input]
  (->> input
       parse-input
       (map decode)
       (reduce +)))

(t/deftest part-1
  (t/is (= 383 (part-1-solver input))))

(t/deftest part-2
  (t/is (= 998900 (time (part-2-solver input)))))
