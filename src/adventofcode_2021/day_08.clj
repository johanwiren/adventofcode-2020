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

(defn matches [sig n-sig]
  (into #{}
        (comp (filter (comp (partial set/superset? sig) second))
              (map first))
        n-sig))

(defn sig-lookup [sigs]
  (let [counts (group-by count sigs)]
    (->> (concat (get counts 6) (get counts 5))
         (reduce (fn [n-sig sig]
                   (let [n (case (count sig)
                             6 (case (matches sig n-sig)
                                 #{} 6
                                 #{1 7} 0
                                 #{1 4 7} 9)
                             5 (if (set/subset? sig (get n-sig 6))
                                 5
                                 (if (set/subset? sig (get n-sig 9))
                                   3
                                   2)))]
                     (assoc n-sig n sig)))
                 {1 (first (get counts 2))
                  4 (first (get counts 4))
                  7 (first (get counts 3))
                  8 (first (get counts 7))})
         (into {} (map (juxt second first))))))

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
