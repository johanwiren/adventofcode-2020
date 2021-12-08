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

(def by-segment-count
  {2 1
   3 7
   4 4
   7 8})

(defn parse-input [input]
  (map parse-line input))

(defn part-1-solver [input]
  (->> (parse-input input)
       (mapcat :out)
       (filter (comp (set (keys by-segment-count)) count))
       (count)))

(defn match [sig-map sig]
  (let [matches (->> sig-map
                     (filter (comp (partial set/superset? sig)
                                   second))
                     (map first))
        n (case (count sig)
            6 (case (set matches)
                #{} 6
                #{1 7} 0
                #{1 7 4} 9)
            5 (if (set/subset? sig (get sig-map 6))
                5
                (if (set/subset? sig (get sig-map 9))
                  3
                  2)))]
    {n sig}))

(defn identify-signals [signals]
  (let [grouped (group-by (comp (set (keys by-segment-count))
                                count)
                          signals)
        sig-map (into {}
                      (map (fn [[k v]]
                             [(get by-segment-count k) (first v)]))
                      (select-keys grouped (keys by-segment-count)))]
    (->> (get grouped nil)
         (sort-by count)
         (reverse)
         (reduce (fn [acc sig]
                   (merge acc (match acc sig)))
                 sig-map)
         (into {} (map (juxt second first))))))

(defn decode [{:keys [sig out]}]
  (let [lookup (identify-signals sig)]
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
