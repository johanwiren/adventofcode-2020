(ns adventofcode-2025.day-06
  (:require
   [clojure.string :as str]
   [clojure.test :as t]
   [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (map (some-fn parse-long (comp resolve symbol))
       (re-seq #"\S+" line)))

(defn parse [input]
  (map parse-line input))

(defn part-1-solver [input]
  (->> (parse input)
       (apply map vector)
       (map (fn [col]
              (reduce (peek col) (pop col))))
       (reduce +)))

(defn part-2-solver [input]
  (->> (apply map vector input)
       (partition-by (partial every? #{\space}))
       (partition 1 2)
       (map first)
       (map (fn [col]
              (let [op (-> col first peek str symbol resolve)
                    xs (->> col
                            (map pop)
                            (map (partial apply str))
                            (map str/trim)
                            (map parse-long))]
                (reduce op xs))))
       (reduce +)))

(t/deftest part-1-test
  (t/is (= 6757749566978 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 10603075273949 (part-2-solver input))))
