(ns adventofcode-2024.day-03
  (:require
   [clojure.string :as str]
   [clojure.test :as t]
   [utils :as u]))

(def input (u/str-input *ns*))

(defn part-1-solver [input]
  (->> input
       (re-seq #"mul\((\d+),(\d+)\)")
       (map rest)
       (map #(map parse-long %))
       (map #(apply * %))
       (reduce +)))

(defn part-2-solver [input]
  (let [dos (-> (str "do()" (str/replace input #"\n" ""))
                (str/split #"don't\(\)"))]
    (->> dos
         (mapcat #(re-seq #"do\(\).*" %))
         (apply str)
         (part-1-solver))))

(t/deftest part-1-test
  (t/is (= 159892596 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 92626942 (part-2-solver input))))
