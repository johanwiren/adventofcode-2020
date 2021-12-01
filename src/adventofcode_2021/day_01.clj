(ns adventofcode-2021.day-01
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :as t]))

(def input (->> "2021/day_01.txt" (io/resource) (io/reader) (line-seq) (map edn/read-string)))

(defn solver [step input]
  (->> (drop step input)
       (map < input)
       (filter true?)
       count))

(def part-1-solver (partial solver 1))

(def part-2-solver (partial solver 3))

(t/deftest part-1-test
  (t/is (= 1228 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 1257 (part-2-solver input))))
