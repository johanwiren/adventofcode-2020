(ns adventofcode-2021.day-01
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.test :as t]))

(def input (-> "2021/day_01.txt" (io/resource) (io/reader) (line-seq)))


(defn parse-input [input]
  (map edn/read-string input))

(defn- part-1 [in]
  (->> (drop 1 in)
       (reduce (fn [acc item]
                 (conj acc [item (- item (first (last acc)))]))
               [[(first in) 0]])
       (filter (comp pos? second))
       count))

(defn part-1-solver [input]
  (let [in (parse-input input)]
    (part-1 in)))

(defn part-2-solver [input]
  (let [in (parse-input input)]
    (->> in
         (reduce (fn [acc item]
                   (if (<= 3 (count acc))
                     (conj acc [item (->> acc
                                          (take-last 3)
                                          (map first)
                                          (reduce +))])
                     (conj acc [item 0])))
                 [])
         (map second)
         (part-1))))


(comment
  (part-1-solver input)

  (part-2-solver input)

  )



(t/deftest part-1-test
  (t/is (= 1228 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 1257 (part-2-solver input))))
