(ns adventofcode-2020.day-25
  (:require [clojure.test :as t]))

(def reference-input [5764801 17807724])

(def input [15335876 15086442])

(defn parse-input [in]
  in)

(defn xf [subject-number]
  (iterate (comp #(rem % 20201227)
                 (partial * subject-number))
           1))

(defn loop-xf [subject-number loop-count]
  (->> (xf subject-number)
       (drop loop-count)
       (first)))

(defn part-1-solver [in]
  (let [loop-count (->> (xf 7)
                        (take-while (complement (set (next in))))
                        (count))]
    (loop-xf (first in) loop-count)))

(defn part-2-solver [in]
  in)

(t/deftest part-1-test
  (t/is (= 14897079 (part-1-solver reference-input)))
  (t/is (= 11707042 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= :FIXME (part-2-solver reference-input))))
