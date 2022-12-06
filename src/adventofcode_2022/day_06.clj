(ns adventofcode-2022.day-06
  (:require [adventofcode-2022.utils :as u]
            [clojure.test :as t]))

(def input (-> (u/line-seq-input *ns*)
               (first)
               (vec)))

(defn find-marker-pos [vec marker-size]
  (loop [pos 0]
    (if (apply distinct? (subvec vec pos (+ pos marker-size)))
      (+ pos marker-size)
      (recur (inc pos)))))

(defn part-1-solver [input]
  (find-marker-pos input 4))

(defn part-2-solver [input]
  (find-marker-pos input 14))

(t/deftest part-1-test
  (t/is (= 1093 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 3534 (time (part-2-solver input)))))
