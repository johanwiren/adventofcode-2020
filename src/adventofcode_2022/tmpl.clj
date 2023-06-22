(ns adventofcode-2022.tmpl
  (:require [adventofcode-2022.utils :as u]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  line)

(defn parse-input [input]
  (map parse-line input))

(defn part-1-solver [input]
  (->> (parse-input input)))

(defn part-2-solver [input]
  (->> (parse-input input)))

(t/deftest part-1-test
  (t/is (= :fixme (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= :fixme (time (part-2-solver input)))))
