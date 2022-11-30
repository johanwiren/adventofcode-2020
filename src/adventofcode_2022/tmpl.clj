(ns adventofcode-2022.tmpl
  (:require [adventofcode-2022.utils :as u]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn part-1-solver [input]
  input)

(defn part-2-solver [input]
  input)

(t/deftest part-1-test
  (t/is (= :FIXME (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= :FIXME (time (part-2-solver input)))))
