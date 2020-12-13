(ns adventofcode-2020.day-13
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn parse-input [in]
  {:ts (-> in first edn/read-string)
   :buses (-> in second (str/split #","))})

(defn part-1-solver [in]
  (let [{:keys [ts buses]} (parse-input in)
        [id departs] (->> buses
                          (remove #{"x"})
                          (map edn/read-string)
                          (map (juxt identity (fn [x]
                                                (+ x (* x (quot ts x))))))
                          (sort-by second)
                          (first))]
    (* id (- departs ts))))

(defn part-2-solver [in]
  in)

(comment

  (parse-input reference-input)

  (part-1-solver reference-input)

  (part-1-solver input)

  (part-2-solver input)

  )

(t/deftest part-1-test
  (t/is (= 295 (part-1-solver reference-input)))
  (t/is (= 370 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= :FIXME (part-2-solver reference-input))))
