(ns adventofcode-2020.day-7
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn parse-line [line]
  (let [[_ bag contents] (re-matches #"(\w+ \w+) bags contain (.*)." line)
        inner-bags (into []
                         (comp (map (partial drop 1))
                               (map reverse)
                               (mapcat (fn [[color n]]
                                         (repeat (Integer/parseInt n) color))))
                         (re-seq #"(\d+) (\w+ \w+),?" contents))]
    {bag inner-bags}))

(defn parse-input [in]
  (into {}
        (map parse-line)
        in))

(defn unwrap [bags color]
  (let [inner-bags (get bags color)]
    (concat inner-bags
            (mapcat (partial unwrap bags)
                    inner-bags))))

(defn part-1-solver [in]
  (let [bags (parse-input in)]
    (->> bags
         (map (comp (partial unwrap bags)
                    first))
         (filter (partial some #{"shiny gold"}))
         count)))

(defn part-2-solver [in]
  (-> in
      parse-input
      (unwrap "shiny gold")
      count))

(comment

  (parse-input reference-input)

  (part-1-solver reference-input)

  (part-1-solver input)

  (part-2-solver reference-input)

  (part-2-solver input)

  )

(t/deftest part-1-test
  (t/is (= 4 (part-1-solver reference-input))))

(t/deftest part-2-test
  (t/is (= 32 (part-2-solver reference-input))))

