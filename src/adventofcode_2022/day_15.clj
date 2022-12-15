(ns adventofcode-2022.day-15
  (:require [adventofcode-2022.utils :as u]
            [clojure.test :as t]))

(def scan-line 2000000)

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (let [[sx sy bx by] (->> (re-seq #"-?\d+" line)
                           (map parse-long))]
    {:sensor [sx sy]
     :beacon [bx by]}))

(defn parse-input [input]
  (map parse-line input))

(defn part-1-solver [input]
  (let [input (parse-input input)
        bcnscans-xs (->> input
                         (mapcat vals)
                         (filter (comp (partial = scan-line) second))
                         (map first)
                         (into #{}))]
    (->> input
         (sort-by (comp second :sensor))
         (reduce (fn [acc {:keys [sensor beacon]}]
                   (let [[sns-x sns-y] sensor
                         [bcn-x bcn-y] beacon
                         reach (->> sensor
                                    (mapv - beacon)
                                    (map abs)
                                    (apply +))
                         overlap (- reach
                                    (abs (- sns-y scan-line)))]
                     (cond-> acc
                       (nat-int? overlap)
                       (into (range (- sns-x overlap) (inc (+ sns-x overlap))))

                       (= sns-y scan-line)
                       (conj sns-x)

                       (= bcn-y scan-line)
                       (conj bcn-x))))
                 #{})
         (remove bcnscans-xs)
         (count))))

(defn part-2-solver [input]
  (->> (parse-input input)))

(t/deftest part-1-test
  (t/is (= 5511201 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= :FIXME (time (part-2-solver input)))))
