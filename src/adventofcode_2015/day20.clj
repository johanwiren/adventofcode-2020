(ns adventofcode-2015.day20
  (:require [clojure.math :as math]))

(def input (int 36000000))

(defn divisors [n]
  (->> (range 1 (inc (math/sqrt n)))
       (into []
             (comp
              (filter (fn [n'] (zero? (mod n n'))))
              (mapcat (fn [n'] (let [res (/ n n')] (if (= n' res)
                                                     [n']
                                                     [n' res]))))))))

(defn give-presents [n]
  (* 10 (reduce + (divisors n))))

(defn part-1-solver [input]
  (->> (range)
       (pmap give-presents)
       (take-while (partial > input))
       (count)))

(defn part-2-solver [input]
  (->> (iterate (fn [[n _ given]]
                  (let [n (inc n)
                        divisors (->> (divisors n)
                                      (remove (comp #{50} given)))]
                    [n (* 11 (reduce + divisors)) (reduce (fn [acc n]
                                                            (update acc n (fnil inc 0)))
                                                          given
                                                          divisors)]))
                [0 0 {}])
       (drop-while (comp (partial > input) second))
       (ffirst)))
