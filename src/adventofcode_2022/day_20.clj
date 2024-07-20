(ns adventofcode-2022.day-20
  (:require [utils :as u]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn parse-input [input]
  (mapv parse-long input))

(defn circular-list [input]
  (let [len (count input)]
    (->> input
         (map-indexed (fn [i v]
                        [(if (zero? i) (dec len) (dec i))
                         v
                         (if (= (dec len) i) 0 (inc i))]))
         (vec))))

(defn mix [lst rounds]
  (let [len (count lst)]
    (->> (apply concat (repeat rounds (range len)))
         (reduce (fn [lst' i]
                   (let [[prev val next] (get lst' i)
                         steps (mod (if (pos? val)
                                      val
                                      (- val (dec len)))
                                    (dec len))
                         dest-node (if (zero? steps)
                                     nil
                                     (->> i
                                          (iterate (comp peek lst'))
                                          (drop steps)
                                          (first)))]
                     (if dest-node
                       (-> lst'
                           (assoc-in [prev 2] next)
                           (assoc-in [next 0] prev)

                           (assoc-in [(get-in lst' [dest-node 2]) 0] i)
                           (assoc-in [i 2] (get-in lst' [dest-node 2]))

                           (assoc-in [dest-node 2] i)
                           (assoc-in [i 0] dest-node))

                       lst')))
                 lst))))

(defn solve [lst rounds]
  (let [mixed (mix lst rounds)]
    (->> mixed
         (filter (comp zero? second))
         (first)
         (iterate (comp mixed peek))
         (drop 1)
         (partition 1000)
         (map last)
         (take 3)
         (map second)
         (reduce +))))

(defn part-1-solver [input]
  (solve (circular-list (parse-input input)) 1))

(defn part-2-solver [input]
  (solve (->> input
              parse-input
              (map (partial * 811589153))
              circular-list) 10))

(t/deftest part-1-test
  (t/is (= 4578 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 2159638736133 (time (part-2-solver input)))))
