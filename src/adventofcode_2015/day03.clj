(ns adventofcode-2015.day03
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))

(def input (slurp (io/reader (io/resource "2015/day-03.txt"))))

(defn parse-move [move]
  (case move
    \< [-1 0]
    \> [+1 0]
    \^ [0 +1]
    \v [0 -1]))

(defn parse-input [input]
  (map parse-move input))

(defn deliver [directions]
  (reduce (fn [{:keys [pos] :as acc} pos']
            (let [new-pos (map + pos pos')]
              (-> acc
                  (update :visited conj new-pos)
                  (assoc :pos new-pos))))
          {:visited #{[0 0]}
           :pos [0 0]}
          directions))

(defn part-1-solver [input]
  (->> input
       (parse-input)
       (deliver)
       (:visited)
       (count)))

(defn part-2-solver [input]
  (let [directions (->> input
                        (parse-input))
        robo-path (take-nth 2 directions)
        santa-path (take-nth 2 (rest directions))]
    (->> [robo-path santa-path]
         (into #{}
               (comp (map deliver)
                     (mapcat :visited)))
         (count))))

(t/deftest part-1-test
  (t/is (= 2592 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 2360 (part-2-solver input))))
