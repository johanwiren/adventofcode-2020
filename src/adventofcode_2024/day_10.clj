(ns adventofcode-2024.day-10
  (:require
   [clojure.set :as set]
   [clojure.test :as t]
   [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn neighbours [pos]
  (map #(mapv + %1 %2)
        (repeat pos)
        [[0 -1] [1  0] [0  1] [-1 0]]))

(defn parse [input]
  (mapv #(into [] (map parse-long) (re-seq #"\d" %)) input))

(defn nines-reachable [g edge]
  (->> [edge]
       (iterate (fn [edges]
                  (mapcat #(get g %) edges)))
       (drop 9)
       (first)
       (set)))

(defn paths-reachable [g edge]
  (->> [[edge]]
       (iterate (fn [paths]
                  (mapcat (fn [path] (map #(conj path %) (get g (peek path))))
                          paths)))
       (drop 9)
       (first)))

(defn solver [input head-fn]
  (let [nodes (parse input)
        edges (for [y (range (count nodes))
                    x (range (count (first nodes)))
                    neighbour (filter #(= (get-in nodes [y x])
                                          (dec (get-in nodes % -10)))
                                      (neighbours [y x]))]
                {[y x] #{neighbour}})
        g (apply merge-with set/union edges)
        heads (filter #(= 0 (get-in nodes %)) (keys g))]
    (->> heads
         (map #(head-fn g %))
         (map count)
         (reduce +))))

(defn part-1-solver [input]
  (solver input nines-reachable))

(defn part-2-solver [input]
  (solver input paths-reachable))

(t/deftest part-1-test
  (t/is (= 535 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 1186 (part-2-solver input))))
