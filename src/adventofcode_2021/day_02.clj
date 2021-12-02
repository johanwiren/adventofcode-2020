(ns adventofcode-2021.day-02
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.test :as t]))

(def input (->> "2021/day_02.txt"
                (io/resource)
                (io/reader)
                (line-seq)
                (map #(str/split % #" "))
                (map (fn [[dir steps]]
                       {(keyword dir) (Integer/parseInt steps)}))))

(defn part-1-solver [input]
  (let [{:keys [up down forward]} (apply merge-with + input)]
    (* (- down up) forward)))

(defn move-forward [{:keys [aim] :as sub} steps]
  (-> sub
      (update :pos + steps)
      (update :depth + (* aim steps))))

(defn part-2-solver [input]
  (let [{:keys [pos depth]}
        (reduce (fn [acc {:keys [up down forward]}]
                  (cond-> acc
                    up (update :aim - up)
                    down (update :aim + down)
                    forward (move-forward forward)))
                {:aim 0
                 :depth 0
                 :pos 0}
                input)]
    (* pos depth)))

(t/deftest part-1
  (t/is (= 1990000 (part-1-solver input))))

(t/deftest part-2
  (t/is (= 1975421260 (part-2-solver input))))
