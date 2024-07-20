(ns adventofcode-2022.day-22
  (:require [utils :as u]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn parse-input [input]
  (let [[map' _ [instr & _]] (partition-by #{""} input)]
    {:nodes (->> map'
                 (map-indexed (fn [n-row row]
                                 (map-indexed (fn [n-col col]
                                                (when (= \. col)
                                                  [n-row n-col]))
                                              row)))
                 (apply concat)
                 (remove nil?)
                 (into #{}))
     :instrs (->> instr
                  (re-seq #"(\d+)(\w)?")
                  (map (fn [[_ steps dir]]
                         [(parse-long steps)
                          (case dir "L" -1 "R" 1 nil)])))}))

(defn move [nodes next-fn init-state [steps dir]]
  (let [{init-facing :facing, init-node :node}
        init-state

        {:keys [facing node]}
        (->> (iterate (fn [current-state]
                        (let [next-state (next-fn current-state)]
                          (if (get nodes (:node next-state))
                            (merge current-state next-state)
                            current-state)))
                      {:facing init-facing :node init-node})
             (drop steps)
             (first))
        new-facing (if dir (mod (+ facing dir) 4) facing)]
    {:facing new-facing :node node}))

(defn solve [{:keys [nodes instrs]} next-fn]
  (let [{:keys [facing node]} (reduce (partial move nodes next-fn) {:facing 0 :node [0 50]} instrs)
        [y x] node]
    (+ (* 1000 (inc y))
       (* 4 (inc x))
       facing)))

(defn p1-next-fn [{:keys [facing node]}]
  (let [[y x] node]
    (cond
      (and (= 3 facing) (zero? y) (<= 50 x 99))
      {:node [149 x]}

      (and (= 3 facing) (zero? y) (<= 100 x 149))
      {:node [49 x]}

      (and (= 2 facing) (<= 0 y 49) (= 50 x))
      {:node [y 149]}

      (and (= 0 facing) (<= 0 y 49) (= 149 x))
      {:node [y 50]}

      (and (= 1 facing) (= 49 y) (<= 100 x 149))
      {:node [0 x]}

      (and (= 2 facing) (<= 50 y 99) (= 50 x))
      {:node [y 99]}

      (and (= 0 facing) (<= 50 y 99) (= 99 x))
      {:node [y 50]}

      (and (= 3 facing) (= 100 y) (<= 0 x 49))
      {:node [199 x]}

      (and (= 2 facing) (<= 100 y 149) (zero? x))
      {:node [y 99]}

      (and (= 0 facing) (<= 100 y 149) (= 99 x))
      {:node [y 0]}

      (and (= 1 facing) (= 149 y) (<= 50 x 99))
      {:node [0 x]}

      (and (= 2 facing) (<= 150 y 199) (= 0 x))
      {:node [y 49]}

      (and (= 0 facing) (<= 150 y 199) (= 49 x))
      {:node [y 0]}

      (and (= 1 facing) (= 199 y))
      {:node [100 x]}

      :else
      {:node (case facing
               0 [y (inc x)]
               1 [(inc y) x]
               2 [y (dec x)]
               3 [(dec y) x])})))

(defn part-1-solver [input]
  (solve (parse-input input) p1-next-fn))

(defn p2-next-fn [{:keys [facing node]}]
  (let [[y x] node]
    (cond
      (and (= 3 facing) (zero? y) (<= 50 x 99))
      {:facing 0 :node [(+ 150 (- x 50)) 0]}

      (and (= 3 facing) (zero? y) (<= 100 x 149))
      {:facing 3 :node [199 (- x 100)]}

      (and (= 2 facing) (<= 0 y 49) (= 50 x))
      {:facing 0 :node [(- 149 y) 0]}

      (and (= 0 facing) (<= 0 y 49) (= 149 x))
      {:facing 2 :node [(- 149 y) 99]}

      (and (= 1 facing) (= 49 y) (<= 100 x 149))
      {:facing 2 :node [(- x 50) 99]}

      (and (= 2 facing) (<= 50 y 99) (= 50 x))
      {:facing 1 :node [100 (- y 50)]}

      (and (= 0 facing) (<= 50 y 99) (= 99 x))
      {:facing 3 :node [49 (+ y 50)]}

      (and (= 3 facing) (= 100 y) (<= 0 x 49))
      {:facing 0 :node [(+ 50 x) 50]}

      (and (= 2 facing) (<= 100 y 149) (zero? x))
      {:facing 0 :node [(- 149 y) 50]}

      (and (= 0 facing) (<= 100 y 149) (= 99 x))
      {:facing 2 :node [(- 149 y) 149]}

      (and (= 1 facing) (= 149 y) (<= 50 x 99))
      {:facing 2 :node [(+ 100 x) 49]}

      (and (= 2 facing) (<= 150 y 199) (= 0 x))
      {:facing 1 :node [0 (- y 100)]}

      (and (= 0 facing) (<= 150 y 199) (= 49 x))
      {:facing 3 :node [149 (- y 100)]}

      (and (= 1 facing) (= 199 y))
      {:facing 1 :node [0 (+ 100 x)]}

      :else
      {:facing facing :node (case facing
                              0 [y (inc x)]
                              1 [(inc y) x]
                              2 [y (dec x)]
                              3 [(dec y) x])})))

(defn part-2-solver [input]
  (solve (parse-input input) p2-next-fn))

(t/deftest part-1-test
  (t/is (= 131052 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 4578 (time (part-2-solver input)))))
