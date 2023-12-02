(ns adventofcode-2017.day-19
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-input [input]
  (let [grid (vec input)
        is-char? (complement #{\space})]
    (into {}
          (for [x (range (count grid))
                y (range (count (get grid x)))
                :let [char (get-in grid [x y])]
                :when (is-char? char)]
            [[x y] char]))))

(defn neighbours [[x y]]
  [[x (dec y)] [(dec x) y] [(inc x) y] [x (inc y)]])

(defn solver [input]
  (let [grid (parse-input input)
        keep-char? (complement #{\| \- \+})]
    (->> {:pos [0 0]
          :last-pos #{[0 5]}
          :dir [0 0]
          :steps 0
          :chars ""}
         (iterate (fn [{:keys [pos last-pos dir chars steps]}]
                    (let [[next & more] (->> pos
                                             neighbours
                                             (filter grid)
                                             (remove last-pos))
                          new-pos (if more (mapv + dir pos) next)]
                      {:pos new-pos
                       :steps (inc steps)
                       :last-pos (set [pos])
                       :dir (mapv - new-pos pos)
                       :chars (cond-> chars
                                (keep-char? (get grid new-pos)) (str (get grid new-pos)))})))
         (take-while :pos)
         (last))))

(defn part-1-solver [input]
  (:chars (solver input)))

(defn part-2-solver [input]
  (:steps (solver input)))
