(ns adventofcode-2022.day-22
  (:require [adventofcode-2022.utils :as u]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn ccw [[y x]]
  [x (- y)])

(defn cw [[y x]]
  [(- x) y])

(def to-entries
  (map (fn [[row-no cols]]
         (map (fn [[col-no char]]
                [[row-no col-no] char])
              cols))))

(defn nested-map [map' [[row col] char]]
  (update map' row (fnil assoc (sorted-map)) col char))

(defn rotate-map [map' rotation]
  (transduce (comp to-entries
                   cat
                   (map #(update % 0 rotation)))
             (completing nested-map)
             (sorted-map)
             map'))

(defn parse-input [input]
  (let [[map' _ [instr & _]] (partition-by #{""} input)]
    {:map' (->> map'
                (map-indexed (fn [n-row row]
                               (map-indexed (fn [n-col col]
                                              (when (not= \space col)
                                                [[n-row n-col] col]))
                                            row)))
                (apply concat)
                (remove nil?)
                (reduce nested-map (sorted-map)))
     :instrs (->> instr
                  (re-seq #"(\d+)(\w)?")
                  (map (fn [[_ steps dir]]
                         [(parse-long steps)
                          (case dir "L" -1 "R" 1 nil)])))}))

(defn move [maps [facing [row col]] [steps dir]]
  (let [map' (get maps facing)
        new-dir (if dir (mod (+ facing dir) 4) facing)
        current-row (get map' row)
        rights (concat (drop-while #(>= col %) (keys current-row))
                       (keys current-row))
        rotation (if (and dir (pos? dir)) cw ccw)
        new-col (or (->> rights
                         (take-while (comp #{\.} current-row))
                         (take steps)
                         (last))
                    col)]
    [new-dir (rotation [row new-col])]))

(defn part-1-solver [input]
  (let [{:keys [instrs map']} (parse-input input)
        start-col (apply min (keys (get map' 0)))
        maps (->> map'
                  (iterate #(rotate-map % cw))
                  (take 4)
                  (vec))
        [facing pos] (reduce (partial move maps) [0 [0 start-col]] instrs)
        [row col] (cw pos)]
    (+ (* 1000 (inc row))
       (* 4 (inc col))
       facing)))

(defn part-2-solver [input]
  (->> (parse-input input)))

(t/deftest part-1-test
  (t/is (= 131052 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= :FIXME (time (part-2-solver input)))))
