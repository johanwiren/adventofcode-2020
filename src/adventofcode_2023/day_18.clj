(ns adventofcode-2023.day-18
  (:require [clojure.string :as str]
            [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-input [input]
  (for [line input]
    (let [[dir steps more] (str/split line #" ")]
      [(keyword dir) (parse-long steps) (parse-long (subs more 7 8)) (Integer/parseInt (subs more 2 7) 16)])))

(def to-move {:R [1 0] :L [-1 0] :U [0 -1] :D [0 1]
              0 [1 0] 1 [0 1] 2 [-1 0] 3 [0 -1]})

(defn interior-area [polygon]
  (let [sum (->> (range (dec (count polygon)))
                 (map (fn [i]
                        (- (* (get-in polygon [i 0])
                              (get-in polygon [(inc i) 1]))
                           (* (get-in polygon [(inc i) 0])
                              (get-in polygon [i 1])))))
                 (reduce +)
                 (abs))]
    (/ sum 2)))

(defn picks-theorem [interior exterior]
  (inc (+ interior (/ exterior 2))))

(defn solver [instrs]
  (let [polygon (->> instrs
                     (reduce (fn [painted [dir steps]]
                               (let [pos (peek painted)]
                                 (conj painted (mapv + pos (mapv * (repeat steps) (to-move dir))))))
                             [[0 0]]))
        exterior (reduce + (map second instrs))
        interior (interior-area polygon)]
    (picks-theorem interior exterior)))

(defn part-1-solver [input]
  (solver (parse-input input)))

(defn part-2-solver [input]
  (solver (map (partial drop 2) (parse-input input))))
