(ns adventofcode-2023.day-10
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn connections-map [a b]
  {a b b a})

(defn mk-connected [pipe pos]
  (case pipe
    \| (connections-map (mapv + [1 0] pos) (mapv + [-1 0] pos))
    \- (connections-map (mapv + [0 1] pos) (mapv + [0 -1] pos))
    \J (connections-map (mapv + [-1 0] pos) (mapv + [0 -1] pos))
    \7 (connections-map (mapv + [0 -1] pos) (mapv + [1 0] pos))
    \F (connections-map (mapv + [0 1] pos) (mapv + [1 0] pos))
    \L (connections-map (mapv + [-1 0] pos) (mapv + [0 1] pos))
    (constantly nil)))

(defn parse-input [input]
  (let [n-col (count (first input))
        point (fn [i] [(quot i n-col) (rem i n-col)])]
    (->> input
         (mapcat (partial re-seq #"."))
         (reduce (fn [[pos xs] x]
                   (let [positions (into #{} (map point (range pos (+ pos (count x)))))]
                     [(+ pos (count x))
                      (->> positions
                           (map (fn [pos]
                                  (let [val (first x)]
                                    {:pos pos
                                     :val val
                                     :next (mk-connected val pos)})))
                           (into xs))]))
                 [0 []])
         (second)
         (remove (comp #{\.} :val))
         (map (juxt :pos identity))
         (into {}))))

(defn pipe-polygon [g]
  (let [start (:pos (first (filter (comp #{\S} :val) (vals g))))
        next (->> [[1 0] [0 1] [-1 0] [0 -1]]
                  (filter (fn [offset]
                            (let [next (get-in g [(mapv + start offset) :next])]
                              (some #{start} (keys next)))))
                  (first)
                  (mapv + start))]
    (->> [start next]
         (iterate (fn [[came-from pos]]
                    (when-let [next (get-in g [pos :next])]
                      [pos (next came-from)])))
         (take-while some?)
         (map first)
         (into []))))

(defn part-1-solver [input]
  (let [g (parse-input input)
        pipe-polygon (pipe-polygon g)]
    (quot (count pipe-polygon) 2)))

(defn area [polygon]
  (let [sum (->> (range (dec (count polygon)))
                 (map (fn [i]
                        (- (* (get-in polygon [i 0])
                              (get-in polygon [(inc i) 1]))
                           (* (get-in polygon [(inc i) 0])
                              (get-in polygon [i 1])))))
                 (reduce +)
                 (abs))]
    (/ sum 2)))

(defn part-2-solver [input]
  (let [g (parse-input input)
        pipe-polygon (pipe-polygon g)
        a (area pipe-polygon)]
    (inc (- a (quot (count pipe-polygon) 2)))))
