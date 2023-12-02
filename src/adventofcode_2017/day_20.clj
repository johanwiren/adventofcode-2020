(ns adventofcode-2017.day-20
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (->> (re-seq #"[\d-]+" line)
       (map parse-long)
       (partition 3)
       (map vec)
       (zipmap [:p :v :a])))

(defn move [ticks {:keys [p v a] :as particle}]
  (let [a-ticks (mapv * a [ticks ticks ticks])
        v' (mapv + v a-ticks)
        dp (->> v'
                (mapv + a v)
                (mapv (fn [x] (* (/ x 2) ticks))))
        p (mapv + p dp)]
    (assoc particle :v v' :p p)))

(defn part-1-solver [input]
  (->> input
       (map parse-line)
       (map (partial move 1000000))
       (map-indexed vector)
       (sort-by (comp (partial reduce +)
                      (partial map abs)
                      :p
                      second))
       (ffirst)))

(defn simple-move [{:keys [p v a] :as particle}]
  (let [v (mapv + v a)
        p (mapv + p v)]
    (assoc particle :v v :p p)))

(defn part-2-solver [input]
  (->> input
       (map parse-line)
       (iterate (fn [ps] (->> ps
                              (mapv simple-move)
                              (group-by :p)
                              (remove (comp (partial < 1) count second))
                              (vals)
                              (flatten))))
       (drop 100)
       (first)
       (count)))
