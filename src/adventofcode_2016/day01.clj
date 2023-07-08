(ns adventofcode-2016.day01
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-input [input]
  (->> (first input)
       (re-seq #"(\w)(\d+)")
       (map rest)
       (map (fn [[dir steps]]
              [(if (= "L" dir) -1 1) (parse-long steps)]))))


(def dirs [[-1 0] [0 1] [1 0] [0 -1]])

(defn init-state []
  {:facing 0
   :pos [0 0]})

(defn part-1-solver [input]
  (->> (parse-input input)
       (reduce (fn [{:keys [facing pos]} [rot steps]]
                 (let [facing (mod (+ facing rot) 4)
                       move (mapv (partial * steps) (get dirs facing))]
                   {:facing facing
                    :pos (mapv + pos move)}))
               (init-state))
       :pos
       (map abs)
       (reduce +)))

(defn part-2-solver [input]
  (->> (parse-input input)
       (reduce (fn [{:keys [facing pos visited]} [rot steps]]
                 (let [facing (mod (+ facing rot) 4)
                       dir (get dirs facing)
                       positions (reductions (fn [pos _]
                                               (mapv + pos dir))
                                             pos
                                             (range steps))]
                   (if-let [re-visited (some visited (rest positions))]
                     (reduced re-visited)
                     {:facing facing
                      :pos (last positions)
                      :visited (into visited positions)})))
               (assoc (init-state) :visited #{}))
       (map abs)
       (reduce +)))
