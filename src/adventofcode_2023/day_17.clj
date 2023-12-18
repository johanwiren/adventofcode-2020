(ns adventofcode-2023.day-17
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-input [input]
  (u/to-xy-point-map (comp parse-long str) input))

(defn neighbours [[x y]]
  [[(dec x) y]
   [(inc x) y]
   [x (dec y)]
   [x (inc y)]])

(defn solver [input min-travel max-travel]
  (let [g (parse-input input)
        [_ goal] (u/bbox-2d (keys g))
        start {:pos [0 0] :dir [] :steps 0}
        result-fn (fn [{:keys [q] :as state}]
                    (when (= goal (-> q first :node :pos))
                      state))
        neighbours-fn (fn [{:keys [pos dir steps]}]
                        (->> (neighbours pos)
                             (map (fn [neighbour-pos]
                                    (let [neighbour-dir (mapv - neighbour-pos pos)
                                          travel-len (if (= dir neighbour-dir)
                                                       1
                                                       min-travel)
                                          travel (->> (iterate (fn [pos']
                                                                 (mapv + pos' neighbour-dir))
                                                               neighbour-pos)
                                                      (take travel-len))]
                                      {:pos (last travel)
                                       :dir neighbour-dir
                                       :steps (if (= dir neighbour-dir)
                                                (inc steps)
                                                min-travel)
                                       :cost (reduce + (keep g travel))})))
                             (filter (comp g :pos))
                             (remove (comp (partial = (mapv - dir)) :dir))
                             (remove (comp (partial < max-travel) :steps))))]
    (->> (u/dijkstra {:start start
                      :neighbours-fn neighbours-fn
                      :cost-fn (fn [_u v] (:cost v))
                      :result-fn result-fn})
         (:g-score)
         (filter (comp #{goal} :pos first))
         (map last)
         (sort)
         (first))))

(defn part-1-solver [input]
  (solver input 1 3))

(defn part-2-solver [input]
  (solver input 4 10))
