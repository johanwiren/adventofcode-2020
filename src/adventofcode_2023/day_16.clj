(ns adventofcode-2023.day-16
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn next-points [came-from point val]
  (let [[dir-x dir-y :as dir] (mapv - point came-from)]
    (cond
      (and (= \| val) (zero? dir-y))
      [(update point 1 inc) (update point 1 dec)]

      (and (= \- val) (zero? dir-x))
      [(update point 0 inc) (update point 0 dec)]

      (= \\ val)
      [(mapv + point [dir-y dir-x])]

      (= \/ val)
      [(mapv - point [dir-y dir-x])]

      :else [(mapv + point dir)])))

(defn count-energized [coords-map came-from pos]
  (let [[[min-x min-y] [max-x max-y]] (u/bbox-2d (keys coords-map))
        in-bbox? (fn [[x y]] (and (<= min-x x max-x)
                                  (<= min-y y max-y)))
        start {:came-from came-from :current-pos pos}]
    (->> (u/search {:q (conj u/dfs-queue start)
                    :visited #{start}}
                   (fn [{:keys [q visited]}]
                     (when (nil? (peek q))
                       visited))
                   (fn [{:keys [q visited] :as state}]
                     (let [{:keys [came-from current-pos] :as current-node} (peek q)
                           to-visit (->> (get coords-map current-pos)
                                         (next-points came-from current-pos)
                                         (filter in-bbox?)
                                         (map (fn [point]
                                                {:came-from   current-pos
                                                 :current-pos point}))
                                         (remove visited))]
                       (-> state
                           (update :q pop)
                           (update :q into to-visit)
                           (update :visited conj current-node)))))
         (map :current-pos)
         (into #{})
         (count))))

(defn part-1-solver [input]
  (let [coords-map (u/to-xy-point-map identity input)]
    (count-energized coords-map [-1 0] [0 0])))

(defn part-2-solver [input]
  (let [coords-map (u/to-xy-point-map identity input)
        [[min-x min-y] [max-x max-y]] (u/bbox-2d (keys coords-map))
        top (for [x (range min-x (inc max-x))] [[x (dec min-y)] [x min-y]])
        down (for [x (range min-x (inc max-x))] [[x (inc max-y)] [x max-y]])
        right (for [y (range min-y (inc max-y))] [[(inc max-x) y] [max-x y]])
        left (for [y (range min-y (inc max-y))] [[(dec min-x) y] [min-x y]])
        starts (concat top left down right)]
    (->> (pmap (partial apply count-energized coords-map) starts)
         (reduce max))))
