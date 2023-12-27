(ns adventofcode-2023.day-23
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn neighbours [[x y]]
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])

(defn dijkstra
  "Modified version of u/dijkstrat that never visits the same node twice"
  [{:keys [start neighbours-fn cost-fn]}]
  (let [search-state {:q (into (u/sorted-set-by-key :cost) [{:cost 0
                                                             :node start
                                                             :node-came-from #{start}}])
                      :g-score {start 0}
                      :came-from {}}
        next-state (fn [{:keys [q g-score] :as state}]
                     (let [q-item (first q)
                           {:keys [node node-came-from]} q-item
                           neighbours (->> (neighbours-fn node)
                                           (remove node-came-from))]
                       (reduce (fn [state neighbour]
                                 (let [score (+ (cost-fn node neighbour)
                                                (g-score node Double/POSITIVE_INFINITY))]
                                   (if (< score (g-score neighbour Double/POSITIVE_INFINITY))
                                     (-> state
                                         (update :q conj {:cost score
                                                          :node neighbour
                                                          :node-came-from (conj node-came-from node)})
                                         (update :came-from assoc neighbour node)
                                         (update :g-score assoc neighbour score))
                                     state)))
                               (update state :q disj q-item)
                               neighbours)))]
    (->> (iterate next-state search-state)
         (drop-while (comp first :q))
         (first))))

(defn part-1-solver [input]
  (let [map' (u/to-xy-point-map #{\. \< \> \v \^} input)
        {:keys [g-score]}
        (->> (dijkstra {:start [1 0]
                        :neighbours-fn (fn [[x y :as pos]]
                                         (let [v (map' pos)]
                                           (case v
                                             \> [[(inc x) y]]
                                             \< [[(dec x) y]]
                                             \v [[x (inc y)]]
                                             \^ [[x (dec y)]]
                                             (->> (neighbours pos)
                                                  (filter map')))))
                        :cost-fn (constantly -1)
                        :result-fn (constantly nil)}))]
    (->> (sort-by second g-score)
         (first)
         (second)
         (-))))

(defn part-2-solver [input]
  (let [g (u/to-xy-point-map #{\. \< \> \v \^} input)
        [start goal] (u/bbox-2d (keys g))
        small-g
        (u/search {:q (conj u/bfs-queue {:pos start :seen #{start} :came-from start :steps 0})
                   :small-g {}}
                  (fn [{:keys [q small-g]}]
                    (when (empty? q)
                      small-g))
                  (fn [{:keys [q small-g] :as state}]
                    (let [{:keys [pos seen came-from steps]} (peek q)
                          neighbours (->> (neighbours pos)
                                          (filter g)
                                          (remove seen)
                                          (remove (every-pred #(get-in small-g [% came-from])
                                                              #(get-in small-g [came-from %]))))
                          save-path? (or (< 1 (count neighbours))
                                         (= goal pos))
                          nodes (map (fn [neigh-pos]
                                       (let [came-from (if save-path? pos came-from)
                                             steps (if save-path? 1 (inc steps))]
                                         {:pos neigh-pos
                                          :came-from came-from
                                          :steps steps
                                          :seen (conj seen pos)}))
                                     neighbours)]
                      (-> state
                          (update :q pop)
                          (update :q into nodes)
                          (cond->
                           save-path? (-> (assoc-in [:small-g came-from pos] steps)
                                          (assoc-in [:small-g pos came-from] steps)))))))
        neighbours-fn (comp keys small-g)
        cost-fn (fn [u v] (get-in small-g [u v]))]
    (u/search {:q (conj u/dfs-queue {:seen #{start} :path [start] :pos start})
               :max-path 0}
              (fn [{:keys [q max-path]}]
                (when (empty? q)
                  max-path))
              (fn [{:keys [q] :as state}]
                (let [{:keys [pos seen path]} (peek q)
                      neighbours (->> (neighbours-fn pos)
                                      (remove seen))]
                  (reduce (fn [state neighbour]
                            (cond-> (update state :q conj {:pos neighbour
                                                           :path (conj path neighbour)
                                                           :seen (conj seen neighbour)})
                              (= goal neighbour) (update :max-path max (->> (conj path neighbour)
                                                                            (partition 2 1)
                                                                            (map (partial apply cost-fn))
                                                                            (reduce +)))))
                          (update state :q pop)
                          neighbours))))))
