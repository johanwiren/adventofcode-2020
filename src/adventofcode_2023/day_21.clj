(ns adventofcode-2023.day-21
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn neighbours [[x y]]
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])

(defn solver [input steps-remaining]
  (let [map' (u/to-xy-point-map #{\S \.} input)
        [_ [max-x max-y]] (u/bbox-2d (keys map'))
        S (ffirst (filter (comp #{\S} val) map'))
        in-map? (fn [[x y]]
                  (get map' [(mod x (inc max-x)) (mod y (inc max-y))]))]
    (->> (u/search {:q (into u/bfs-queue [{:pos S :steps 0}])
                    :seen #{}
                    :reached 0}
                   (fn [{:keys [q] :as state}]
                     (when (nil? (peek q))
                       state))
                   (fn [{:keys [q seen] :as state}]
                     (let [{:keys [pos steps] :as node} (peek q)
                           state (update state :q pop)]
                       (if (= steps-remaining steps)
                         state
                         (let [neighbours (->> (neighbours pos)
                                               (filter in-map?)
                                               (remove seen))]
                           (-> state
                               (update :q into (map (fn [pos]
                                                      (-> node
                                                          (assoc :pos pos)
                                                          (update :steps inc)))
                                                    neighbours))
                               (update :reached + (if (or (and (even? steps) (odd? steps-remaining))
                                                          (and (odd? steps) (even? steps-remaining)))
                                                    (count neighbours)
                                                    0))
                               (update :seen into neighbours)))))))
         (:reached))))

(defn part-1-solver [input]
  (solver input 64))

(defn part-2-solver [input]
  (u/lagrange-interpolation [65 (+ 65 131) (+ 65 131 131)]
                            (partial solver input) 26501365))
