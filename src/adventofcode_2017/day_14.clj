(ns adventofcode-2017.day-14
  (:require [adventofcode-2017.day-10 :as knot]
            [utils :as u]))

(def input "uugsqrei")

(defn part-1-solver [input]
  (->> (range 128)
       (map #(str input "-" %))
       (map (partial mapv int))
       (mapcat knot/knot-hash-binary-str)
       (filter #{\1})
       (count)))

(defn neighbours [[x y]]
  [[(dec x) y]
   [(inc x) y]
   [x (dec y)]
   [x (inc y)]])

(defn search [grid start]
  (u/search {:q (into u/dfs-queue [start])
             :seen #{start}}
            (fn [{:keys [seen q]}]
              (when (nil? (peek q))
                [(reduce (fn [grid v]
                           (assoc-in grid v false))
                         grid
                         seen)
                 seen]))
            (fn [{:keys [seen q] :as state}]
              (let [current (peek q)
                    next (->> (neighbours current)
                              (remove seen)
                              (filter (partial get-in grid)))]
                (-> state
                    (update :seen into next)
                    (update :q pop)
                    (update :q into next))))))

(defn part-2-solver [input]
  (let [grid
        (->> (range 128)
             (map #(str input "-" %))
             (map (partial mapv int))
             (mapv (comp (partial mapv (comp boolean #{\1})) knot/knot-hash-binary-str)))
        vs (for [x (range 128)
                 y (range 128)]
             [x y])]
    (->> vs
         (reduce (fn [[grid regions] v]
                   (if (get-in grid v)
                     (let [[grid seen] (search grid v)]
                       [grid (cond-> regions (seq seen) inc)])
                     [grid regions]))
                 [grid 0])
         (last))))
