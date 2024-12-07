(ns adventofcode-2024.day-06
  (:require
   [clojure.test :as t]
   [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse [input]
  (u/to-xy-point-map identity input))

(def direction->movement
  {0       [0 -1]
   1             [1  0]
   2       [0  1]
   3 [-1 0]})

(defn step [pos direction]
  (mapv + pos (direction->movement direction)))

(defn should-turn? [{:keys [grid pos direction]}]
  (= \# (get grid (step pos direction))))

(defn turn [direction]
  (mod (inc direction) 4))

(defn walk [{:keys [grid pos direction] :as game}]
  (let [new-direction (if (should-turn? game) (turn direction) direction)
        new-pos (step pos new-direction)]
    (if-not (get grid new-pos)
      (assoc game :finished? true)
      (-> game
          (update :visited conj [new-pos new-direction])
          (assoc :direction new-direction)
          (assoc :pos new-pos)))))

(defn walk-map [map]
  (->> (iterate walk map)
       (drop-while (complement :finished?))
       (first)))

(defn part-1-solver [input]
  (let [grid (parse input)
        start-pos (ffirst (filter (comp #{\^} second) grid))]
    (->> (walk-map {:grid grid
                    :pos start-pos
                    :direction 0
                    :visited #{[start-pos 0]}})
         :visited
         (map first)
         set
         count)))

(defn walk-2 [{:keys [grid pos direction visited] :as game}]
  (let [new-direction (if (should-turn? game) (turn direction) direction)
        new-pos (if (should-turn? game) pos (step pos new-direction))]
    (cond
      (not (get grid new-pos))
      (assoc game :finish-reason :out-of-bounds)

      (visited [new-pos new-direction])
      (assoc game :finish-reason :loop)

      :else
      (-> game
          (update :visited conj [new-pos new-direction])
          (assoc :direction new-direction)
          (assoc :pos new-pos)))))

(defn walk-map-2 [map]
  (->> (iterate walk-2 map)
       (drop-while (complement :finish-reason))
       (first)
       :finish-reason))

(defn part-2-solver [input]
  (let [grid (parse input)
        start-pos (ffirst (filter (comp #{\^} second) grid))
        map' {:grid grid
              :pos start-pos
              :direction 0
              :visited #{[start-pos 0]}}
        path (->> (walk-map map')
                  :visited
                  (map first)
                  (set))]
    (->> path
         (remove (hash-set start-pos))
         (pmap (fn [pos]
                 (walk-map-2 (-> map'
                                 (update :grid assoc pos \#)))))
         (filter #{:loop})
         count)))

(t/deftest part-1-test
  (t/is (= 4778 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 1618 (part-2-solver input))))
