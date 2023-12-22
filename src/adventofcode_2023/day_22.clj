(ns adventofcode-2023.day-22
  (:require [utils :as u]
            [clojure.set :as set]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (let [[a b c x y z] (map parse-long (re-seq #"\d+" line))]
    [[a b c] [x y z]]))

(defn mk-brick [[[a b c] [x y z] :as cubes]]
  {:origin [a b c x y z]
   :cubes cubes
   :points (set (for [x (range a (inc x))
                      y (range b (inc y))
                      z (range c (inc z))]
                  [x y z]))})

(defn update-points [{:keys [cubes] :as brick}]
  (let [[[a b c] [x y z]] cubes]
    (assoc brick :points (set (for [x (range a (inc x))
                                    y (range b (inc y))
                                    z (range c (inc z))]
                                [x y z])))))

(defn down [x]
  (max 1 (dec x)))

(defn fall [brick]
  (let [{:keys [cubes]} brick]
    (cond-> brick
      (and (not= 1 (get-in cubes [0 2]))
           (not= 1 (get-in cubes [1 2])))
      (-> (update-in [:cubes 0 2] down)
          (update-in [:cubes 1 2] down)
          (update-points)))))

(defn z [{:keys [cubes]}]
  (apply min (map peek cubes)))

(defn gravity-step [bricks]
  (->> (reduce (fn [{:keys [bricks occupied-points]} brick]
                 (let [maybe-fallen (fall brick)
                       settled (if (some occupied-points (:points maybe-fallen))
                                 brick
                                 maybe-fallen)]
                   {:bricks (conj bricks settled)
                    :occupied-points (into occupied-points (:points settled))}))
               {:bricks [] :occupied-points #{}}
               (sort-by z bricks))
       (:bricks)))

(defn gravitate [bricks]
  (->> (iterate gravity-step bricks)
       (partition 2)
       (drop-while (partial apply not=))
       (ffirst)))

(defn part-1-solver [input]
  (let [bricks (->> (map parse-line input)
                    (map mk-brick)
                    (gravitate))
        can-remove? (fn [{:keys [cubes]}]
                      (let [bricks (remove (comp (partial = cubes) :cubes) bricks)]
                        (= bricks (gravitate bricks))))]
    (count (filter can-remove? bricks))))

(defn part-2-solver [input]
  (->> (let [bricks (->> (map parse-line input)
                         (map mk-brick)
                         (gravitate))]
         (for [brick bricks
               :let [removed (remove (comp (partial = (:cubes brick)) :cubes) bricks)]]
           (count (set/difference (set removed) (set (gravitate removed))))))
       (reduce +)))
