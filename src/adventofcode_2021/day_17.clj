(ns adventofcode-2021.day-17)

(def input "target area: x=150..171 y=-129..-70")

(defn parse-input [input]
  (->> input
       (re-matches #"target area: x=(.*)\.\.(.*) y=(.*)\.\.(.*)")
       rest
       (map #(Integer/parseInt %))
       (zipmap [:min-x :max-x :min-y :max-y])))

(def min-x-velocity 17)
(def max-x-velocity 171)
(def max-y-velocity 128)
(def min-y-velocity -129)

(def probe {:position [0 0]
            :velocity [0 0]})

(defn steps [start end]
  (if (< start end)
    (range start (inc end))
    (range start (dec end) -1)))

(defn gravity [[x y]]
  [x (dec y)])

(defn drag [[x y]]
  [(max 0 (dec x)) y])

(defn missed-target? [{:keys [max-x min-y]} {[x y] :position}]
  (or (< max-x x)
      (> min-y y)))

(defn hit-target? [{:keys [min-x max-x min-y max-y]} {[x y] :position}]
  (and (<= min-x x max-x)
       (<= min-y y max-y)))

(defn trajectory-step [{:keys [velocity] :as probe}]
  (-> probe
      (update :position (partial mapv +) velocity)
      (update :velocity gravity)
      (update :velocity drag)))

(defn shoot [probe]
  (iterate trajectory-step probe))

(defn part-1-solver [_]
  ;; "Solution" for part 1
  (->> (assoc probe :velocity [18 128])
       (shoot)
       (take-while (comp nat-int? last :position))
       #_(drop-while (comp nat-int? last :position))
       (drop 120)
       (take 20)))

(defn reached-end? [target probe]
  (or (hit-target? target probe)
      (missed-target? target probe)))

(defn part-2-solver [input]
  (let [target (parse-input input)]
    (-> (for [x (steps min-x-velocity max-x-velocity)
              y (steps min-y-velocity max-y-velocity)
              :let [end-state (->> (assoc probe :velocity [x y])
                                   (shoot)
                                   (drop-while (complement (partial reached-end? target)))
                                   (first))]
              :when (hit-target? target end-state)]
          [x y])
        (set)
        (count))))
