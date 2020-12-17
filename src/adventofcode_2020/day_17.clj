(ns adventofcode-2020.day-17
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn neighbours [[y x z]]
  (for [y' (range (dec y) (+ y 2))
        x' (range (dec x) (+ x 2))
        z' (range (dec z) (+ z 2))
        :when (not= [y x z] [y' x' z'])]
    [y' x' z']))

(defn neighbours4 [[y x z w]]
  (for [y' (range (dec y) (+ y 2))
        x' (range (dec x) (+ x 2))
        z' (range (dec z) (+ z 2))
        w' (range (dec w) (+ w 2))
        :when (not= [y x z w] [y' x' z' w'])]
    [y' x' z' w']))

(defn toggle-actives [{:keys [min max active] :as in}]
  (let [r (range min max)
        coords (for [y r x r z r] [y x z])]
    (assoc in
           :active
           (persistent!
            (reduce (fn [acc coord]
                      (let [neighbour-count (count (keep (partial get active)
                                                         (neighbours coord)))
                            active? (get active coord)]
                        (if (or (and active?
                                     (#{2 3} neighbour-count))
                                (and (not active?)
                                     (= 3 neighbour-count)))
                          (assoc! acc coord true)
                          acc)))
                    (transient {})
                    coords)))))

(defn toggle-actives4 [{:keys [active] :as in}]
  (let [[ys xs zs ws] (apply (partial map vector) (keys active))
        coords (for [y (range (dec (apply min ys)) (+ (apply max ys) 2))
                     x (range (dec (apply min xs)) (+ (apply max xs) 2))
                     z (range (dec (apply min zs)) (+ (apply max zs) 2))
                     w (range (dec (apply min ws)) (+ (apply max ws) 2))]
                 [y x z w])]
    (assoc in
           :active
           (into {}
                 (->> coords
                      (pmap (fn [coord]
                              (let [neighbour-count (count (->> (neighbours4 coord)
                                                                (keep (partial get active))))
                                    active? (get active coord)]
                                (when (or (and active?
                                               (case neighbour-count
                                                 (2 3) true
                                                 false))
                                          (and (not active?)
                                               (= 3 neighbour-count)))
                                  [coord true]))))
                      (filter identity))))))

(defn generation [cubes]
  (-> cubes
      (update :min dec)
      (update :max inc)
      (toggle-actives)))

(defn generation4 [cubes]
  (-> cubes
      (update :min dec)
      (update :max inc)
      (toggle-actives4)))

(defn parse-input [in]
  (let [rows (mapv (comp vec seq) in)
        n (count rows)]
    {:min 0
     :max n
     :active (into {}
                   (for [y (range n)
                         x (range n)
                         :when (= \# (get-in rows [y x]))]
                     [[y x 0] true]))}))

(defn parse-input4 [in]
  (let [rows (mapv (comp vec seq) in)
        n (count rows)]
    {:min 0
     :max n
     :active (into {}
                   (for [y (range n)
                         x (range n)
                         :when (= \# (get-in rows [y x]))]
                     [[y x 0 0] true]))}))

(defn part-1-solver [in]
  (->> (parse-input in)
       (iterate generation)
       (drop 6)
       first
       :active
       count))

(defn part-2-solver [in]
  (->> (parse-input4 in)
       (iterate generation4)
       (drop 6)
       first
       :active
       count))

(t/deftest part-1-test
  (t/is (= 112 (part-1-solver reference-input)))
  (t/is (= 271 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 848 (time (part-2-solver reference-input))))
  (t/is (= 2064 (time (part-2-solver input)))))
