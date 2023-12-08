(ns adventofcode-2022.day-24
  (:require [utils :as u]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn parse-input [input]
  (let [rows (->> input
                  (drop 1)
                  (butlast)
                  (mapv (fn [row]
                          (->> row
                               (drop 1)
                               (butlast)
                               vec))))
        n-rows (count rows)
        n-cols (count (first rows))
        lcm (u/lcm n-rows n-cols)
        blizzards (for [y (range n-rows)
                        x (range n-cols)
                        :let [blizz (get-in rows [y x])]
                        :when (not= \. blizz)]
                    [[y x] blizz])]
    {:max-y (dec n-rows)
     :max-x (dec n-cols)
     :blizzards blizzards
     :lcm lcm}))

(defn blizzard-locations [{:keys [blizzards lcm max-x max-y]}]
  (into []
        (comp (map (comp set (partial map first)))
              (take lcm))
        (iterate (fn [blizzards]
                   (map (fn [[[y x] direction]]
                          [(case direction
                             \> [y (mod (inc x) (inc max-x))]
                             \< [y (mod (dec x) (inc max-x))]
                             \v [(mod (inc y) (inc max-y)) x]
                             \^ [(mod (dec y) (inc max-y)) x])
                           direction])
                        blizzards))
                 blizzards)))

(defn neighbours [[x y]]
  [[(dec x) y] [(inc x) y] [x y] [x (dec y)] [x (inc y)]])

(defn fingerprint [{:keys [pos minute]}]
  (let [[y x] pos]
    (+ y
       (* 1000 x)
       (* 1000000 minute))))

(defn bfs [blizzard-locations blizzard-cycle max-x max-y minute root goal]
  (loop [q (into u/bfs-queue [{:pos root :minute minute}])
         seen #{}]
    (let [{:keys [pos minute]} (peek q)]
      (if (or (= pos goal) (nil? pos))
        minute
        (let [minute (inc minute)
              neighs (->> (neighbours pos)
                          (remove (fn [[y x :as pos]]
                                    (and (not= root pos)
                                         (not= goal pos)
                                         (or (some neg? pos)
                                             (< max-x x)
                                             (< max-y y)))))
                          (remove (get blizzard-locations (mod minute blizzard-cycle)))
                          (map (fn [pos] {:pos pos :minute minute}))
                          (remove (comp seen fingerprint)))]
          (recur (into (pop q) neighs)
                 (into seen (map fingerprint neighs))))))))

(defn part-1-solver [input]
  (let [{:keys [max-y max-x lcm] :as parsed} (parse-input input)
        blizzard-locations (blizzard-locations parsed)
        start [-1 0]
        goal [(inc max-y) max-x]]
    (bfs blizzard-locations lcm max-x max-y 0 start goal)))

(defn part-2-solver [input]
  (let [{:keys [max-y max-x lcm] :as parsed} (parse-input input)
        blizzard-locations (blizzard-locations parsed)
        start [-1 0]
        goal [(inc max-y) max-x]
        r1 (bfs blizzard-locations lcm max-x max-y 0 start goal)
        r2 (bfs blizzard-locations lcm max-x max-y r1 goal start)]
    (bfs blizzard-locations lcm max-x max-y r2 start goal)))

(t/deftest part-1-test
  (t/is (= 232 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 715 (time (part-2-solver input)))))
