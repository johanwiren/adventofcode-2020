(ns adventofcode-2021.day-19
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]))

(def input (->> "2021/day_19.txt"
                    (io/resource)
                    (io/reader)
                    (line-seq)))

(defn parse-input [input]
  (->> input
       (partition-by #{""})
       (remove #{[""]})
       (map (fn [[scanner & beacons]]
              {:name (Integer/parseInt (first (re-seq #"\d+" scanner)))
               :beacons (mapv (fn [beacon]
                                (->> (str/split beacon #",")
                                     (mapv edn/read-string)))
                              beacons)}))))

(defn pairs [vec]
  (set (for [x (range (count vec))
             y (range (count vec))
             :when (not= x y)]
         (set [(vec x) (vec y)]))))

(def rotations [(fn [x y z] [x y z])     ;; identity
                (fn [x y z] [x (- z) y]) ;; x-cw
                (fn [x y z] [x z (- y)]) ;; x-ccw
                (fn [x y z] [z y (- x)]) ;; y-cw
                (fn [x y z] [(- z) y x]) ;; y-ccw
                (fn [x y z] [y (- x) z]) ;; z-cw
                (fn [x y z] [(- y) x z]) ;; z-ccw
                (fn [x y z] [x (- y) (- z)]) ;; x-180
                (fn [x y z] [(- x) y (- z)]) ;; y-180
                (fn [x y z] [(- x) (- y) z]) ;; z-180
                (fn [x y z] [y x (- z)])         ;; edge-xy-1
                (fn [x y z] [(- y) (- x) (- z)]) ;; edge-xy-2
                (fn [x y z] [z (- y) x])         ;; edge-zx-1
                (fn [x y z] [(- z) (- y) (- x)]) ;; edge-zx-2
                (fn [x y z] [(- x) z y])         ;; edge-yz-1
                (fn [x y z] [(- x) (- z) (- y)]) ;; edge-yz-2
                (fn [x y z] [z x y])         ;; corner min,min,min max,max,max 1
                (fn [x y z] [y z x])         ;; corner min,min,min max,max,max 2
                (fn [x y z] [(- y) (- z) x]) ;; corner min,max,min max,min,max 1
                (fn [x y z] [z (- x) (- y)]) ;; corner min,max,min max,min,max 2
                (fn [x y z] [(- z) (- x) y]) ;; corner max,min,min min,max,max 1
                (fn [x y z] [(- y) z (- x)]) ;; corner max,min,min min,max,max 2
                (fn [x y z] [y (- z) (- x)]) ;; corner max,max,min min,min,max 1
                (fn [x y z] [(- z) x (- y)]) ;; corner max,max,min min,min,max 2
                ])

(defn distance [[p1 p2]]
  (->> (map - p1 p2)
       (map #(Math/pow % 2))
       (apply +)
       (Math/sqrt)))

(defn beacon-pairs [scan1 scan2]
  (let [s1beacons (:beacons scan1)
        s2beacons (:beacons scan2)]
    (for [s1 (range (count s1beacons))
          s2 (range (count s2beacons))]
      [(get s1beacons s1) (get s2beacons s2)])))

(defn rotate [rot beacons]
  (mapv (partial apply rot) beacons))

(defn align-em [scan1 scan2]
  (reduce (fn [acc rot]
            (let [aligned (update scan2 :beacons (partial rotate rot))
                  pairs (beacon-pairs scan1 aligned)
                  alignment-pair (->> pairs
                                      (group-by distance)
                                      (vals)
                                      (filter (comp (partial <= 12) count))
                                      (ffirst))]
              (if alignment-pair
                (reduced (assoc aligned
                                :alignment-pair alignment-pair
                                :offset (mapv + (:offset scan1) (apply (partial mapv -) alignment-pair))))
                acc)))
          nil
          rotations))

(defn transform [{:keys [offset] :as scan}]
  (update scan :beacons (partial mapv (partial mapv + offset))))

(defn align-em-all [input]
  (let [[[scan0] scans] (split-at 1 input)]
    (loop [universe {0 (assoc scan0 :offset [0 0 0])}
           [scan & more] scans]
      (if scan
        (do
          (println "Aligning" (:name scan) (keys universe))
          (if-let [aligned (->> (vals universe)
                                (keep (fn [ref]
                                        (align-em ref scan)))
                                first)]
            (recur (assoc universe (:name scan) aligned) more)
            (do
              (println "Deferring" (:name scan))
              (recur universe (conj (vec more) scan)))))
        (vals universe)))))

(defn solver [input]
  (let [universe (align-em-all (parse-input input))
        part1 (->> universe
                   (map transform)
                   (mapcat :beacons)
                   (set)
                   (count))
        part2 (->> universe
                   (mapv :offset)
                   (pairs)
                   (map (partial apply (partial mapv -)))
                   (map (partial apply +))
                   (sort)
                   (last)) ]
    {:part1 part1
     :part2 part2}))

(t/deftest solver-test
  (t/is (= {:part1 462, :part2 12158} (solver input))))
