(ns adventofcode-2021.day-11
  (:require [clojure.test :as t]
            [clojure.java.io :as io]))

(def input (->> "2021/day_11.txt"
                (io/resource)
                (io/reader)
                (line-seq)))

(defn parse-line [line]
  (into (vector-of :int) (map #(Integer/parseInt %)) (re-seq #"\d" line)))

(defn parse-input [input]
  (mapv parse-line input))

(defn increase-energy
  ([squids]
   (mapv (partial mapv inc) squids))
  ([squids point]
   (update-in squids point inc)))

(defn high-energy-points [squids]
  (for [x (range (count squids))
        y (range (count squids))
        :when (<= 10 (get-in squids [x y]))]
    [x y]))

(defn neighbours [squids [x y :as point]]
  (let [max-x (dec (count squids))
        max-y (dec (count (first squids)))]
    (for [x (range (dec x) (+ 2 x))
          y (range (dec y) (+ 2 y))
          :when (and (nat-int? x)
                     (nat-int? y)
                     (not= point [x y])
                     (<= x max-x)
                     (<= y max-y))]
      [x y])))

(defn reset [squids]
  (mapv (partial mapv
                 (fn [x] (if (<= 10 x) 0 x)))
        squids))

(defn flash [squids]
  (loop [squids squids
         flashed #{}]
    (let [flashing-points (remove flashed (high-energy-points squids))
          to-highlight (into []
                             (comp (mapcat (partial neighbours squids))
                                   (remove flashed))
                             flashing-points)]
      (if-not (seq to-highlight)
        {:flashed flashed
         :squids (reset squids)}
        (let [squids
              (reduce increase-energy
                      squids
                      to-highlight)]
          (recur squids (into flashed flashing-points)))))))

(defn step [{:keys [step squids total-flashed]}]
  (let [{:keys [squids flashed]} (-> squids
                                     increase-energy
                                     flash)]
    {:squids squids
     :step (inc step)
     :flashed flashed
     :total-flashed (+ total-flashed (count (filter zero? (flatten squids))))}))

(defn init [squids]
  {:total-flashed 0
   :step 0
   :squids squids})

(defn part-1-solver [input]
  (->> (parse-input input)
       (init)
       (iterate step)
       (drop 100)
       (first)
       (:total-flashed)))

(defn part-2-solver [input]
  (->> (parse-input input)
       (init)
       (iterate step)
       (drop-while (comp (partial some pos?) flatten :squids))
       (first)
       (:step)))

(t/deftest part-1
  (t/is (= 1732 (time (part-1-solver input)))))

(t/deftest part-2
  (t/is (= 290 (time (part-2-solver input)))))
