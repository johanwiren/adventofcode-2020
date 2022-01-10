(ns adventofcode-2021.day-20
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))

(def input (->> "2021/day_20.txt"
                (io/resource)
                (io/reader)
                (line-seq)))

(defn bools [hashdots]
  (into (vector-of :boolean) (map (fn [x] (= x \#))) hashdots))

(defn parse-input [input]
  (let [lookup (first input)
        image (drop 2 input)]
    {:lookup (bools lookup)
     :image (mapv bools image)}))

(defn grow [image padding]
  (let [x (count (first image))
        empty-row (into (vector-of :boolean) (repeat (+ x 2) padding))]
    (into [empty-row]
          (-> (mapv (fn [row]
                      (into (vector-of :boolean padding) (conj row padding)))
                    image)
              (conj empty-row)))))

(defn pad [image]
  (grow image (ffirst image)))

(defn to-int [bits]
  (->> (reverse bits)
       (map-indexed (fn [i bit]
                      (* (short (Math/pow 2 i)) (if bit 1 0))))
       (reduce +)))

(defn lookup-bits [image [y x]]
  (if (and (< 0 y (dec (count image)))
           (< 0 x (dec (count (first image)))))
    (let [rows (subvec image (dec y) (+ 2 y))]
      (mapcat (fn [row] (subvec row (dec x) (+ 2 x))) rows))
    (into [] (repeat 9 (ffirst image)))))

(defn enhance [{:keys [lookup image]}]
  (let [padded-image (pad image)]
    {:lookup lookup
     :image (reduce (fn [image' pos]
                      (assoc-in image' pos (-> padded-image
                                               (lookup-bits pos)
                                               (to-int)
                                               (lookup))))
                    padded-image
                    (for [y (range (count padded-image))
                          x (range (count (first padded-image)))]
                      [y x]))}))

(defn part-1-solver [input]
  (->> (update (parse-input input) :image grow false)
       (iterate enhance)
       (drop 2)
       (first)
       (:image)
       (mapcat identity)
       (filter identity)
       (count)))

(defn part-2-solver [input]
  (->> (update (parse-input input) :image grow false)
       (iterate enhance)
       (drop 50)
       (first)
       (:image)
       (mapcat identity)
       (filter identity)
       (count)))

(t/deftest part-1-test
  (t/is (= 5765 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 18509 (part-2-solver input))))
