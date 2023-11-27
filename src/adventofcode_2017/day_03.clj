(ns adventofcode-2017.day-03
  (:require [clojure.math :as math]
            [clojure.test :as t]))

(def input 289326)

(defn solve [n]
  (let [[square max-n]
        (->> (range 1 Integer/MAX_VALUE 2)
             (map (partial #(* % %)))
             (map-indexed vector)
             (drop-while (fn [[_ max]] (< max n)))
             (first))]
    [square max-n]
    (+ square
       (- square
          (mod (- max-n n)
               square)))))

(defn part-1-solver [input]
  (solve input))

(defn square [n]
  (let [square (int (math/ceil (math/sqrt n)))]
    (cond-> square
      (even? square) inc)))

(defn xy [n]
  (if (= 1 n)
    [0 0]
    (let [square (square n)
          seg (quot (- n (* square square)) (dec square))
          seg->centre (quot (dec square) 2)
          pt->centre (- n
                        (+ (* square square)
                           (* (dec seg) (dec square)))
                        (quot (dec square) 2))]
      (case seg
        -3 [seg->centre (- pt->centre)]
        -2 [(- pt->centre) (- seg->centre)]
        -1 [(- seg->centre) pt->centre]
        -0 [pt->centre seg->centre]))))

(defn neighbours [[x y]]
  (for [x' (range (dec x) (+ 2 x))
        y' (range (dec y) (+ 2 y))
        :when (not= [x y] [x' y'])]
    [x' y']))

(defn part-2-solver [input]
  (reduce (fn [acc n]
            (let [xy (xy n)
                  neighbours (neighbours xy)
                  val (->> neighbours
                           (map #(get acc % 0))
                           (reduce +))]
              (if (< input val)
                (reduced val)
                (assoc acc xy val))))
          {[0 0] 1}
          (range 2 Integer/MAX_VALUE)))
