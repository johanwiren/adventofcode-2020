(ns adventofcode-2015.day18
  (:require [utils :as u]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn parse-input [input]
  (mapv (fn [line]
          (mapv (partial = \#)
                line))
        input))

(defn neighbours [[x y]]
  (for [x' (range (max 0 (dec x)) (+ 2 x))
        y' (range (max 0 (dec y)) (+ 2 y))
        :when (not= [x' y'] [x y])]
    [x' y']))

(defn generation
  ([lights]
   (generation lights (constantly false)))
  ([lights p2-pred]
   (let [coords (for [x (range (count lights))
                      y (range (count (first lights)))
                      :when (not (p2-pred [x y]))]
                  [x y])]
     (reduce (fn [new-lights coord]
               (let [n-lit (->> (neighbours coord)
                                (filter (partial get-in lights))
                                (count))
                     was-lit? (get-in lights coord)
                     lit? (or (and was-lit?
                                   (<= 2 n-lit 3))
                              (and (not was-lit?)
                                   (= 3 n-lit)))]
                 (assoc-in new-lights coord lit?)))
             lights
             coords))))

(defn part-1-solver [input]
  (->> (parse-input input)
       (iterate generation)
       (drop 100)
       (first)
       (flatten)
       (filter true?)
       (count)))

(defn part-2-solver [input]
  (let [lights (parse-input input)
        x (dec (count lights))
        y (dec (count (first lights)))
        stuck-lights (-> lights
                         (assoc-in [0 0] true)
                         (assoc-in [x 0] true)
                         (assoc-in [0 y] true)
                         (assoc-in [x y] true))]
    (->> stuck-lights
         (iterate #(generation % #{[0 0] [x 0] [0 y] [x y]}))
         (drop 100)
         (first)
         (flatten)
         (filter true?)
         (count))))
