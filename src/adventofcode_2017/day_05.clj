(ns adventofcode-2017.day-05
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-input [input]
  (mapv parse-long input))

(defn part-1-solver [input]
  (->> (iterate (fn [[pos xs]]
                  (if-let [offset (get xs pos)]
                    [(+ pos offset) (update xs pos inc)]
                    [:invalid xs]))
                [0 (parse-input input)])
       (map first)
       (take-while int?)
       (count)
       (dec)))

(defn part-2-solver [input]
  (let [xs (int-array (parse-input input))]
    (->> (iterate (fn [pos]
                    (if (< pos (alength xs))
                      (let [offset (aget xs pos)]
                        (aset xs pos (if (<= 3 offset)
                                       (dec offset)
                                       (inc offset)))
                        (+ pos offset))
                      :invalid))
                  0)
         (take-while int?)
         (count)
         (dec))))
