(ns adventofcode-2015.day10
  (:require [clojure.string :as str]))

(defn describe [numstr]
  (loop [res ""
         numstr numstr]
    (if-not (seq numstr)
      res
      (let [n (subs numstr 0 1)
            cnt (count (take-while (set n) numstr))]
        (recur (str res cnt n) (subs numstr cnt))))))


(defn part-2-solver [numstr]
  (reduce (fn [acc item]
            (conj acc (str (count item)) (str (first item))))
          []
          (partition-by identity numstr)))

(comment

  (part-2-solver "1211")

  (->> (iterate part-2-solver "1321131112")
       (take 3)
       #_(first)
       #_(count))

  (->> (iterate part-2-solver "1")
       (drop 40)
       (first)
       (count))

  (->> (iterate part-2-solver "1321131112")
                     (drop 50)
                     (first)
                     (count))

  (int \a)

  (Integer/toString 25 25)

  (re-seq #"(.)$+" "aaaaaaaaabbppa")

  *1)

