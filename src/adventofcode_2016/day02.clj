(ns adventofcode-2016.day02
  (:require [clojure.math :as math]
            [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (->> line
       (re-seq #"\w")
       (map {"U" [-1 0]
             "D" [1  0]
             "L" [0 -1]
             "R" [0  1]})))

(defn parse-input [input]
  (map parse-line input))


(defn move [pos step]
  (mapv (fn [a b]
          (min 2 (max 0 (+ a b))))
        pos
        step))

(def digit
  {[0 0] 1
   [0 1] 2
   [0 2] 3
   [1 0] 4
   [1 1] 5
   [1 2] 6
   [2 0] 7
   [2 1] 8
   [2 2] 9})


(defn part-1-solver [input]
  (->> (parse-input input)
       (reduce (fn [{:keys [pos code]} code-steps]
                 (let [new-pos (reduce move pos code-steps)]
                   {:pos new-pos
                    :code (str code (digit new-pos))}))
               {:pos [1 1]
                :code ""})
       (:code)))

(def digit-p2
  {[-2  0] 1
   [-1 -1] 2
   [-1  0] 3
   [-1  1] 4
   [ 0 -2] 5
   [ 0 -1] 6
   [ 0  0] 7
   [ 0  1] 8
   [ 0  2] 9
   [ 1 -1] "A"
   [ 1  0] "B"
   [ 1  1] "C"
   [ 2  0] "D"})

(defn move-p2 [pos step]
  (let [new-pos (mapv + pos step)]
    (if (digit-p2 new-pos)
      new-pos
      pos)))

(defn part-2-solver [input]
  (->> (parse-input input)
       (reduce (fn [{:keys [pos code]} code-steps]
                 (let [new-pos (reduce move-p2 pos code-steps)]
                   {:pos new-pos
                    :code (str code (digit-p2 new-pos))}))
               {:pos [0 -2]
                :code ""})
       (:code)))
