(ns adventofcode-2023.day-24
  (:require [utils :as u]
            [adventofcode-2022.day-15 :as lin]
            [clojure.math :as math]))

(def input (u/line-seq-input *ns*))

(def input ["19, 13, 30 @-2,  1, -2"
            "18, 19, 22 @-1, -1, -2"
            "20, 25, 34 @-2, -2, -4"
            "12, 31, 28 @-1, -2, -1"
            "20, 19, 15 @1, -5, -3"])

(defn parse-line [line]
  (let [[x y z a b c] (map parse-long (re-seq #"[-\d]+" line))]
    [[x y z] [a b c]]))

(defn line [[pos velocity]]
  [pos (mapv + pos velocity)])

(defn hailstone-2d [[pos velocity]]
  [(pop pos) (pop velocity)])

(defn intersecting-hailstones [hailstones]
  (->> (u/pairs hailstones)
       (filter (fn [[h1 h2]]
                 (let [[h1-pos h1-velocity :as h1] (hailstone-2d h1)
                       [h2-pos h2-velocity :as h2] (hailstone-2d h2)]
                   (when-let [[x y :as intersect] (lin/intersect (lin/line-eqn (line h1))
                                                                 (lin/line-eqn (line h2)))]
                     (let [h1-diff (mapv - intersect h1-pos)
                           h2-diff (mapv - intersect h2-pos)]
                       (and
                        (<= 200000000000000 x 400000000000000)
                        (<= 200000000000000 y 400000000000000)
                        (= (map math/signum h1-diff) (map math/signum h1-velocity))
                        (= (map math/signum h2-diff) (map math/signum h2-velocity))))))))
       (apply concat)))

(defn part-1-solver [input]
  (let [lines (->> (map parse-line input)
                   (map hailstone-2d))]
    (->> (u/pairs lines)
         (filter (fn [[[h1-pos h1-velocity :as h1]
                       [h2-pos h2-velocity :as h2]]]
                   (when-let [[x y :as intersect] (lin/intersect (lin/line-eqn (line h1))
                                                                 (lin/line-eqn (line h2)))]
                     (let [h1-diff (mapv - intersect h1-pos)
                           h2-diff (mapv - intersect h2-pos)]
                       (and
                        (<= 200000000000000 x 400000000000000)
                        (<= 200000000000000 y 400000000000000)
                        (= (map math/signum h1-diff) (map math/signum h1-velocity))
                        (= (map math/signum h2-diff) (map math/signum h2-velocity)))))))
         (count))))

(comment
  (let [intersecting-hailstones (intersecting-hailstones (map parse-line input))]
    intersecting-hailstones)

  (->> (u/pairs (map parse-line input))
       (keep (fn [[h1 h2]]
                 (let [[h1-pos h1-velocity :as h1] (hailstone-2d h1)
                       [h2-pos h2-velocity :as h2] (hailstone-2d h2)]
                   (when-let [[x y :as intersect] (lin/intersect (lin/line-eqn (line h1))
                                                                 (lin/line-eqn (line h2)))]
                     (let [h1-diff (mapv - intersect h1-pos)
                           h2-diff (mapv - intersect h2-pos)]
                       (and
                        (<= 200000000000000 x 400000000000000)
                        (<= 200000000000000 y 400000000000000)
                        (= (map math/signum h1-diff) (map math/signum h1-velocity))
                        (= (map math/signum h2-diff) (map math/signum h2-velocity))
                        intersect))))))
       (filter identity)
       (sort)
       #_(apply concat))

  #__)

;; 19 + -2t - 18 + 1s = 0
;; 13 + 1t - 19 + 1s = 0
;; 30 + -2t - 22 + 2s = 0

;; -2t + 1s = -1
;;  1t + 1s = 6
;; -2t + 2s = -8

(= -8 (- (* 2 (/ 11 3))
         (* 2 (- 6 (/ 11 3)))))

;; -2 1 | -1
;;  1 1 |  6
;; -2 2 | -8

;;  1 1 |  6
;; -2 1 | -1

;;  1 1 |  6
;;  0 3 | 11

;;  1 1 |  6 =
;;  0 1 | 11/3 = s

;; 1t + 11/3 = 6 = 6 - 11/3

(- 6 (/ 11 3))


;; -2 2 | -8
(* -2 2)


;; 𝑎1+𝑡(𝑏1−𝑎1)=𝑐1+𝑠(𝑑1−𝑐1)
;; 𝑎1+𝑡(𝑏1−𝑎1)-c1=𝑠(𝑑1−𝑐1)
;; => 𝑎1+𝑡(𝑏1−𝑎1)-c1-𝑠(𝑑1−𝑐1)=0

;; 𝑎2+𝑡(𝑏2−𝑎2)=𝑐2+𝑠(𝑑2−𝑐2)
;; 𝑎2+𝑡(𝑏2−𝑎2)-c2=𝑠(𝑑2−𝑐2)
;; => 𝑎2+𝑡(𝑏2−𝑎2)-c2-s(d2-c2)=0

;; 𝑎3+𝑡(𝑏3−𝑎3)=𝑐3+𝑠(𝑑3−𝑐3)
;; 𝑎3+𝑡(𝑏3−𝑎3)-c3=𝑠(𝑑3−𝑐3)
;; => 𝑎3+𝑡(𝑏3−𝑎3)-c3-s(d3-c3)=0


(comment


  #__)
