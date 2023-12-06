(ns adventofcode-2023.day-06
  (:require [clojure.math :as math]
            [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-input [input]
  (->> input
       (map (fn [line] (map parse-long (re-seq #"\d+" line))))
       (apply map (fn [t record] {:t t :record record}))))

(defn parse-input-v2 [input]
  (->> input
       (map (fn [line] (parse-long (apply str (re-seq #"\d+" line)))))
       (zipmap [:t :record])))

(defn solve [a b c]
  (let [discriminant (- (* b b) (* 4 a c))]
    (when (nat-int? discriminant)
      (let [x1 (/ (+ (- b) (math/sqrt discriminant))
                  (* 2 a))
            x2 (/ (- (- b) (math/sqrt discriminant))
                  (* 2 a))]
        [x1 x2]))))

(defn race [{:keys [t record]}]
  (apply - (map long (solve 1 (- t) (inc record)))))

(defn part-1-solver [input]
  (reduce * (map race (parse-input input))))

(defn part-2-solver [input]
  (race (parse-input-v2 input)))

