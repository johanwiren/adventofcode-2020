(ns adventofcode-2017.day-12
  (:require [clojure.string :as str]
            [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (let [[id & connections] (map parse-long (re-seq #"\d+" line))]
    [id connections]))

(defn parse-input [input]
  (into {} (map parse-line input)))

(defn children [g seen v]
  (let [seen (conj seen v)]
    (into seen
          (->> (g v)
               (remove seen)
               (mapcat (partial children g seen))))))

(defn part-1-solver [input]
  (-> input
      (parse-input)
      (children #{} 0)
      (count)))

(defn part-2-solver [input]
  (let [g (parse-input input)]
    (->> g
         (iterate (fn [g]
                    (apply (partial dissoc g) (children g #{} (ffirst g)))))
         (take-while seq)
         (count))))
