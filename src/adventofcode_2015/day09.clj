(ns adventofcode-2015.day09
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(def ref-input ["London to Dublin = 464"
                "London to Belfast = 518"
                "Dublin to Belfast = 141"])

(def input (line-seq (io/reader (io/resource "2015/day-09.txt"))))

(defn parse-line [input]
  (let [[_ from to dist] (re-matches #"(.*) to (.*) = (.*)" input)]
    {(set [from to]) (edn/read-string dist)}))

(defn parse-input [input]
  (into {} (map parse-line input)))

(defn permutations [coll]
  (if (= 1 (count coll))
    (list coll)
    (for [head coll
          tail (permutations (disj (set coll) head))]
      (cons head tail))))

(defn distance [distances path]
  (reduce (fn [{:keys [loc distance]} next]
            {:loc next
             :distance (+ distance (get distances (set [loc next])))})
          {:distance 0
           :loc (first path)}
          (rest path)))

(defn part-1-solver [input]
  (let [distances (parse-input input)
        locs (set (mapcat identity (keys distances)))
        paths (permutations locs)]
    (->> paths
         (map (partial distance distances))
         (sort-by :distance)
         (first)
         (:distance))))

(defn part-2-solver [input]
  (let [distances (parse-input input)
        locs (set (mapcat identity (keys distances)))
        paths (permutations locs)]
    (->> paths
         (map (partial distance distances))
         (sort-by :distance)
         (reverse)
         (first)
         (:distance))))


(comment
  (paths (parse-input input))

  (part-1-solver input)

  (part-2-solver input)

  (set (mapcat identity (keys (parse-input ref-input))))

  (get (parse-input ref-input) #{"Dublin" "Belfast"}))
