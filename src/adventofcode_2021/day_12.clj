(ns adventofcode-2021.day-12
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.test :as t]))

(def input (->> "2021/day_12.txt"
                (io/resource)
                (io/reader)
                (line-seq)))

(defn parse-line [line]
  (str/split line #"-"))

(defn parse-input [input]
  (map parse-line input))

(defn mk-cave-map [lines]
  (->> lines
       (mapcat (fn [[k v]]
                 [{k (set (remove #{"start"} [v]))}
                  {v (set (remove #{"start"} [k]))}]))
       (apply (partial merge-with set/union))))

(def lower-case?
  (memoize
   (fn [s]
     (= s (str/lower-case s)))))

(defn walk
  ([cave-map]
   (walk cave-map true))
  ([cave-map single-pass]
   (walk cave-map single-pass #{} #{} [] "start"))
  ([cave-map single-pass paths visited path node]
   (let [path (conj path node)]
     (if (= "end" node)
       (conj paths path)
       (let [single-pass' (or single-pass
                             (visited node))
             visited' (if (lower-case? node)
                       (conj visited node)
                       visited)
             nodes (if single-pass'
                     (remove (set visited') (get cave-map node))
                     (get cave-map node))]
         (concat paths (mapcat (partial walk cave-map single-pass' paths visited' path) nodes)))))))

(defn part-1-solver [input]
  (-> (parse-input input)
      (mk-cave-map)
      (walk)
      (count)))

(defn part-2-solver [input]
  (-> (parse-input input)
      (mk-cave-map)
      (walk false)
      (count)))

(t/deftest part-1
  (t/is (= 3563 (part-1-solver input))))

(t/deftest part-2
  (t/is (= 105453 (part-2-solver input))))
