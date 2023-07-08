(ns adventofcode-2016.day13
  (:require [adventofcode-2021.day-15 :as algo]))

(def input 1362)

(defn wall? [fav-num [x y]]
  (->> (+ (* x x)
          (* 3 x)
          (* 2 x y)
          y
          (* y y)
          fav-num)
       Long/toBinaryString
       (filter #(= \1 %))
       (count)
       (odd?)))

(defn neighbours [[x y]]
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])

(defn find-path
  ([goal fav-num]
   (find-path fav-num (comp #{goal} :pos) (comp :moves :item)))
  ([fav-num break-fn return-fn]
   (let [wall? (partial wall? fav-num)]
     (loop [q (into clojure.lang.PersistentQueue/EMPTY [{:pos [1 1]
                                                         :moves 0}])
            seen #{[1 1]}]
       (when-let [{:keys [pos moves] :as item} (peek q)]
         (if (break-fn item)
           (return-fn {:item item :seen seen})
           (let [neighs (into []
                              (comp (filter (partial every? nat-int?))
                                    (remove wall?)
                                    (remove seen))
                              (neighbours pos))]
             (recur (into (pop q) (map (fn [n] {:moves (inc moves) :pos n}) neighs))
                    (into seen neighs)))))))))


(defn part-1-solver [input]
  (find-path [31 39] input))

(defn part-2-solver [input]
  (find-path input (comp #{50} :moves) (comp count :seen)))
