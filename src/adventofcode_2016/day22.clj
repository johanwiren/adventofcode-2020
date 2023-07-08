(ns adventofcode-2016.day22
  (:require [utils :as u]
            [clojure.string :as str]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (let [[x y & ns] (map parse-long (re-seq #"\d+" line))]
    (into [[x y]] ns)))

(defn parse-input [input]
  (let [keys (str/split (nth input 1) #" +")
        lines (map parse-line (drop 2 input))]
    (map (partial zipmap (map keyword keys)) lines)))

(defn moves [fs-infos]
  (for [fs1 fs-infos
        fs2 fs-infos
        :let [{:keys [Used Size]} fs1]
        :when (and (not= fs1 fs2)
                   (pos? Used)
                   (<= Used (:Avail fs2)))]
    [(assoc fs1 :Avail Size :Used 0)
     (-> fs2 (update :Used + Size) (update :Avail - Size))]))

(defn part-1-solver [input]
  (count (let [fs-infos (parse-input input)]
           (moves fs-infos))))

(defn neighbours [[x y]]
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])

(defn from-free-to-storage [filesystems {:keys [Avail] :as free-node} goal-pos]
  (u/search {:q (into u/bfs-queue [(:Filesystem free-node)])
             :came-from {(:Filesystem free-node) :start}}
            (fn [state]
              (when (= goal-pos (-> state :q peek))
                (:came-from state)))
            (fn [{:keys [came-from q] :as state}]
              (let [path (peek q)
                    {:keys [Filesystem]} (filesystems path)
                    next (->> (neighbours Filesystem)
                              (keep filesystems)
                              (filter (fn [tgt] (<= (:Used tgt) Avail)))
                              (map :Filesystem)
                              (remove came-from))]
                (-> state
                    (update :came-from (fn [came-from]
                                         (reduce (fn [came-from node]
                                                   (assoc came-from node Filesystem))
                                                 came-from
                                                 next)))
                    (update :q pop)
                    (update :q into next))))))

(defn part-2-solver [input]
  (let [[max-x filesystems]
        (->> (parse-input input)
             (reduce (fn [[mx fss] fs]
                       (let [[x _ :as pos] (:Filesystem fs)]
                         [(max mx x) (assoc fss pos fs)]))
                     [0 {}]))
        free-node (first (sort-by :Avail > (vals filesystems)))
        storage-node [(dec max-x) 0]
        came-from (from-free-to-storage filesystems free-node storage-node)
        steps-to-align-free (->> (iterate came-from storage-node)
                                 (take-while (complement #{:start}))
                                 count)]
    (+ steps-to-align-free
       (* 5 (dec max-x)))))
