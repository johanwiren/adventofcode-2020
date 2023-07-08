(ns adventofcode-2016.day24
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-input [input]
  (let [cols (count (first input))
        chart (into [] (mapcat seq) input)
        digit? (into #{} (map (comp first str) (range 10)))
        to-coord (fn [n] [(mod n cols) (quot n cols)])
        numbers (->> chart
                     (map vector (range))
                     (filter (comp digit? second))
                     (map (juxt (comp parse-long str second) (comp to-coord first)))
                     (into (sorted-map)))]
    {:x-y->pos (fn [[x y]] (+ x (* y cols)))
     :chart chart
     :numbers numbers}))

(defn permutations [coll]
  (if (= 1 (count coll))
    (list coll)
    (for [head coll
          tail (permutations (disj (set coll) head))]
      (cons head tail))))

(defn neighbours [[x y]]
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])

(defn search [{:keys [chart x-y->pos]} start goal]
  (u/search {:q (into u/bfs-queue [start])
             :came-from {start :start}}
            (fn [{:keys [came-from q]}]
              (when (= goal (peek q))
                (->> (iterate came-from goal)
                     (take-while (complement #{start}))
                     (count))))
            (fn [{:keys [came-from q] :as state}]
              (let [current (peek q)
                    next (->> (neighbours current)
                              (remove came-from)
                              (remove (comp #{\#} chart x-y->pos)))]
                (-> state
                    (update :came-from (fn [came-from]
                                         (reduce (fn [came-from node]
                                                   (assoc came-from node current))
                                                 came-from
                                                 next)))
                    (update :q pop)
                    (update :q into next))))))

(defn pairs [vec]
  (set (for [x (range (count vec))
             y (range (count vec))
             :when (not= x y)]
         #{(vec x) (vec y)})))

(defn path-cost [costs-map path]
  (->> (partition 2 1 path)
       (map set)
       (map costs-map)))

(defn solver
  ([input]
   (solver input false))
  ([input back-to-start?]
   (let [{:keys [numbers] :as input} (parse-input input)
         costs-map (->> numbers
                        vals
                        vec
                        pairs
                        (map (juxt identity (partial apply search input)))
                        (into {}))
         start (numbers 0)]
     (->> numbers
          (drop 1)
          (map last)
          (permutations)
          (map (fn [path] (cond-> (vec (conj path start))
                            back-to-start? (conj start))))
          (map (partial path-cost costs-map))
          (map (partial reduce +))
          (sort <)
          (first)))))

(defn part-1-solver [input]
  (solver input))

(defn part-2-solver [input]
  (solver input true))
