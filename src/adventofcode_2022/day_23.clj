(ns adventofcode-2022.day-23
  (:require [adventofcode-2022.utils :as u]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(def initial-proposals (apply concat (repeat [:N :S :W :E])))

(defn parse-input [input]
  (let [lines (mapv vec input)
        elves (set
               (for [y (range (count lines))
                     x (range (count (first lines)))
                     :when (= \# (get-in lines [y x]))]
                 [y x]))]
    {:proposals initial-proposals
     :elves elves
     :round 0}))

(defn neighbours [[y x]]
  (for [y' (range (dec y) (+ 2 y))
        x' (range (dec x) (+ 2 x))
        :when (not= [y x] [y' x'])]
    [y' x']))

(def proposal->move-fn
  {:N (fn [[y x]] [(dec y) x])
   :S (fn [[y x]] [(inc y) x])
   :E (fn [[y x]] [y (inc x)])
   :W (fn [[y x]] [y (dec x)])})

(def proposal->neigh-fn
  {:N (fn [[y x]] (for [x' (range (dec x) (+ 2 x))] [(dec y) x']))
   :S (fn [[y x]] (for [x' (range (dec x) (+ 2 x))] [(inc y) x']))
   :E (fn [[y x]] (for [y' (range (dec y) (+ 2 y))] [y' (inc x)]))
   :W (fn [[y x]] (for [y' (range (dec y) (+ 2 y))] [y' (dec x)]))})

(defn round [{:keys [elves proposals round]}]
  (let [by-neighs (group-by (comp (comp boolean
                                        (partial some elves))
                                  neighbours)
                            elves)
        to-move (get by-neighs true [])
        stays (set (get by-neighs false []))
        round-proposals (take 4 proposals)
        elf-proposals (group-by (fn [elf]
                                  (->> round-proposals
                                       (keep (fn [proposal]
                                               (let [move-fn  (proposal->move-fn proposal)
                                                     neigh-fn (proposal->neigh-fn proposal)]
                                                 (when (not-any? elves (neigh-fn elf))
                                                   (move-fn elf)))))
                                       (first)))
                                to-move)
        no-move-found (get elf-proposals nil)
        moved (into []
                    (comp (filter (comp #{1} count second))
                          (map first))
                    (dissoc elf-proposals nil))
        not-moved (into []
                        (comp (filter (comp (complement #{1}) count second))
                              (mapcat second))
                        (dissoc elf-proposals nil))]
    {:elves (into stays (concat no-move-found not-moved moved))
     :proposals (drop 1 proposals)
     :round (inc round)}))

(defn bbox [points]
  (reduce (fn [[mn mx] point]
            [(mapv min mn point)
             (mapv max mx point)])
          [(first points) (first points)]
          points))

(defn part-1-solver [input]
  (let [{:keys [elves]} (->> (parse-input input)
                             (iterate round)
                             (drop 10)
                             (first))
        [[min-y min-x] [max-y max-x]] (bbox elves)
        area (* (count (range min-y (inc max-y)))
                (count (range min-x (inc max-x))))]
    (- area (count elves))))

(defn part-2-solver [input]
  (->> (parse-input input)
       (iterate round)
       (partition 2)
       (drop-while (comp (partial apply not=)
                         (partial map :elves)))
       (first)
       (last)
       (:round)))

(t/deftest part-1-test
  (t/is (= 4082 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 1065 (time (part-2-solver input)))))

