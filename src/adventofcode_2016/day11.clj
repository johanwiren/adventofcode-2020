(ns adventofcode-2016.day11
  (:require [utils :as u]
            [clojure.set :as set])
  (:import (java.util PriorityQueue)))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (->> line
       (re-seq #"a (\w+)(?:-\w+)? (\w+)(?:-\w+)?")
       (map (comp (partial apply keyword) rest))
       (set)))

(defn parse-input [input]
  (->> input
       (mapv parse-line)))

(defn valid-floor? [floor]
  (->> (group-by namespace floor)
       (filter (comp #{1} count second))
       (mapcat second)
       (map name)
       (set)
       (not= #{"generator" "microchip"})))

(defn done? [{:keys [floors]}]
  (every? empty? (butlast floors)))

(defn mk-seen [state]
  (select-keys state [:elevator :floors]))

(defn score [state]
  (->> (:floors state)
       (map-indexed (fn [i f]
                      (* (inc i) (count f))))
       (reduce +)))

(defn find-route [state]
  (->> (iterate (fn [{:keys [q seen]}]
                  (let [{:keys [elevator floors] :as state} (.poll q)
                        new-states (for [item-1 (get floors elevator)
                                         item-2 (get floors elevator)
                                         next-floor [(max 0 (dec elevator)) (min 3 (inc elevator))]
                                         :let [in-elevator (set [item-1 item-2])
                                               source-floor (set/difference (get floors elevator) in-elevator)
                                               destination-floor (into (get floors next-floor) in-elevator)]
                                         :when (and (valid-floor? source-floor)
                                                    (valid-floor? in-elevator)
                                                    (valid-floor? destination-floor))]
                                     (-> state
                                         (update :moves inc)
                                         (assoc :elevator next-floor)
                                         (assoc-in [:floors elevator] source-floor)
                                         (assoc-in [:floors next-floor] destination-floor)))]
                    (doseq [state (remove (comp seen mk-seen) new-states)]
                      (.add q state))
                    {:q q
                     :seen (into seen (map mk-seen new-states))}))
                (let [q (PriorityQueue. 1 (fn [x y]
                                            (- (score y)
                                               (score x))))]
                  (.add q (assoc state :moves 0))
                  {:q q
                   :seen #{}}))
       (filter (fn [s] (done? (.peek (:q s)))))
       (first)
       (:q)
       (.peek)))

(defn part-1-solver [input]
  (->> (parse-input input)
       (assoc {:elevator 0} :floors)
       (find-route)
       (:moves)))

(defn part-2-solver [input]
  (->> (update (parse-input input)
               0
               into
               [:elerium/generator :elerium/microchip :dilithium/generator :dilithium/microchip])
       (assoc {:elevator 0} :floors)
       (find-route)
       (:moves)))
