(ns adventofcode-2022.day-16
  (:require [adventofcode-2022.utils :as u]
            [clojure.string :as str]
            [clojure.test :as t]))

(set! *unchecked-math* false)

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (let [[_ valve rate valves]
        (re-matches #"Valve (.*) has flow rate=(.*); tunnels? leads? to valves? (.*)" line)
        kw-valves (map keyword (str/split valves #", "))]
    {:valve (keyword valve) :rate (parse-long rate) :valves kw-valves}))

(defn parse-input [input]
  (map parse-line input))

(defn bfs [root goal neigh-fn]
  (loop [q         (into clojure.lang.PersistentQueue/EMPTY [root])
         steps     0
         came-from {}
         seen      (set [])]
    (let [v (peek q)]
      (if (= goal v)
        (->> goal
             (iterate came-from)
             (drop 1)
             (take-while some?)
             (count))
        (let [neighs (remove seen (neigh-fn v))]
          (recur (into (pop q) neighs)
                 (inc steps)
                 (reduce (fn [came-from neigh]
                           (assoc came-from neigh v))
                         came-from
                         neighs)
                 (conj seen v)))))))

(defn add-neighbours [cave caves map']
  (assoc cave
         :valves
         (into [] (comp
                   (remove (comp #{:AA} :valve))
                   (remove (comp (set [(:valve cave)]) :valve))
                   (map (fn [goal]
                          (let [steps (inc (bfs (:valve cave) (:valve goal) map'))]
                            (-> goal
                                (select-keys [:valve :rate])
                                (assoc :steps steps))))))
               caves)))

(defn best-flow [caves root max-steps]
  (loop [q         (into clojure.lang.PersistentQueue/EMPTY [(-> (get caves root)
                                                                 (assoc :steps 0)
                                                                 (assoc :flow 0)
                                                                 (assoc :seen [root]))])
         best-node {:flow 0}]
    (let [{:keys [steps valve seen flow] :as v} (peek q)]
      (if (nil? v)
        best-node
        (let [neighs (->> (into []
                                (comp
                                 (remove (comp (set seen) :valve))
                                 (map #(assoc % :seen (conj seen (:valve %))))
                                 (map #(assoc % :flow (+ flow (* (:rate %)
                                                                 (- max-steps
                                                                    (+ steps
                                                                       (:steps %)))))))
                                 (map #(update % :steps + steps))
                                 (remove (comp (partial <= max-steps) :steps)))
                                (get-in caves [valve :valves]))
                          (seq))]
          (recur (into (pop q) neighs)
                 (if neighs
                   best-node
                   (if (< (:flow best-node) (:flow v))
                     v
                     best-node))))))))

(defn part-1-solver [input]
  (let [caves             (parse-input input)
        map'              (into {} (map (juxt :valve :valves) caves))
        interesting-caves (filter (some-fn (comp pos? :rate)
                                           (comp #{:AA} :valve))
                                  caves)
        with-neighbours   (into {}
                                (comp
                                 (map #(add-neighbours % interesting-caves map'))
                                 (map (juxt :valve identity)))
                                interesting-caves)]
    (:flow (best-flow with-neighbours :AA 30))))

(defn best-tandem-flow [caves root max-steps]
  (loop [q         (into [] [{:self root
                              :other root
                              :self-path [root]
                              :self-steps 0
                              :self-flow 0
                              :other-path [root]
                              :other-steps 0
                              :other-flow 0
                              :seen #{root}}])
         seen-paths #{[root]}
         best-node {:self-flow 0
                    :other-flow 0}]
    (let [{:keys [self-steps self other seen other-steps] :as v} (peek q)]
      (if (nil? v)
        (select-keys best-node [:self :other :self-flow :other-flow :seen])
        (let [same-pos? (= self other)
              self-valves (get-in caves [self :valves])
              split-valves (split-at (quot (count self-valves) 2) self-valves)
              self-neighs (->> (into []
                                     (comp
                                      (remove (comp seen :valve))
                                      (map (fn [valve]
                                             (-> v
                                                 (update :self-flow + (* (:rate valve)
                                                                         (- max-steps
                                                                            (+ self-steps
                                                                               (:steps valve)))))
                                                 (update :self-path conj (:valve valve))
                                                 (update :seen conj (:valve valve))
                                                 (update :self-steps + (:steps valve))
                                                 (assoc :self (:valve valve)))))
                                      (remove (comp (partial <= max-steps) :self-steps))
                                      (remove (comp seen-paths :self-path)))
                                     (if same-pos?
                                       (first split-valves)
                                       (get-in caves [self :valves])))
                               (seq))
              other-neighs (->> (into []
                                      (comp
                                       (remove (comp seen :valve))
                                       (map (fn [valve]
                                              (-> v
                                                  (update :other-flow + (* (:rate valve)
                                                                           (- max-steps
                                                                              (+ other-steps
                                                                                 (:steps valve)))))
                                                  (update :other-path conj (:valve valve))
                                                  (update :seen conj (:valve valve))
                                                  (update :other-steps + (:steps valve))
                                                  (assoc :other (:valve valve)))))
                                       (remove (comp (partial <= max-steps) :other-steps)))
                                      (if same-pos?
                                        (second split-valves)
                                        (get-in caves [other :valves])))
                                (seq))]
          (recur (into (pop q) (concat self-neighs other-neighs))
                 (into seen-paths (concat (map :self-path self-neighs) (map :other-path other-neighs)))
                 (if (or (seq self-neighs)
                         (seq other-neighs))
                   best-node
                   (if (< (+ (:self-flow best-node)
                             (:other-flow best-node))
                          (+ (:self-flow v)
                             (:other-flow v)))
                     v
                     best-node))))))))

(defn part-2-solver [input]
  (let [caves             (parse-input input)
        map'              (into {} (map (juxt :valve :valves) caves))
        interesting-caves (filter (some-fn (comp pos? :rate)
                                           (comp #{:AA} :valve))
                                  caves)
        with-neighbours   (into {}
                                (comp
                                 (map #(add-neighbours % interesting-caves map'))
                                 (map (juxt :valve identity)))
                                interesting-caves)

        {:keys [self-flow other-flow]} (best-tandem-flow with-neighbours :AA 26)]
    (+ self-flow other-flow)))

(t/deftest part-1-test
  (t/is (= 1862 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 2422 (time (part-2-solver input)))))
