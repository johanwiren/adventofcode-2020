(ns adventofcode-2023.day-08
  (:require [clojure.string :as str]
            [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-map [map']
  (let [[k & [l r]] (map keyword (re-seq #"\w+" map'))]
    [k {:L l :R r}]))

(defn parse-input [input]
  (let [[instrs _ & maps] input]
    {:instrs (map (comp keyword str) instrs)
     :maps (into {} (map parse-map maps))}))

(defn steps-for [{:keys [instrs maps]} node]
  (->> {:steps 0 :node node :instrs (apply concat (repeat instrs))}
       (iterate (fn [{:keys [node instrs steps]}]
                  (let [[instr & instrs] instrs]
                    {:node   (get-in maps [node instr])
                     :instrs instrs
                     :steps (inc steps)})))
       (filter (comp #(str/ends-with? % "Z") :node))
       (first)
       (:steps)))

(defn part-1-solver [input]
  (steps-for (parse-input input) :AAA))

(defn part-2-solver [input]
  (let [{:keys [maps] :as in} (parse-input input)]
    (->> (keys maps)
         (filter (comp #(str/ends-with? % "A") name))
         (map (partial steps-for in))
         (apply u/lcm))))
