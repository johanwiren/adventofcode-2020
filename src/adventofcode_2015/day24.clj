(ns adventofcode-2015.day24
  (:require [utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.edn :as edn]))

(def input (u/line-seq-input *ns*))

(defn parse-input [input]
  (map parse-long input))

(defn iterate-combos [q balanced-weight pkgs]
  (->> (iterate (fn [{:keys [q balanced seen]}]
                  (let [current (peek q)
                        new-combos (->> pkgs
                                        (remove current)
                                        (map (fn [pkg] (conj current pkg)))
                                        (remove seen)
                                        (remove (fn [combo] (< balanced-weight (reduce + combo)))))]
                    {:q (into (pop q) new-combos)
                     :balanced     (into balanced (filter (fn [combo]
                                                            (= balanced-weight (reduce + combo)))
                                                          new-combos))
                     :seen      (into seen new-combos)}))
                {:q (into q (map (comp set vector) (sort < pkgs)))
                 :balanced []
                 :seen #{}})
       (take-while (every-pred (fn [{:keys [q]}]
                                 (peek q))
                               (fn [{:keys [balanced]}]
                                 (->> (map count balanced)
                                      (frequencies)
                                      (keys)
                                      (count)
                                      (> 2)))))
       (mapcat :balanced)
       (distinct)))

(defn find-combos [balanced-weight pkgs]
  (iterate-combos clojure.lang.PersistentQueue/EMPTY balanced-weight pkgs))

(defn find-combo [balanced-weight pkgs]
  (first (iterate-combos [] balanced-weight pkgs)))

(defn solver [compartments pkgs]
  (let [pkgs (set pkgs)
        balanced-weight (/ (reduce + pkgs) compartments)]
    (->> (find-combos balanced-weight pkgs)
         (map (fn [a]
                (let [b (find-combo balanced-weight (set/difference pkgs a))
                      c (set/difference pkgs a b)]
                  [a b c])))
         (map (fn [[a _ _]]
                (reduce * a)))
         (sort <)
         (first))))

(defn part-1-solver [input]
  (solver 3 (parse-input input)))

(defn part-2-solver [input]
  (solver 4 (parse-input input)))
