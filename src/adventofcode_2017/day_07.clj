(ns adventofcode-2017.day-07
  (:require [clojure.string :as str]
            [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (let [[name val children]
        (rest (re-matches #"(\w+) \((\d+)\)(?: -> (.*))?" line))]
    {(keyword name) {:weight (parse-long val)
                     :children (->> (str/split (str children) #", ")
                                    (remove empty?)
                                    (mapv keyword))}}))

(defn parse-input [input]
  (into {} (map parse-line input)))

(defn part-1-solver [input]
  (let [programs (->> input
                      (parse-input))
        children (mapcat :children (vals programs))]
    (->> children
         (reduce (fn [acc item]
                   (dissoc acc item))
                 programs)
         (keys)
         (first))))

(defn get-total-weight [programs {:keys [weight children]}]
  (+ weight (->> children
                 (map programs)
                 (map (partial get-total-weight programs))
                 (reduce +))))

(defn balanced-weight [programs {:keys [children]} & [result]]
  (let [[[total-weight [program-key]] [new-tgt-weight]]
        (->> children
             (group-by (comp (partial get-total-weight programs)
                             programs))
             (sort-by (comp count second)))

        {:keys [weight] :as unbalanced-program}
        (program-key programs)

        children-weight (- total-weight weight)]

    (if new-tgt-weight
      (balanced-weight programs unbalanced-program (- new-tgt-weight children-weight))
      result)))

(defn part-2-solver [input]
  (let [programs (->> input
                      (parse-input))
        start (part-1-solver input)]
    (balanced-weight programs (start programs))))
