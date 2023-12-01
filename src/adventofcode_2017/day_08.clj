(ns adventofcode-2017.day-08
  (:require [clojure.string :as str]
            [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (let [[reg op val _ guard-reg guard-op guard-val] (str/split line #" ")]
    {:reg (keyword reg)
     :op (case op "inc" + "dec" -)
     :val (parse-long val)
     :guard-reg (keyword guard-reg)
     :guard-op (case guard-op
                 "!=" not=
                 (resolve (symbol guard-op)))
     :guard-val (parse-long guard-val)}))

(defn solver [input]
  (let [{:keys [max-val] :as end-state}
        (->> input
             (map parse-line)
             (reduce (fn [cpu {:keys [reg op val guard-reg guard-op guard-val]}]
                       (if (guard-op (guard-reg cpu 0) guard-val)
                         (let [new-val ((fnil op 0) (reg cpu) val)]
                           (-> cpu
                               (assoc reg new-val)
                               (update :max-val (fnil max 0) new-val)))
                         cpu))
                     {}))]
    {:part-2 max-val
     :part-1 (-> end-state
                 (dissoc :max-val)
                 (vals)
                 (sort)
                 (last))}))

(defn part-1-solver [input]
  (:part-1 (solver input)))

(defn part-1-solver [input]
  (:part-2 (solver input)))
