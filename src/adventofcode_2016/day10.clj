(ns adventofcode-2016.day10
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (let [[_ bot low-type low high-type high] (re-matches #"bot (\d+) gives low to (\w+) (\d+) and high to (\w+) (\d+)" line)
        [_ value to] (re-matches #"value (\d+) goes to bot (\d+)" line)]
    (if bot
      {:bot (parse-long bot)
       :give {:low [(keyword low-type) (parse-long low)]
              :high [(keyword high-type) (parse-long high)]}
       :input []}
      {:value (parse-long value)
       :to (parse-long to)})))

(defn parse-input [input]
  (map parse-line input))

(defn give [bots {:keys [value to]}]
  (update-in bots [to :input] conj value))

(defn give-from-bots [{:keys [bot] :as state} bot-id]
  (let [{:keys [input give]} (get bot bot-id)]
    (if (= 2 (count input))
      (reduce (fn [state [low-high value]]
                (-> state
                    (assoc-in [:bot bot-id :input] [])
                    (update-in (conj (get give low-high) :input) conj value)
                    (cond->
                      (= #{61 17} (set input))
                      (assoc :magic-bot bot-id)

                      (= :bot (first (get give low-high)))
                      (give-from-bots (second (get give low-high))))))
              state
              (map vector [:low :high] (sort input)))
      state)))

(defn solver [input]
  (let [{:keys [bot value]}
        (->> (parse-input input)
             (group-by (comp (partial some #{:bot :value}) keys)))
        bots (into (sorted-map) (map (juxt :bot identity) bot))]
    (give-from-bots {:bot (reduce give bots value)} 12)))

(defn part-1-solver [input]
  (-> (solver input)
      (:magic-bot)))

(defn part-2-solver [input]
  (let [outputs (-> (solver input)
                    (:output)
                    (select-keys [0 1 2]))]
    (->> outputs
         (vals)
         (mapcat :input)
         (reduce *))))
