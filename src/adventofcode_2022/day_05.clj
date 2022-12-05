(ns adventofcode-2022.day-05
  (:require [adventofcode-2022.utils :as u]
            [clojure.string :as str]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn transpose [coll]
  (apply (partial map vector) coll))

(defn parse-stack [stack]
  (->> stack
       (drop 1)
       (partition-all 4)
       (map first)
       (map str)
       (map keyword)))

(defn parse-move [move]
  (-> (apply hash-map (str/split move #" "))
      (update-keys keyword)
      (update-vals parse-long)
      (update :from dec)
      (update :to dec)))

(defn parse-input [input]
  (let [[stacks _ moves] (partition-by #{""} input)]
    {:stacks (->> stacks
                  butlast
                  (map parse-stack)
                  (transpose)
                  (mapv (partial remove (set [(keyword " ")]))))
     :moves  (map parse-move moves)}))

(defn mover [is-9001?]
  (let [take-fn (cond->> take
                  is-9001? (comp reverse))]
    (fn [stacks {:keys [move from to]}]
      (let [taken (take-fn move (get stacks from))]
        (-> stacks
            (update to into taken)
            (update from (partial drop move)))))))

(defn do-moves [{:keys [stacks moves mover]}]
  (reduce mover stacks moves))

(defn solve [puzzle]
  (->> (do-moves puzzle)
       (map first)
       (map name)
       (apply str)))

(defn part-1-solver [input]
  (-> input
      parse-input
      (assoc :mover (mover false))
      (solve)))

(defn part-2-solver [input]
  (-> input
      parse-input
      (assoc :mover (mover true))
      (solve)))

(t/deftest part-1-test
  (t/is (= "HBTMTBSDC" (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= "PQTJRSHWS" (time (part-2-solver input)))))
