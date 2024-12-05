(ns adventofcode-2024.day-05
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :as t]
   [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse [input]
  (let [[rules _ updates] (partition-by #{""} input)]
    {:rules (map #(map parse-long (str/split % #"\|")) rules)
     :updates (map #(mapv parse-long (str/split % #",")) updates)}))

(defn in-order? [rules update]
  (let [rules (update-vals rules #(set/intersection % (set update)))]
    (reduce (fn [updated page]
              (let [rule (get rules page)]
                (if (set/subset? rule updated)
                  (conj updated page)
                  (reduced false))))
            #{}
            update)))

(defn simple-rules [rules]
  (->> rules
       (map #(hash-map (second %) (hash-set (first %))))
       (apply merge-with set/union)))

(defn middle-element [vec]
  (get vec (quot (count vec) 2)))

(defn part-1-solver [input]
  (let [{:keys [rules updates]} (parse input)
        rules (simple-rules rules)]
    (->> updates
         (filter #(in-order? rules %))
         (map middle-element)
         (reduce +))))

(defn print-in-order [rules update]
  (let [rules (update-vals rules (partial filter (set update)))]
    (loop [printed []
           seen #{}
           q update]
      (if-let [page (peek q)]
        (let [deps (remove seen (get rules page))]
          (cond
            (seen page)
            (recur printed seen (pop q))

            (seq deps)
            (recur printed seen (into q deps))

            :else
            (recur (conj printed page) (conj seen page) (pop q))))
        printed))))

(defn part-2-solver [input]
  (let [{:keys [rules updates]} (parse input)
        rules (simple-rules rules)]
    (->> updates
         (remove #(in-order? rules %))
         (map #(print-in-order rules %))
         (map middle-element)
         (reduce +))))

(t/deftest part-1-test
  (t/is (= 3608 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 4922 (part-2-solver input))))
