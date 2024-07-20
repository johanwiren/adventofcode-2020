(ns adventofcode-2023.day-19
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-statement [statement]
  (let [[k-or-res fn val res] (re-seq #"\w+|\d+|[<>]" statement)]
    (if fn
      [(keyword k-or-res) (symbol fn) (parse-long val) (keyword res)]
      (keyword k-or-res))))

(defn parse-workflow [workflow]
  (let [[_ n rules] (re-matches #"(\w+)\{(.*)\}" workflow)
        stmts (str/split rules #",")
        parsed-stmts (mapv parse-statement stmts)]
    [(keyword n) parsed-stmts]))

(defn parse-rating [rating]
  (-> (apply hash-map (re-seq #"[xmas]|\d+" rating))
      (update-vals parse-long)
      (update-keys keyword)))

(defn parse-input [input]
  (let [[workflows _ ratings] (partition-by #{""} input)]
    [(into {} (map parse-workflow workflows)) (map parse-rating ratings)]))

(defn flatten-workflows [workflows]
  (let [kw (fn [k i]
             (keyword (str (name k) (when (pos? i) i))))]
    (into {} (mapcat (fn [[k v]]
                       (loop [i 0
                              res []
                              [check & more] v]
                         (if (keyword (first more))
                           (conj res [(kw k i) [check (first more)]])
                           (recur (inc i) (conj res [(kw k i) [check (kw k (inc i))]]) more))))
                     workflows))))

(defn accepted [ranges workflows k]
  (cond
    (some nil? (vals ranges)) []

    (= :R k) []

    (= :A k) ranges

    :else
    (let [wf (k workflows)
          [[k f val then] else] wf
          left (update ranges k (comp first u/range-disj) (if (= '< f)
                                                            [(dec val) 4000]
                                                            [1 (inc val)]))
          right (update ranges k (comp first u/range-disj) (if (= '< f)
                                                             [1 val]
                                                             [val 4000]))]
      (flatten [(accepted left workflows then)
                (accepted right workflows else)]))))

(def xmas (zipmap [:x :m :a :s] (repeat [1 4000])))

(defn part-1-solver [input]
  (let [[workflows ratings] (parse-input input)
        flat-wfs (flatten-workflows workflows)
        accepted (accepted xmas flat-wfs :in)]
    (->> ratings
         (filter (fn [rating]
                   (some #(->> (merge-with (fn [rating [from to]]
                                             (<= from rating to))
                                           rating
                                           %)
                               (vals)
                               (every? true?))
                         accepted)))
         (mapcat vals)
         (apply +))))

(defn part-2-solver [input]
  (let [[workflows _] (parse-input input)
        flat-wfs (flatten-workflows workflows)]
    (->> (accepted xmas flat-wfs :in)
         (map (fn [ranges]
                (->> ranges
                     (vals)
                     (map (fn [[l h]] (- (inc h) l)))
                     (apply *))))
         (apply +))))
