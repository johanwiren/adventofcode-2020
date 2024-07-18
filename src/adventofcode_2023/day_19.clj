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

(defn stmt-sexp [stmt]
  (if (vector? stmt)
    (let [[k f val res] stmt]
      (list 'and `(~f ~(symbol (name k)) ~val) res))
    stmt))

(defn stmts-sexp [stmts]
  (let [sexp (map stmt-sexp stmts)]
    (list 'fn '[{:keys [x m a s] :as input}] (cons 'or sexp))))

(defn workflow-functions [workflows]
  (update-vals workflows (comp eval stmts-sexp)))

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

(defn part-1-solver [input]
  (let [[workflows ratings] (parse-input input)
        fs (workflow-functions (flatten-workflows workflows))
        accepted? (fn [rating]
                    (->> (iterate #((% fs) rating) :in)
                         (drop-while (complement #{:A :R}))
                         (first)
                         (= :A)))]

    (->> (filter accepted? ratings)
         (mapcat vals)
         (reduce +))))

(defn combinations [ranges workflows k]
  (cond
    (some nil? (vals ranges)) 0

    (= :R k) 0

    (= :A k) (->> ranges
                  vals
                  (map (fn [[l h]] (- (inc h) l)))
                  (reduce *))

    :else
    (let [wf (k workflows)
          [[k f val then] else] wf
          left (update ranges k (comp first u/range-disj) (if (= '< f)
                                                            [(dec val) 4000]
                                                            [1 (inc val)]))
          right (update ranges k (comp first u/range-disj) (if (= '< f)
                                                             [1 val]
                                                             [val 4000]))]
      (+ (combinations left workflows then)
         (combinations right workflows else)))))

(def xmas (zipmap [:x :m :a :s] (repeat [1 4000])))

(defn part-2-solver [input]
  (let [[workflows _] (parse-input input)
        flat-wfs (flatten-workflows workflows)]
    (combinations xmas flat-wfs :in)))
