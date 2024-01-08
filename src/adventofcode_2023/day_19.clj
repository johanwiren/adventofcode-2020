(ns adventofcode-2023.day-19
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [utils :as u]))

(def input (u/line-seq-input *ns*))

(def input ["px{a<2006:qkq,m>2090:A,rfg}"
            "pv{a>1716:R,A}"
            "lnx{m>1548:A,A}"
            "rfg{s<537:gd,x>2440:R,A}"
            "qs{s>3448:A,lnx}"
            "qkq{x<1416:A,crn}"
            "crn{x>2662:A,R}"
            "in{s<1351:px,qqz}"
            "qqz{s>2770:qs,m<1801:hdj,R}"
            "gd{a>3333:R,R}"
            "hdj{m>838:A,pv}"
            ""
            "{x=787,m=2655,a=1222,s=2876}"
            "{x=1679,m=44,a=2067,s=496}"
            "{x=2036,m=264,a=79,s=2244}"
            "{x=2461,m=1339,a=466,s=291}"
            "{x=2127,m=1623,a=2188,s=1013}"])

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

(defn range-merge [[a b :as ar] [x y :as xr]]
  (println "Merge" ar xr)
  (cond
    (= nil a x) []
    (nil? a) [x y]
    (nil? x) [a b]
    :else [(min a x) (max b y)]))

(defn combinations [ranges workflows k]
  (if (#{:A :R} k)
    (if (= :A k)
      (do
        (println "Approved combos" ranges)
        [ranges])
      [{:x [] :m [] :a [] :s []}])
    (let [wf (k workflows)
          [[k f val then] else] wf
          left (update ranges k (comp first u/range-disj) (if (= '< f)
                                                            [val 4001]
                                                            [1 val]))
          right (update ranges k (comp first u/range-disj) (if (= '< f)
                                                             [1 val]
                                                             [val 4001]))]
      (concat (combinations left workflows then)
              (combinations right workflows else)))))

(u/range-disj [1 4001] [1000 4001])
;; => [[1 1000]]
(u/range-disj [1 4001] [1 1000])
;; => [[1000 4001]]

(defn n-passed [ranges]
  (->> ranges
       #_(remove (comp (partial some empty?) vals))
       (filter (comp (partial some seq) vals))
       (map (comp (partial keep seq) vals))
       (map (fn [ranges]
              (map (fn [[l h]] (- h l))
                   ranges)))
       (map (partial reduce *))
       (reduce +)))

(comment
  (->> (let [workflows (flatten-workflows (first (parse-input input)))]
         (map (fn [xmas]
                (n-passed (combinations xmas workflows :in)))
              [{:x [1 4000] :m [1 4000] :a [1 4000] :s [1 4000]}
               {:x [1 4001] :m [1 4001] :a [1 4001] :s [1 4001]}])))

  (- 167474394229030 167409079868000)

  (- 167775479868000 167409079868000)
  ;; => 366400000000

  ;;    167409079868000 -- Test

  #__)

(comment
  ;; => 256000000000000
  ;;    167409079868000 -- Test
  ;; => 173972673622530

  ;; This works but takes HOURS
  (def res (future (let [workflows (flatten-workflows (first (parse-input input)))
                         fs (workflow-functions workflows)
                         valid? (fn [rating]
                                  (->> (iterate #((% fs) rating) :in)
                                       (drop-while (complement #{:A :R}))
                                       (first)
                                       (= :A)))
                         {:keys [x m a s]} (->> workflows
                                                (vals)
                                                (map first)
                                                (map butlast)
                                                (map (fn [[k op val]]
                                                       {k (if (= op '<) (dec val) val)}))
                                                (apply merge-with conj (zipmap [:x :m :a :s] (repeat (sorted-set 0 4000)))))]
                     [x m a s]
                     (reduce +
                             (for [[xl xh] (map vector x (rest x))
                                   [ml mh] (map vector m (rest m))
                                   [al ah] (map vector a (rest a))
                                   [sl sh] (map vector s (rest s))
                                   :when (valid? {:x (inc xl) :m (inc ml) :a (inc al) :s (inc sl)})]
                               (* (- xh xl) (- mh ml) (- ah al) (- sh sl)))))))

  (map vector (range 5) (rest (range 5)))
  ;; => ([0 1] [1 2] [2 3] [3 4])
  (map vector (range 4) (rest (range 4)))
  ;; => ([0 1] [1 2] [2 3])

  ;; => 167459205617600
  (- 167459205617600 167409079868000)

;; => 137488213956800

  (->> (let [workflows (flatten-workflows (first (parse-input input)))]
         (->> (combinations {:x [1 4001] :m [1 4001] :a [1 4001] :s [1 4001]}
                            workflows
                            :in)
              (n-passed)
              #_(reduce (partial merge-with range-merge)))))

  ;; Too high -- 132612381836994
  ;; Too high -- 197888972111600
  (->> (combinations {:x [1 4001] :m [1 4001] :a [1 4001] :s [1 4001]}
                     {:s [[:s > 2000 :A] :R]
                      :a [[:a > 2000 :A] :s]
                      :m [[:m > 2000 :A] :a]
                      :in [[:x > 2000 :A] :m]}
                     :in)
       n-passed)
  ;; => 240000000000000

  ;; => 240000000000000
  ;; =>  16000000000000
  (->> (combinations {:x [1 4001] :m [1 4001] :a [1 4001] :s [1 4001]}
                     {:s [[:s > 2001 :R] :A]
                      :a [[:a > 2001 :R] :s]
                      :m [[:m > 2001 :R] :a]
                      :in [[:x > 2001 :R] :m]}
                     :in)
       n-passed)
  ;; => 16000000000000

  (/ 240 4)

  (->> (combinations {:x [1 4001] :m [1 4001] :a [1 4001] :s [1 4001]}
                     (flatten-workflows {:q [[:a '< 1 :R] :A]
                                         :a [[:a '> 2001 :q] [:s '> 2001 :A] :R]
                                         :m [[:m '> 2001 :A] :a]
                                         :in [[:x '> 2001 :A] :m]})
                     :in)
       n-passed)

  #__)
