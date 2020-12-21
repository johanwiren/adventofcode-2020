(ns adventofcode-2020.day-19
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.test :as t])
  (:refer-clojure :exclude 'resolve))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn unwrap [x]
  (if (seqable? x)
    (let [[h & t :as l] x]
      (if t l h))
    x))

(defn parse-rule [rule]
  (let [[_ id rule] (re-matches #"(\d+): (.*)" rule)]
    [(Integer/parseInt id) (-> rule
                               (str/replace "|" "\"|\"")
                               (str/split #" ")
                               (->> (map edn/read-string))
                               (unwrap))]))



(defn resolve-rule [rules n]
  (if (string? n)
    n
    (let [rule (get rules n)]
      (cond
        (string? rule) rule
        (seqable? rule) (let [rs (map (partial resolve-rule rules) rule)]
                          (format "(%s)" (apply str rs)))
        :else (resolve-rule rules rule)))))

(defn parse-rules [rules]
  (->> rules
       (sort-by first)
       (reverse)
       (map parse-rule)
       (into {})))

(defn parse-input [in]
  (let [[rules _ messages] (partition-by #{""} in)]
    [(parse-rules rules) messages]))

(defn count-matches [re messages]
  (->> messages
       (filter (partial re-matches re))
       (count)))

(defn part-1-solver [in]
  (let [[rules messages] (parse-input in)
        r0 (resolve-rule rules 0)
        re (re-pattern r0)]
    (count-matches re messages)))

(defn part-2-solver [in]
  (let [[rules messages] (parse-input in)
        patched-rules (assoc rules
                             ;; [42 "|" 42 8]
                             8 ["(" 42 ")" "+"]
                             ;; [42 31 "|" 42 11 31]
                             11 [42 31 "|" 42 42 31 31 "|" 42 42 42 31 31 31 "|" 42 42 42 42 31 31 31 31])
        r0 (resolve-rule patched-rules 0)
        re (re-pattern r0)]
    (count-matches re messages)))

(t/deftest part-1-test
  (t/is (= 2 (part-1-solver reference-input)))
  (t/is (= 149 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 332 (part-2-solver input))))
