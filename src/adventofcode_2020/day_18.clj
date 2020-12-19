(ns adventofcode-2020.day-18
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn parse-line [line]
  (edn/read-string (format "(%s)" line)))

(defn parse-input [in]
  (map parse-line in))

(defn infix [a1 op a2 & more]
  (let [res ((resolve op) a1 a2)]
    (if more
      (apply infix (cons res more))
      res)))

(defn infix-2 [expr]
  (let [expr-map
        (->> (cons '+ expr)
             (partition 2)
             (group-by first))]
    (-> expr-map
        (update '+ (comp
                    (partial reduce +)
                    (partial map second)))
        (update '* (comp
                    (partial reduce *)
                    (partial map second)))
        (vals)
        (->>
         (apply *)))))

(defn fix-precedence [expr]
  (-> (str/join " " expr)
      (str/replace #"(\d+( \+ \d+)+)" "($1)")
      (parse-line)))

(defn walk-1 [expr]
  (walk/postwalk
   (fn [x] (cond
             (and (seqable? x)
                  (next x))
             (apply infix x)

             (seqable? x)
             (first x)

             :else x))
   expr))

(defn walk-2 [expr]
  (walk/postwalk
   (fn [x] (if (seqable? x)
             (if (and (some #{'+} x)
                      (< 3 (count x)))
               (walk-1 (fix-precedence x))
               (infix-2 x))
             x))
   expr))

(defn part-1-solver [in]
  (->> (parse-input in)
       (map walk-1)
       (apply +)))

(defn part-2-solver [in]
  (->> (parse-input in)
       (map walk-2)
       (apply +)))

(t/deftest part-1-test
  (t/is (= 13632 (part-1-solver reference-input)))
  (t/is (= 6923486965641 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 23340 (part-2-solver reference-input)))
  (t/is (= 70722650566361 (part-2-solver input))))
