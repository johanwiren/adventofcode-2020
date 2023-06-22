(ns adventofcode-2015.day07
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def input (line-seq (io/reader (io/resource "2015/day-07.txt"))))

(defn parse-line [line]
  (let [[strexpr k] (rest (re-matches #"(.*) -> (.*)" line))
        [e1 e2 e3] (map edn/read-string (str/split strexpr #" "))
        v (if e3
            {:op e2
             :args [e1 e3]}
            (if e2
              {:op e1
               :args [e2]}
              e1))]
    {(edn/read-string k) v}))

(defn parse-input [input]
  (into {} (map parse-line input)))

(declare resolve)

(defn resolve-op [{:keys [op args]} m]
  (let [vals (map (partial resolve m) args)
        f (case op
            AND bit-and
            OR bit-or
            NOT bit-not
            RSHIFT bit-shift-right
            LSHIFT bit-shift-left)]
    (apply f vals)))

(def resolve
  (memoize
   (fn [m k]
     (if (number? k)
       k
       (let [v (get m k)]
         (cond
           (map? v) (resolve-op v m)
           (symbol? v) (resolve m v)
           (number? v) v))))))

(defn part-1-solver [input]
  (resolve (parse-input input) 'a))

(defn part-2-solver [input]
  (let [wiring (parse-input input)
        a (resolve wiring 'a)]
    (resolve (assoc wiring 'b a) 'a)))

(comment
  (part-1-solver input)

  (part-2-solver input)

  (parse-input input)

  (resolve (parse-input input) (symbol "a"))

  (map parse-line input)




  )
