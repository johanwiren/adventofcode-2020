(ns adventofcode-2022.day-21
  (:require [adventofcode-2022.utils :as u]
            [clojure.string :as str]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn sexp [vars expr]
  (cond
    (vector? expr)
    (cons (first expr) (map (partial sexp vars) (rest expr)))

    (keyword? expr)
    (sexp vars (get vars expr))

    :else
    expr))

(defn parse-line [line]
  (let [[k _ a1 op a2] (str/split line #"[: ]")]
    (cond-> [(keyword k)]
      op
      (conj [(symbol op) (keyword a1) (keyword a2)])

      (not op)
      (conj (parse-long a1)))))

(defn parse-input [input]
  (into {} (map parse-line input)))

(defn part-1-solver [input]
  (let [vars (parse-input input)]
    (eval (sexp vars (:root vars)))))

(defn part-2-solver [input]
  (let [vars (-> (parse-input input)
                 (assoc :humn 'x))
        root (:root vars)
        [_ left right] root
        sexp-left (sexp vars left)
        expected (eval (sexp vars right))
        f (eval (list 'fn '[x] sexp-left))]
    (loop [l 0
           r 1000000000000000]
      (let [m (quot (+ l r) 2)
            res (f m)]
        (cond
          (= expected res) m
          (< expected res) (recur (inc m) r)
          :else (recur l (dec m)))))))

(t/deftest part-1-test
  (t/is (= 104272990112064 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 3220993874133 (time (part-2-solver input)))))
