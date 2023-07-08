(ns adventofcode-2016.day21
  (:require [utils :as u]
            [clojure.string :as str]))

(def input (u/line-seq-input *ns*))

(def to-scramble "abcdefgh")

(defn parse-line [line]
  (->> ["(rotate based on position) of letter (.)"
        "(swap letter) (.) with letter (.)"
        "(move position) (.) to position (.)"
        "(reverse positions) (.) through (.)"
        "(swap position) (.) with position (.)"
        "(rotate right) (.) steps?"
        "(rotate left) (.) steps?"]
       (keep (comp next #(re-matches % line) re-pattern))
       (first)
       ((fn [[cmd & args]]
          [(keyword (str/replace cmd \space \-)) (mapv (fn [arg] (if-let [n (parse-long arg)]
                                                                   n
                                                                   (first arg)))
                                                       args)]))))

(defn parse-input [input]
  (map parse-line input))

(defn rev [s from to]
  (str (subs s 0 from) (apply str (reverse (subs s from (inc to)))) (subs s (inc to))))

(defn swap-letter [s a b]
  (let [token (first (str/upper-case b))]
    (-> s
        (str/replace a token)
        (str/replace b a)
        (str/replace token b))))

(defn swap-position [s i j]
  (swap-letter s (get s i) (get s j)))

(defn rotate-right [s n]
  (let [len (count s)
        n (mod n len)]
    (str (subs s (- len n)) (subs s 0 (- len n)))))

(defn rotate-by-letter [s c]
  (let [n (str/index-of s c)]
    (rotate-right s (cond-> (inc n)
                      (<= 4 n) (inc)))))

(def inv-rot
  {0 1, 1 1, 2 6, 3 2, 4 7, 5 3, 6 0, 7 4})

(defn rotate-left [s n]
  (let [n (mod n (count s))]
    (str (subs s n) (subs s 0 n))))

(defn rotate-by-letter-inv [s c]
  (let [i (str/index-of s c)
        inv-i (inv-rot i)]
    (rotate-left s inv-i)))

(defn insert-at [v i x]
  (let [left (subvec v 0 i)
        right (subvec v i)]
    (into (conj left x) right)))

(defn delete-at [v i]
  (let [left (subvec v 0 i)
        right (subvec v (inc i))]
    (into left right)))

(defn move [s from to]
  (let [chars (into [] s)
        char (get chars from)]
    (apply str (-> chars (delete-at from) (insert-at to char)))))

(def cmd-fn
  {:rotate-based-on-position rotate-by-letter
   :swap-letter swap-letter
   :swap-position swap-position
   :move-position move
   :reverse-positions rev
   :rotate-right rotate-right
   :rotate-left rotate-left})

(defn scramble [instrs s]
  (->> instrs
       (reduce (fn [s [cmd & [args]]]
                 (let [cmd-fn (cmd-fn cmd)]
                   (apply cmd-fn s args)))
               s)))

(defn part-1-solver [input]
  (scramble (parse-input input) to-scramble))

(def cmd-fn-p2
  (merge cmd-fn
         {:rotate-based-on-position rotate-by-letter-inv
          :move-position (fn [s from to] (move s to from))
          :rotate-right rotate-left
          :rotate-left rotate-right}))

(defn part-2-solver [input]
  (->> (parse-input input)
       (reverse)
       (reduce (fn [s [cmd & [args]]]
                 (let [cmd-fn (cmd-fn-p2 cmd)]
                   (apply cmd-fn s args)))
               "fbgdceah")))
