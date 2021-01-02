(ns adventofcode-2020.day-23
  (:require [clojure.test :as t]))

(def reference-input [3 8 9 1 2 5 4 6 7])

(def input [9 6 3 2 7 5 4 8 1])

(defn parse-input [in]
  (let [nexts (make-array Integer/TYPE (inc (count in)))
        curr (first in)]
    (run! (fn [[k v]]
            (aset-int nexts k v))
          (zipmap in (conj (vec (rest in))
                           curr)))
    [(first in) nexts (apply max in)]))

(defn parse-input-2 [in]
  (let [nexts (into-array Integer/TYPE (range 1 1000002))]
    (run! (fn [[k v]]
            (aset-int nexts k v))
          (zipmap in (conj (vec (rest in))
                           (inc (count in)))))
    (aset nexts 1000000 (first in))
    [(first in) nexts 1000000]))

(defn pick-up [nexts curr]
  (->> curr
       (iterate (partial aget nexts))
       (drop 1)
       (take 3)))

(defn dest-cup [nexts picked-up curr max]
  (or (->> (iterate dec (dec curr))
           (take-while pos?)
           (filter (partial aget nexts))
           (remove (set picked-up))
           (first))
      max))

(defn move [[curr nexts max]]
  (let [picked-up (pick-up nexts curr)
        dest-cup (dest-cup nexts picked-up curr max)
        last-picked-up (last picked-up)
        new-curr (aget nexts last-picked-up)
        new-dest-cup (first picked-up)
        new-last-picked-up (aget nexts dest-cup)]
    (aset-int nexts curr new-curr)
    (aset-int nexts dest-cup new-dest-cup)
    (aset-int nexts last-picked-up new-last-picked-up)
    [(aget nexts curr) nexts max]))

(defn- right-of [nexts cup]
  (->> (aget nexts cup)
       (iterate (partial aget nexts))
       (take 8)
       (apply str)
       (Integer/parseInt)))

(defn part-1-solver [in]
  (let [game (parse-input in)
        nexts (->> game
                   (iterate move)
                   (drop 100)
                   (first)
                   (second))]
    (right-of nexts 1)))

(defn part-2-solver [in]
  (let [game (parse-input-2 in)
        nexts (->> game
                   (iterate move)
                   (drop 10000000)
                   (first)
                   (second))]
    (* (aget nexts 1)
       (aget nexts (aget nexts 1)))))

(t/deftest part-1-test
  (t/is (= 97632548 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 412990492266 (time (part-2-solver reference-input)))))
