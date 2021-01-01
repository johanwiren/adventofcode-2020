(ns adventofcode-2020.day-23
  (:require [adventofcode-2020.util :refer [t tt]]
            [clojure.test :as t]))

(def reference-input [3 8 9 1 2 5 4 6 7])

(def input [9 6 3 2 7 5 4 8 1])

(defn parse-input [in]
  (let [curr (first in)]
    {:nexts (zipmap in (conj (vec (rest in)) curr))
     :curr curr}))

(defn parse-input-2 [in]
  (let [curr (first in)
        m (apply max in)
        all (concat in (range (inc m) 1001))]
    {:nexts (zipmap all (conj (vec (rest all)) curr))
     :curr curr}))

(defn- pick-up [nexts curr]
  (->> curr
       (iterate nexts)
       (drop 1)
       (take 3)))

(defn- dest-cup [remaining-cups curr]
  (or (->> (iterate dec (dec curr))
           (take-while pos?)
           (filter (set remaining-cups))
           (first))
      (apply max remaining-cups)))

(defn move [{:keys [curr nexts]}]
  (let [picked-up (pick-up nexts curr)
        remaining-cups (->> (keys nexts)
                            (remove (set picked-up)))
        dest-cup (dest-cup remaining-cups curr)
        last-picked-up (last picked-up)
        new-nexts (-> nexts
                      (assoc curr (nexts last-picked-up))
                      (assoc dest-cup (first picked-up))
                      (assoc last-picked-up (nexts dest-cup)))]
    {:nexts new-nexts
     :curr (new-nexts curr)}))

(defn- right-of [nexts cup]
  (->> (nexts cup)
       (iterate nexts)
       (take 8)
       (apply str)
       (Integer/parseInt)))

(defn part-1-solver [in]
  (let [game (parse-input in)
        nexts (->> game
                   (iterate move)
                   (drop 100)
                   (first)
                   (:nexts))]
    (right-of nexts 1)))

(defn part-2-solver [in]
  (let [game (parse-input-2 in)
        nexts (->> game
                   (iterate move)
                   (drop 10000)
                   (first)
                   (:nexts))]
    (nexts 1)))

(comment

  (part-1-solver input)
  (part-2-solver input)

  )

(t/deftest part-1-test
  (t/is (= 67384529 (part-1-solver reference-input)))
  (t/is (= 97632548 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= :FIXME (part-2-solver reference-input))))
