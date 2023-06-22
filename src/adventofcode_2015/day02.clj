(ns adventofcode-2015.day02
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))

(def input (line-seq (io/reader (io/resource "2015/day-02.txt"))))

(defn parse-line [line]
  (->> line
       (re-matches #"(\d+)x(\d+)x(\d+)")
       (rest)
       (map #(Integer/parseInt %))
       (zipmap [:l :w :h])))

(defn parse-input [input]
  (map parse-line input))

(defn add-sides [{:keys [l w h] :as box}]
  (assoc box :sides [(* l w)
                     (* w h)
                     (* h l)]))

(defn add-box-area [{:keys [sides] :as box}]
  (assoc box :area (->> sides
                        (map (partial * 2))
                        (reduce +))))

(defn add-slack [{:keys [sides] :as box}]
  (assoc box :slack (apply min sides)))

(defn part-1-solver [input]
  (->> input
       (parse-input)
       (map add-sides)
       (map add-box-area)
       (map add-slack)
       (mapcat (juxt :area :slack))
       (reduce +)))

(defn add-perimeters [{:keys [l w h] :as box}]
  (assoc box :perimeters [(+ (* 2 l) (* 2 w))
                          (+ (* 2 w) (* 2 h))
                          (+ (* 2 h) (* 2 l))]))

(defn add-volume [{:keys [l w h] :as box}]
  (assoc box :volume (* l w h)))

(defn add-ribbon [{:keys [perimeters] :as box}]
  (assoc box :ribbon (apply min perimeters)))

(defn part-2-solver [input]
  (->> input
       (parse-input)
       (map add-perimeters)
       (map add-volume)
       (map add-ribbon)
       (mapcat (juxt :ribbon :volume))
       (reduce +)))

(t/deftest part-1-test
  (t/is (= 1598415 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 3812909 (part-2-solver input))))

(comment

  (part-2-solver input)

  (part-1-solver input)

  (parse-input input)

  

  )
