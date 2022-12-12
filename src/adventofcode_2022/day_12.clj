(ns adventofcode-2022.day-12
  (:require [adventofcode-2021.day-15 :as lib]
            [adventofcode-2022.utils :as u]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (mapv (fn [char]
          (let [v (int char)]
            (if (> 97 v) (+ 53 v) v)))
        line))

(defn parse-input [input]
  (mapv parse-line input))

(defn find-start-end [input]
  (into {}
        (for [x     (range (count input))
              y     (range (count (first input)))
              :let  [v (get-in input [x y])]
              :when (#{136 122} v)]
          [(keyword (str (char (- v 53)))) [x y]])))

(defn a-star [input start end]
  (let [rows     (count input)
        cols     (count (first input))
        es       input
        vs       (for [x (range rows)
                       y (range cols)]
                   [x y])
        neigh-fn (fn [v]
                   (let [val (get-in es v)]
                     (filter (fn [[x y]]
                               (and (<= 0 x (dec rows))
                                    (<= 0 y (dec cols))
                                    (or (= start v)
                                        (<= (get-in es [x y]) (inc val)))))
                             (lib/neighbours v))))
        h        (fn [v] (apply + (map - end v)))]
    (lib/a-star vs start end neigh-fn (fn [_u v] (get-in es v)) h)))

(defn part-1-solver [input]
  (let [input               (parse-input input)
        {:keys [S E]}       (find-start-end input)
        {:keys [came-from]} (a-star input S E)]
    (->> E
         (iterate came-from)
         (take-while some?)
         (map (juxt identity (comp str char (partial get-in input))))
         (count)
         (dec))))

(defn part-2-solver [input]
  (let [input               (parse-input input)
        {:keys [S E]}       (find-start-end input)
        {:keys [came-from]} (a-star input S E)]
    (->> E
         (iterate came-from)
         (take-while (comp (partial not= 97) (partial get-in input)))
         (map (juxt identity (comp str char (partial get-in input))))
         (count))))

(t/deftest part-1-test
  (t/is (= 534 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 525 (time (part-2-solver input)))))
