(ns adventofcode-2025.day-05
  (:require
   [clojure.test :as t]
   [utils :as u]))

;; start inclusive, end inclusive
(def input (u/line-seq-input *ns*))

(defn overlaps? [interval-a interval-b]
  (let [[a b] interval-a
        [x y] interval-b]
    (or
     (<= x a b y)
     (<= x a y b)
     (<= a x b y)
     (<= a x y b))))

(defn merge-interval [[a b] [x y]]
  [(min a x) (max b y)])

(defn conj-interval [intervals interval]
  (let [targets (filter #(overlaps? interval %) intervals)]
    (if (seq targets)
      (-> (apply disj intervals targets)
          (conj (reduce merge-interval interval targets)))
      (conj intervals interval))))

(defn parse-line [line]
  (mapv parse-long (re-seq #"\d+" line)))

(defn exclusive-end [[a b]]
  ;; Input is inclusive end, we want exclusive end
  [a (inc b)])

(defn parse [input]
  (let [[intervals ids]
        (->> input
             (map parse-line)
             (split-with seq))]
    {:intervals (map exclusive-end intervals)
     :ids (->> ids
               (drop 1) ;; Separator
               (map first))}))

(defn in-intervals? [intervals n]
  (->> intervals
       (filter (fn [[a b]] (<= a n b)))
       seq))

(defn part-1-solver [input]
  (let [{:keys [intervals ids]} (parse input)]
    (let [non-overlapping (reduce conj-interval (sorted-set) intervals)]
      (->> ids
           (filter #(in-intervals? non-overlapping %))
           (count)))))

(defn part-2-solver [input]
  (let [{:keys [intervals ids]} (parse input)]
    (let [non-overlapping (reduce conj-interval (sorted-set) intervals)]
      (->> non-overlapping
           (map (fn [[a b]] (- b a)))
           (reduce +)))))

(t/deftest part-1-test
  (t/is (= 888 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 344378119285354 (part-2-solver input))))
