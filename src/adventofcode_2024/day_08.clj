(ns adventofcode-2024.day-08
  (:require
   [clojure.test :as t]
   [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse [input]
  (let [max-x (dec (count (first input)))
        max-y (dec (count input))]
    {:bounds [[0 0] [max-x max-y]]
     :antennas (u/to-xy-point-map #(when (not= \. %) %) input)}))

(defn in-bbox? [[[min-x min-y] [max-x max-y]] [x y]]
  (and (<= min-x x max-x)
       (<= min-y y max-y)))

(defn antinodes [a b]
  [(mapv + a (mapv - a b))
   (mapv + b (mapv - b a))])

(defn part-1-solver [input]
  (let [{:keys [bounds antennas]} (parse input)]
    (->> (reduce (fn [acc [pos freq]]
                   (update acc freq (fnil conj []) pos))
                 {}
                 antennas)
         (vals)
         (mapcat u/pairs)
         (mapcat (fn [[a b]] (antinodes a b)))
         (filter (partial in-bbox? bounds))
         set
         count)))

(defn repeating-antinodes [a b]
  [(iterate #(mapv + % (mapv - a b)) a)
   (iterate #(mapv + % (mapv - b a)) b)])

(defn part-2-solver [input]
  (let [{:keys [bounds antennas]} (parse input)]
    (->> (reduce (fn [acc [pos freq]]
                   (update acc freq (fnil conj []) pos))
                 {}
                 antennas)
         (vals)
         (mapcat u/pairs)
         (mapcat (fn [[a b]]
                   (mapcat (partial take-while (partial in-bbox? bounds))
                           (repeating-antinodes a b))))
         set
         count)))

(t/deftest part-1-test
  (t/is (= 329 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 1190 (part-2-solver input))))
