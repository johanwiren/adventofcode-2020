(ns adventofcode-2022.day-15
  (:require [utils :as u]
            [clojure.test :as t]))

(def scan-line 2000000)

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (let [[sx sy bx by] (->> (re-seq #"-?\d+" line)
                           (map parse-long))]
    {:sensor [sx sy]
     :beacon [bx by]}))

(defn parse-input [input]
  (map parse-line input))

(defn add-reach [{:keys [beacon sensor] :as scan}]
  (let [reach (->> sensor
                   (mapv - beacon)
                   (map abs)
                   (apply +))]
    (assoc scan :reach reach)))

(defn scan-line-intersect [{:keys [reach sensor]}]
  (let [[x y]        sensor
        to-scan-line (abs (- y scan-line))
        span         (- reach to-scan-line)]
    (when (pos? span)
      [(- x span) (+ x span)])))

(defn part-1-solver [input]
  (let [intersects (->> (parse-input input)
                        (map add-reach)
                        (keep scan-line-intersect)
                        (apply concat))]
    (- (apply max intersects) (apply min intersects))))

(defn line-eqn [[[x1 y1] [x2 y2]]]
  (let [a (- y1 y2)
        b (- x2 x1)
        c (+ (* (- x1 x2)
                y1)
             (* (- y2 y1)
                x1))]
    {:a a :b b :c c}))

(defn det [^double a ^double b ^double c ^double d]
  (- (* a d) (* b c)))

(defn intersect [{m-a :a m-b :b m-c :c}
                 {n-a :a n-b :b n-c :c}]
  (let [zn (det m-a m-b n-a n-b)]
    (when-not (zero? zn)
      [(/ (- (det m-c m-b n-c n-b)) zn)
       (/ (- (det m-a m-c n-a n-c)) zn)])))

(defn in-reach? [scans pos]
  (some (fn [{:keys [sensor reach]}]
          (->> pos
               (map (comp abs -)
                    sensor)
               (apply +)
               (>= reach)))
        scans))

(defn pairs [xs]
  (let [v (vec xs)
        n (count v)]
    (for [i     (range n)
          j     (range i n)
          :when (not= i j)]
      [(get v i) (get v j)])))

(defn perimeter [{:keys [sensor reach]}]
  (let [[x y] sensor
        reach (inc reach)
        top   [x (- y reach)]
        right [(+ x reach) y]
        down  [x (+ y reach)]
        left  [(- x reach) y]]
    [[top right] [right down] [down left] [left top]]))

(defn part-2-solver [input]
  (let [scans (map add-reach (parse-input input))]
    (->> scans
         (mapcat perimeter)
         (map line-eqn)
         (pairs)
         (keep (partial apply intersect))
         (filter (fn [[x y]]
                   (and (<= 0 x 4000000)
                        (<= 0 y 4000000))))
         (remove (partial in-reach? scans))
         (map (fn [[x y]] (long (+ (* 4000000 x) y))))
         (distinct)
         (first))))

(t/deftest part-1-test
  (t/is (= 5511201 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 11318723411840 (time (part-2-solver input)))))
