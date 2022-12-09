(ns adventofcode-2022.day-09
  (:require [adventofcode-2022.utils :as u]
            [clojure.string :as str]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (let [[dir count] (str/split line #" ")]
    (repeat (parse-long count) (keyword dir))))

(defn parse-input [input]
  {:moves (mapcat parse-line input)})

(defn do-move [[x y] dir]
  (case dir
    :U [x (inc y)]
    :D [x (dec y)]
    :L [(dec x) y]
    :R [(inc x) y]))

(defn follow-head [head tail]
  (let [diff     (mapv - head tail)
        abs-diff (set (map abs diff))]
    (cond
      (= #{1 2} abs-diff)
      (mapv (fn [diff-val tail-val]
              (if (= 1 (abs diff-val))
                (+ tail-val diff-val)
                (+ tail-val (quot diff-val 2))))
            diff
            tail)

      (or (= #{0 2} abs-diff)
          (= #{2} abs-diff))
      (mapv + (mapv #(quot % 2) diff) tail)

      :else
      tail)))

(defn move [{rope :rope, t-visited :t-visited [move & moves] :moves}]
  (let [new-head (do-move (first rope) move)
        new-rope (reduce (fn [new-rope pos]
                           (conj new-rope (follow-head (last new-rope) pos)))
                         [new-head]
                         (rest rope))]
    {:rope new-rope :moves moves :t-visited (conj t-visited (last new-rope))}))

(defn solve [input rope]
  (->> (assoc input :rope rope :t-visited (set [(last rope)]))
       (iterate move)
       (drop-while :moves)
       (first)
       (:t-visited)
       (count)))

(defn part-1-solver [input]
  (solve (parse-input input) [[0 0] [0 0]]))

(defn part-2-solver [input]
  (solve (parse-input input) (repeat 10 [0 0])))

(t/deftest part-1-test
  (t/is (= 6486 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 2678 (time (part-2-solver input)))))
