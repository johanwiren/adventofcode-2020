(ns adventofcode-2022.day-17
  (:require [utils :as u]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(def minus [[0 0] [1 0] [2 0] [3 0]])
(def plus [[1 0] [0 1] [1 1] [2 1] [1 2]])
(def invL [[0 0] [1 0] [2 0] [2 1] [2 2]])
(def pipe [[0 0] [0 1] [0 2] [0 3]])
(def square [[0 0] [1 0] [0 1] [1 1]])

(def rocks [minus plus invL pipe square])

(def width 7)
(def floor (map (fn [i] (vector i 0)) (range width)))

(defn parse-input [input]
  (->> input
       first
       (map (comp keyword str))))

(def down [0 -1])

(defn move [shape direction]
  (map (partial map + direction) shape))

(defn place-on-top [piece chamber]
  (let [y (->> chamber
               (apply (partial max-key second))
               (last)
               (+ 4))]
    (move piece [2 y])))

(defn collision? [chamber piece]
  (some (set piece) chamber))

(defn outside? [piece]
  (some #{-1 7} (map first piece)))

(defn blow [shape direction]
  (let [direction (case direction
                    :< [-1 0]
                    :> [1 0])
        blown (move shape direction)]
    (if (outside? blown)
      shape
      blown)))

(defn tick [{:keys [chamber jet-pattern] :as state} piece]
  (let [blown (blow piece (first jet-pattern))
        checked-blown (if (collision? chamber blown) piece blown)
        fallen (move checked-blown down)
        state (update state :jet-pattern rest)]
    (if (collision? chamber fallen)
      (-> state
          (update :chamber (partial take 100))
          (update :chamber into checked-blown))
      (recur state fallen))))

(defn infseq [seq]
  (apply concat (repeat seq)))

(defn height-after-n-rocks [jet-pattern n]
  (let [rocks (take n (infseq rocks))]
    (->> rocks
         (reduce (fn [{:keys [chamber] :as state} rock]
                   (let [placed-rock (place-on-top rock chamber)]
                     (tick state placed-rock)))
                 {:chamber floor :jet-pattern jet-pattern})
         (:chamber)
         (take 40)
         (max-key second)
         (first)
         (second))))

(defn part-1-solver [input]
  (let [jet-pattern (->> input parse-input infseq)]
    (height-after-n-rocks jet-pattern 2022)))

(defn has-line? [chamber]
  (->> chamber
       (map second)
       (sort-by -)
       (take 7)
       (apply =)))

(defn height [chamber]
  (->> chamber
       (take 5)
       (max-key second)
       (first)
       (second)))

(defn calculate-height [input cycles]
  (let [last-cycle (->> cycles (take-last 2) (reverse))
        rocks-until-stable (-> cycles second :rocks)
        height-at-stable (-> cycles second :height)
        rocks-per-cycle (apply - (map :rocks last-cycle))
        height-per-cycle (apply - (map :height last-cycle))
        needed-rocks (- 1000000000000 rocks-until-stable)
        needed-cycles (quot needed-rocks rocks-per-cycle)
        height-at-cycles (+ height-at-stable (* height-per-cycle needed-cycles))
        remaining-rocks (rem needed-rocks rocks-per-cycle)
        remaining-height (- (height-after-n-rocks (infseq input)
                                                  (+ remaining-rocks rocks-until-stable))
                            height-at-stable)]
    (+ height-at-cycles remaining-height)))

(defn find-cycles [rocks jet-pattern]
  (reduce (fn [{:keys [chamber rocks lines] :as state} rock]
            (let [placed-rock (place-on-top rock chamber)
                  has-line? (has-line? chamber)]
              (if (= 4 (count lines))
                (reduced lines)
                (cond-> (-> (tick state placed-rock)
                            (update :rocks inc))
                  has-line?
                  (update :lines conj {:rocks  rocks
                                       :height (height chamber)})))))
          {:chamber floor :jet-pattern jet-pattern :rocks 0 :lines []}
          rocks))

(defn part-2-solver [input]
  (let [input (parse-input input)
        jet-pattern (infseq input)
        rocks (infseq rocks)
        cycles (find-cycles rocks jet-pattern)]
    (calculate-height input cycles)))

(t/deftest part-1-test
  (t/is (= 3153 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 1553665689155 (time (part-2-solver input)))))
