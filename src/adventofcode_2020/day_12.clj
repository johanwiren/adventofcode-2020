(ns adventofcode-2020.day-12
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn parse-line [line]
  (let [[_ dir n] (re-matches #"(\w)(\d+)" line)]
    [(keyword dir) (Integer/parseInt n)]))

(defn compass [ship]
  (case (:facing ship)
    :dir/north 0
    :dir/east 90
    :dir/south 180
    :dir/west 270))

(defn ship []
  {:facing :dir/east
   :dir/east 0
   :dir/north 0
   :dir/west 0
   :dir/south 0})

(defn parse-input [in]
  (map parse-line in))

(defn move [ship movement]
  (merge-with + ship movement))

(defn to-dir [degrees]
  (case degrees
    (360 0) :dir/north
    90 :dir/east
    180 :dir/south
    270 :dir/west
    -90 :dir/west
    -180 :dir/south
    -270 :dir/east))

(defn rotate [ship degrees]
  (let [new-compass (+ (compass ship) degrees)
        new-dir (cond
                  (< 360 new-compass) (- new-compass 360)
                  :else new-compass)]
    (assoc ship :facing (to-dir new-dir))))

(defn step [ship [dir n]]
  (case dir
    :F (move ship {(:facing ship) n})
    :N (move ship {:dir/north n})
    :S (move ship {:dir/south n})
    :E (move ship {:dir/east n})
    :W (move ship {:dir/west n})
    :L (rotate ship (- n))
    :R (rotate ship n)))

(defn pos [m]
  (select-keys m [:dir/north :dir/east :dir/west :dir/south]))

(defn rotate-pos [{:keys [dir/west dir/north dir/east dir/south] :as nav} n]
  (case n
    90 {:dir/north west
        :dir/east north
        :dir/south east
        :dir/west south}
    180 (-> (rotate nav 90)
            (rotate 90))
    270 (-> (rotate nav 180)
            (rotate 90))
    360 nav
    450 (rotate nav 90)
    -90 {:dir/north east
         :dir/east south
         :dir/south west
         :dir/west north}
    -180 (-> (rotate nav -90)
             (rotate -90))
    -270 (-> (rotate nav -180)
             (rotate -90))
    -360 nav))

(defn rotate-waypoint [{:keys [ship waypoint] :as nav} n]
  (assoc nav :waypoint (merge-with - (pos ship) waypoint)))

(defn step-2 [{:keys [waypoint] :as nav} [dir n]]
  (case dir
    :F (update nav :ship move (into {}
                                    (map (fn [[k v]]
                                           [k (* n v)]))
                                    (pos waypoint)))
    :N (update nav :waypoint move {:dir/north n})
    :S (update nav :waypoint move {:dir/south n})
    :E (update nav :waypoint move {:dir/east n})
    :W (update nav :waypoint move {:dir/west n})
    :L (rotate-waypoint nav (- n))
    :R (rotate-waypoint nav n)))

(defn nav []
  {:ship (ship)
   :waypoint (assoc (ship)
                    :dir/north 1
                    :dir/east 10)})

(defn mh-dist-from-start [{:keys [dir/north dir/east dir/south dir/west]}]
  (Math/abs (+ (- south north)
               (- east west))))

(defn part-1-solver [in]
  (-> (reduce step
              (ship)
              (parse-input in))
      mh-dist-from-start))

(defn part-2-solver [in]
  in)

(t/deftest part-1-test
  (t/is (= 25 (part-1-solver reference-input)))
  (t/is (= 636 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= :FIXME (part-2-solver reference-input))))
