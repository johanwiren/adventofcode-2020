(ns adventofcode-2020.day-15
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn init-game [ns]
  (reduce (fn [acc [i n]]
            (-> (update acc n (comp (partial cons (inc i))
                                    (partial take 1)))
                (assoc :spoken n
                       :turn (inc i))))
          {}
          (map-indexed (fn [i n]
                         [i n])
                       ns)))

(defn play-turn [{:keys [turn spoken] :as game}]
  (let [this-turn (inc turn)
        n (max 0
               (->> (get game spoken [0])
                    (apply -)))]
    (-> (assoc game :spoken n)
        (assoc :turn this-turn)
        (update n (comp (partial cons this-turn)
                        (partial take 1))))))

(defn parse-input [in]
  (init-game
   (map edn/read-string
        (-> in
            first
            (str/split #",")))))

(defn part-1-solver [in]
  (let [{:keys [turn] :as game} (parse-input in)]
    (->> (iterate play-turn game)
         (drop (- 2020 turn))
         (first)
         (:spoken))))

(defn part-2-solver [in]
  (let [{:keys [turn] :as game} (parse-input in)]
    (->> (iterate play-turn game)
         (map :spoken))))

(comment

  (->> (part-1-solver reference-input)
       (drop 2017)
       (first )
       (map :spoken))

  (parse-input reference-input)

  (parse-input input)

  (part-1-solver input)

  ;; 660 Too high

  (->> input
       part-2-solver
       (drop 200000)
       (take 1000))

  )

(t/deftest part-1-test
  (t/is (= 436 (part-1-solver reference-input)))
  (t/is (= 253 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= :FIXME (part-2-solver reference-input))))
