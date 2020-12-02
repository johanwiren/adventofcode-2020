(ns adventofcode-2020.day-2
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [clojure.edn :as edn]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn parse-line [line]
  (let [entry (->> line
                   (re-matches #"([\d]+)-([\d]+) ([a-z]): (.*)")
                   (drop 1)
                   (zipmap [:min :max :char :password]))]
    (-> entry
        (update :min edn/read-string)
        (update :max edn/read-string))))

(defn parse-line-2 [line]
  (let [entry (->> line
                   (re-matches #"([\d]+)-([\d]+) ([a-z]): (.*)")
                   (drop 1)
                   (zipmap [:pos1 :pos2 :char :password]))]
    (-> entry
        (update :pos1 (comp dec edn/read-string))
        (update :pos2 (comp dec edn/read-string)))))

(defn parse-input [in]
  (map parse-line in))

(defn valid? [{:keys [min max password char]}]
  (let [match-count (->> password
                         (re-seq (re-pattern char))
                         (count))]
    (<= min match-count max)))

(defn part-1-solver [in]
  (->> in
       parse-input
       (filter valid?)
       (count)))

(defn extract-chars [{:keys [pos1 pos2 password]}]
  (->> [(get password pos1)
        (get password pos2)]
       (remove nil?)
       (apply str)))

(defn validate-2 [{:keys [char] :as passwd-entry}]
  (let [check-chars (extract-chars passwd-entry)]
    (->> check-chars
         (re-seq (re-pattern char))
         (apply str)
         (= char))))

(defn part-2-solver [in]
  (->> in
       (map parse-line-2)
       (filter validate-2)
       (count)))

(comment

  (part-1-solver reference-input)

  (part-1-solver input)

  (part-2-solver reference-input)

  (time (part-2-solver input))

  )

(t/deftest part-1-test
  (t/is (= 2 (part-1-solver reference-input))))

(t/deftest part-2-test
  (t/is (= 1 (part-2-solver reference-input))))
