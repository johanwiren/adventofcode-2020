(ns adventofcode-2015.day16
  (:require [utils :as u]
            [clojure.string :as str]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (let [kws (-> line
                (str/replace #"[:,]" "")
                (str/split #" "))]
    (-> (apply hash-map kws)
        (update-vals parse-long)
        (update-keys keyword))))

(defn parse-input [input]
  (map parse-line input))

(def my-sue {:children 3
             :cats 7
             :samoyeds 2
             :pomeranians 3
             :akitas 0
             :vizslas 0
             :goldfish 5
             :trees 3
             :cars 2
             :perfumes 1})

(defn is-my-sue? [cmp sue]
  (every? (fn [[k v]]
            ((cmp k) v (k my-sue)))
          (dissoc sue :Sue)))

(defn solver [input cmp]
  (->> (parse-input input)
       (filter (partial is-my-sue? cmp))
       (first)
       :Sue))

(defn part-1-solver [input]
  (solver input (constantly =)))

(defn sue-cmp [k]
  (case k
    (:cats :trees) >
    (:pomeranians :goldfish) <
    =))

(defn part-2-solver [input]
  (solver input sue-cmp))

(t/deftest part-1-solver-test
  (t/is (= 373 (part-1-solver input))))

(t/deftest part-2-solver-test
  (t/is (= 260 (part-2-solver input))))
