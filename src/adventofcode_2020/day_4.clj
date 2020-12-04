(ns adventofcode-2020.day-4
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn parse-line [line]
  (->> (str/split line #" ")
       (mapcat #(str/split % #":"))
       (partition 2)
       (map (fn [[k v]] [(keyword k) v]))
       (into {})))

(defn parse-input [in]
  (->> in
       (partition-by #{""})
       (remove #{[""]})
       (map #(str/join " " %))
       (map parse-line)))

(def req-fields #{:byr :iyr :eyr :hgt :hcl :ecl :pid})
(def all-fields (conj req-fields :cid))

(defn valid-p1? [passport]
  (let [fields (-> passport
                   (select-keys all-fields)
                   keys
                   set)]
    (set/superset? fields req-fields)))

(defn part-1-solver [in]
  (->> (parse-input in)
       (filter valid-p1?)
       (count)))

(defn valid-p2? [{:keys [byr iyr eyr hgt hcl ecl pid]}]
  (and (<= 1920 (edn/read-string byr) 2002)
       (<= 2010 (edn/read-string iyr) 2020)
       (<= 2020 (edn/read-string eyr) 2030)
       (or (re-matches #"1([5-8][0-9]|9[0-3])cm" hgt)
           (re-matches #"(59|6[0-9]|7[0-6])in" hgt))
       (re-matches #"#[0-9a-f]{6}" hcl)
       (re-matches #"(amb|blu|brn|gry|grn|hzl|oth)" ecl)
       (re-matches #"[0-9]{9}" pid)))

(defn part-2-solver [in]
  (->> (parse-input in)
       (filter valid-p1?)
       (filter valid-p2?)
       (count)))

(comment

  (parse-input input)

  (parse-input reference-input)

  (part-1-solver reference-input)

  (part-1-solver input)

  (part-2-solver reference-input)

  (part-2-solver input)

  )

(t/deftest part-1-test
  (t/is (= 2 (part-1-solver reference-input))))
