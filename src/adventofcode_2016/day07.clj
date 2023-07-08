(ns adventofcode-2016.day07
  (:require [utils :as u]
            [clojure.string :as str]))

(def input (u/line-seq-input *ns*))

(defn is-abba-str? [s]
  (boolean (some (comp #{2} count set rest)
                 (re-seq #"(.)(.)\2\1" s))))

(defn parse-ip [ip]
  (->> (str/split ip #"[\[\]]")
       (map-indexed vector)
       (reduce (fn [acc [i s]]
                 (update acc (if (even? i) :addrs :hypernets) (fnil conj []) s))
               {})))

(defn is-abba? [ip]
  (let [{:keys [addrs hypernets]} (parse-ip ip)]
    [addrs hypernets]
    (and (some is-abba-str? addrs)
         (not-any? is-abba-str? hypernets))))

(defn part-1-solver [input]
  (->> input
       (filter is-abba?)
       count))

(defn find-matches [re s]
  (->> (partition-all (count s) 1 s)
       (map (partial apply str))
       (keep (partial re-matches re))
       (map rest)
       (distinct)))

(defn is-aba? [ip]
  (some true?
        (let [{:keys [addrs hypernets]} (parse-ip ip)
              abas (map (partial find-matches #"^(.)(.)(\1).*") addrs)]
          (for [aba abas
                [a b _a] aba
                hypernet hypernets
                :let [bab (str b a b)]
                :when (and (not= a b)
                           (re-seq (re-pattern bab) hypernet))]
            true))))

(defn part-2-solver [input]
  (->> input
       (filter is-aba?)
       count))
