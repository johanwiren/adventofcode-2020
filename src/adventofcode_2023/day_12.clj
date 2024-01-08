(ns adventofcode-2023.day-12
  (:require [clojure.string :as str]
            [clojure.math :as math]
            [utils :as u]))

(def input (u/line-seq-input *ns*))

#_(def input ["???.### 1,1,3"
            ".??..??...?##. 1,1,3"
            "?#?#?#?#?#?#?#? 1,3,1,6"
            "????.#...#... 4,1,1"
            "????.######..#####. 1,6,5"
            "?###???????? 3,2,1"])

(defn parse-input [input]
  (for [line input]
    (let [[springs checks] (str/split line #" ")]
      {:springs (mapv (comp keyword str) springs)
       :damages (map parse-long (re-seq #"\d+" checks))})))

(defn repair [springs]
  (let [broken-pos (.indexOf springs :?)]
    (if (nat-int? broken-pos)
      (mapcat repair [(assoc springs broken-pos :.)
                      (assoc springs broken-pos :#)])
      [springs])))

(defn repair-record [{:keys [springs damages]}]
  (map (fn [springs]
         {:springs springs :damages damages})
       (repair springs)))

(defn valid [{:keys [springs damages]}]
  (= damages
     (->> springs
          (partition-by identity)
          (filter (comp #{:#} first))
          (map count))))

(defn part-1-solver [input]
  (->> (parse-input input)
       (pmap repair-record)
       (apply concat)
       (filter valid)
       (count)))

(defn part-2-solver [input]
  false)

(defn add-strat [record]
  (assoc record :strat (->> record
                            :springs
                            (partition-by #{:.})
                            (remove (partial some #{:.}))
                            #_(map (fn [group]
                                   (if (some #{:#} group)
                                     [:fixed (count group)]
                                     [:wildcard (count group)]))))))

(defn add-repairs-count [{:keys [damages strat] :as record}]
  (assoc record :repairs-count 0))


(comment

  (let [{:keys [springs damages]}
        (->> (parse-input input)
             (drop 4)
             (first))]
    [damages (->> springs
                  (partition-by #{:.})
                  (remove (partial some #{:.})))])

  #__)


