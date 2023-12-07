(ns adventofcode-2023.day-04
  (:require [clojure.string :as str]
            [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (let [[[_ & winning] numbers]
        (->> (str/split line #"\|")
             (map (comp (partial map parse-long)
                        #(re-seq #"\d+" %))))]
    {:numbers numbers
     :winning (set winning)}))

(defn parse-input [input]
  (map parse-line input))

(defn score [{:keys [numbers winning]}]
  (let [wins (count (filter winning numbers))
        bsl (dec wins)]
    (if (pos? wins)
      (bit-shift-left 1 bsl)
      0)))

(defn part-1-solver [input]
  (->> (parse-input input)
       (map score)
       (reduce +)))

(defn part-2-solver [input]
  (->> (parse-input input)
       (map (fn [{:keys [winning numbers]}]
              (count (filter winning numbers))))
       (reverse)
       (reduce (fn [acc score]
                 (conj acc (inc (reduce + (take score acc)))))
               (list))
       (reduce +)))
