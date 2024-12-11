(ns adventofcode-2024.day-11
  (:require
   [clojure.string :as str]
   [clojure.test :as t]
   [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn long-gen [n]
  (let [s (str n)
        count (count s)
        split (quot count 2)]
    (cond
      (= 0 n)
      [1]

      (even? count)
      [(parse-long (subs s 0 split))
       (parse-long (subs s split))]

      :else
      [(* 2024 n)])))

(defn solver [input generations]
  (loop [counts (frequencies (map parse-long (str/split (first input) #" ")))
         iter generations]
    (if (pos? iter)
      (recur (->> counts
                  (mapcat (fn [[n count]]
                            (map #(vector % count) (long-gen n))))
                  (reduce (fn [acc [n count]]
                            (assoc acc n (+ (get acc n 0) count)))
                          {}))
             (dec iter))
      (->> counts
           vals
           (reduce +)))))

(defn part-1-solver [input]
  (solver input 25))

(defn part-2-solver [input]
  (solver input 75))

(t/deftest part-1-test
  (t/is (= 186175 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 220566831337810 (part-2-solver input))))
