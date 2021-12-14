(ns adventofcode-2021.day-14
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))

(def input (->> "2021/day_14.txt"
                (io/resource)
                (io/reader)
                (line-seq)))

(defn mk-lookup [rules]
  (into {}
        (map (fn [[a b _ _ _ _ c]]
               [[a b] {:char c
                       :pairs [[a c] [c b]]}]))
        rules))

(defn parse-input [input]
  (let [[template _ & rules] input]
    {:template (frequencies (partition 2 1 template))
     :rules (mk-lookup rules)
     :char-freqs (frequencies (seq template))}))

(defn insert [{:keys [rules template] :as state}]
  (reduce (fn [acc [pair n]]
            (let [{char :char [p1 p2] :pairs} (get rules pair)]
              (-> acc
                  (update-in [:char-freqs char] (fnil + 0) n)
                  (update-in [:template p1] (fnil + 0) n)
                  (update-in [:template p2] (fnil + 0) n))))
          (dissoc state :template)
          template))

(defn max-min-diff [{:keys [char-freqs]}]
  (let [xs (vals char-freqs)]
    (- (apply max xs) (apply min xs))))

(defn solver [n input]
  (-> (iterate insert (parse-input input))
      (nth n)
      (max-min-diff)))

(def part-1-solver (partial solver 10))

(def part-2-solver (partial solver 40))

(t/deftest part-1
  (t/is (= 2447 (part-1-solver input))))

(t/deftest part-2
  (t/is (= 3018019237563 (part-2-solver input))))
