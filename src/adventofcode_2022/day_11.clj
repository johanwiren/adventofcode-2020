(ns adventofcode-2022.day-11
  (:require [utils :as u]
            [clojure.string :as str]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn make-op-fn [op-line]
  (let [[arg1 op arg2] (take-last 3 (str/split op-line #" "))
        f              (case op "+" + "*" *)]
    (if (= "old" arg1 arg2)
      #(f % %)
      #(f (parse-long arg2) %))))

(defn parse-monkey [monkey]
  (let [[_ item-line op-line test-line & rules]
        monkey

        op-fn                  (make-op-fn op-line)
        divisor                (parse-long (last (re-seq #"\d+" test-line)))
        test-fn                (comp zero? #(rem % divisor))
        items                  (map parse-long (re-seq #"\d+" item-line))
        [case-true case-false] (map parse-long (re-seq #"\d+" (apply str rules)))]
    {:items     items
     :inspected 0
     :divisor   divisor
     :op-fn     op-fn
     :test-fn   test-fn
     :cases     [case-true case-false]}))

(defn parse-input [input]
  (->> input
       (partition-by #{""})
       (remove #{[""]})
       (mapv parse-monkey)))

(defn round [worry-strategy monkeys]
  (reduce (fn [monkeys m]
            (let [{:keys [op-fn test-fn cases items]} (get monkeys m)
                  [case-true case-false]              cases]
              (reduce (fn [monkeys item]
                        (let [val           (-> item op-fn worry-strategy)
                              target-monkey (if (test-fn val) case-true case-false)]
                          (update monkeys target-monkey update :items #(cons val %))))
                      (-> monkeys
                          (update m assoc :items (list))
                          (update m update :inspected + (count items)))
                      items)))
          monkeys
          (range (count monkeys))))

(defn solve [monkeys worry-strategy rounds]
  (->> monkeys
       (iterate (partial round worry-strategy))
       (drop rounds)
       (first)
       (map :inspected)
       (sort-by -)
       (take 2)
       (apply *)))

(defn part-1-solver [input]
  (solve (parse-input input) #(quot % 3) 20))

(defn part-2-solver [input]
  (let [monkeys        (parse-input input)
        multiplier     (apply * (map :divisor monkeys))
        worry-strategy #(mod % multiplier)]
    (solve monkeys worry-strategy 10000)))

(t/deftest part-1-test
  (t/is (= 56350 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 13954061248 (time (part-2-solver input)))))
