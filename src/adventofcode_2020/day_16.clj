(ns adventofcode-2020.day-16
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn parse-rules [rules]
  (into {}
        (comp (map next)
              (map (juxt (comp keyword first)
                         (comp (partial partition 2)
                               (partial map #(Integer/parseInt %))
                               rest))))
        (map (partial re-matches #"([\w ]+): (\d+)-(\d+) or (\d+)-(\d+)") rules)))

(defn parse-tickets [tickets]
  (->> (rest tickets)
       (map (comp (partial map #(Integer/parseInt %))
                  #(str/split % #",")))))

(defn parse-input [in]
  (let [[rules my-ticket nearby-tickets]
        (->> in
             (partition-by #{""})
             (remove #{[""]}))]
    {:rules (parse-rules rules)
     :my-ticket (first (parse-tickets my-ticket))
     :nearby (parse-tickets nearby-tickets)}))

(defn p1-validator [{:keys [rules]}]
  (fn [x]
    (->> rules
         (keep (fn [[r [[r11 r12] [r21 r22]]]]
                 (when-not (or (<= r11 x r12)
                               (<= r21 x r22))
                   r)))
         seq)))

(defn part-1-solver [in]
  (let [{:keys [rules nearby] :as tickinf} (parse-input in)
        val-validator (p1-validator tickinf)
        invalid? (fn [x]
                 (= (set (keys rules))
                    (set (val-validator x))))]
    (->> (flatten nearby)
         (filter invalid?)
         (apply +))))

(defn shrink [x]
  (if (every? keyword? x)
    x
    (let [[i r] (first (->> x
                            (map-indexed (fn [i x]
                                           [i x]))
                            (filter (comp set? second))
                            (filter (comp #{1} count second))))
          rule-key (first r)]
      (->> (assoc x i rule-key)
           (mapv (fn [x]
                   (if (set? x)
                     (disj x rule-key)
                     x)))
           (shrink)))))

(defn part-2-solver [in]
  (let [{:keys [rules nearby my-ticket] :as tickinf} (parse-input in)
        val-validator (p1-validator tickinf)
        rule-keys (set (keys rules))
        invalid? (partial some (fn [x]
                                 (= rule-keys
                                    (set (val-validator x)))))
        ks (->> nearby
                (remove invalid?)
                (map (partial map (comp set val-validator)))
                (reduce (fn [acc x]
                          (map (fn [a b] (set/difference a b)) acc x))
                        (repeat (count rule-keys) rule-keys))
                (vec)
                (shrink))]
    (->> (zipmap ks my-ticket)
         (filter (comp #(str/starts-with? % "departure") name first))
         (map second)
         (apply *))))

(t/deftest part-1-test
  (t/is (= 71 (part-1-solver reference-input)))
  (t/is (= 29878 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 855438643439 (time (part-2-solver input)))))

