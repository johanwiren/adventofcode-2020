(ns adventofcode-2015.day17
  (:require [utils :as u]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn parse-input [input]
  (mapv parse-long input))

(defn find-150-combos [buckets]
  (let [ns (range (count buckets))]
         (loop [q (into [] (map (fn [n]
                                  {:sum (get buckets n)
                                   :combo #{n}})
                                ns))
                seen-combos #{}
                found-combos #{}]
           (if-not (seq q)
             found-combos
             (let [{:keys [combo sum]} (peek q)
                   new-gen (->> ns
                                (map (fn [n]
                                       {:sum (+ sum (get buckets n))
                                        :combo (conj combo n)}))
                                (remove (comp seen-combos :combo)))]
               (recur (into (pop q)
                            (remove (comp (partial <= 150) :sum))
                            new-gen)
                      (into seen-combos
                            (map :combo)
                            new-gen)
                      (into found-combos
                            (filter (comp (partial = 150) :sum))
                            new-gen)))))))

(defn part-1-solver [input]
  (->> (parse-input input)
       (find-150-combos)
       (count)))

(defn part-2-solver [input]
  (->> (parse-input input)
       (find-150-combos)
       (map (comp count :combo))
       (frequencies)
       (sort-by first)
       (first)
       (second)))

(t/deftest part-1-solver-test
  (t/is (= 654 (part-1-solver input))))

(t/deftest part-2-solver-test
  (t/is (= 57 (part-2-solver input))))
