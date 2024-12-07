(ns adventofcode-2024.day-07
  (:require
   ;; [clojure.math :as math]
   [clojure.string :as str]
   [clojure.test :as t]
   [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse [input]
  (map (fn [line]
         (let [[res & ns]
               (map parse-long (str/split line #" |: "))]
           {:res res
            :ns ns}))
       input))

(defn add-ops [ns ops]
  (reduce (fn [eqns n]
            (mapcat
             (fn [eqn]
               (mapv
                (fn [op]
                  (into eqn [op n]))
                ops))
             eqns))
          [(vec (take 1 ns))]
          (rest ns)))

(defn concat-longs [a b]
  ;; (long (+ (* (math/pow 10 (inc (math/floor (math/log10 b)))) a) b))
  (parse-long (str a b)))

(defn calc [expr]
  (reduce (fn [acc [op n]]
            (if (= :| op)
              (concat-longs acc n)
              (op acc n)))
          (first expr)
          (partition 2 (rest expr))))

(defn solve [{:keys [res ns]} ops]
  (some (fn [expr]
          (= res (calc expr)))
        (add-ops ns ops)))

(defn part-1-solver [input]
  (->> (parse input)
       (filter #(solve % [+ *]))
       (map :res)
       (reduce +)))

(defn part-2-solver [input]
  (->> (parse input)
       (pmap (juxt identity #(solve % [+ * :|])))
       (filter second)
       (map (comp :res first))
       (reduce +)))

(t/deftest part-1-test
  (t/is (= 7710205485870 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 20928985450275 (part-2-solver input))))
