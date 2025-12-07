(ns adventofcode-2025.day-07
  (:require
   [clojure.test :as t]
   [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse [input]
  (mapv (partial mapv {\. 0 \S 1 \^ \^}) input))

(defn merge-rows [r1 r2]
  (let [cols (count r1)
        m (meta r1)]
    (loop [i 0
           res r2
           splits 0]
      (if (< cols i)
        (with-meta res {:part-1/splits (+ (:part-1/splits m 0) splits)})
        (let [prev (get r1 i)
              curr (get r2 i)
              has-beam? (and (number? prev)
                             (pos? prev))
              split? (and has-beam?
                          (= \^ curr))
              res (cond
                    split?
                    (-> res
                        (update (dec i) + prev)
                        (update (inc i) + prev))

                    has-beam?
                    (update res i + prev)

                    :else
                    res)
              hits (if split? (inc splits) splits)]
          (recur (inc i) res hits))))))

(defn part-1-solver [input]
  (-> (reduce merge-rows (parse input))
      (meta)
      (:part-1/splits)))

(defn part-2-solver [input]
  (->> (parse input)
       (reduce merge-rows)
       (filter number?)
       (reduce +)))

(t/deftest part-1-test
  (t/is (= 1600 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 8632253783011 (part-2-solver input))))
