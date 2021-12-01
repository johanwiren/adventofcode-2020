(ns adventofcode-2021.day-01
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.test :as t]))

(def input (-> "2021/day_01.txt" (io/resource) (io/reader) (line-seq)))

(defn parse-input [input]
  (map edn/read-string input))

(defn add-diff [rf]
  (let [prev (volatile! nil)]
    (fn
      ([] (rf))
      ([res] (rf res))
      ([res item]
       (let [diff (if @prev
                    (- item @prev)
                    0)]
         (vreset! prev item)
         (rf res [item diff]))))))

(defn- part-1 [in]
  (->> in
       (sequence (comp add-diff
                       (filter (comp pos? second))))
       (count)))

(defn part-1-solver [input]
  (let [in (parse-input input)]
    (part-1 in)))

(defn sliding-window
  ([n]
   (let [window (volatile! [])]
     (fn [rf]
       (fn
         ([] (rf))
         ([res] (rf res))
         ([res item]
          (if (= n (count @window))
            (do
              (vreset! window (-> @window (subvec 1) (conj item)))
              (rf res @window))
            (vswap! window conj item)))))))
  ([n xs]
   (sequence (sliding-window n) xs)))

(defn part-2-solver [input]
  (let [in (parse-input input)]
    (->> in
         (sequence (comp (sliding-window 3)
                         (map (partial apply +))))
         (part-1))))

(comment
  (part-1-solver input)

  (part-2-solver input)

  )

(t/deftest part-1-test
  (t/is (= 1228 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 1257 (part-2-solver input))))
