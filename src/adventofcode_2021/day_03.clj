(ns adventofcode-2021.day-03
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))


(def input (->> "2021/day_03.txt"
                (io/resource)
                (io/reader)
                (line-seq)))

(defn bit-analysis [bits]
  (let [freqs (frequencies bits)
        equal? (->> freqs vals (apply =))]
    (if equal?
      :equal
      (->> freqs
           (sort-by second)
           (keys)
           (zipmap [:least-common :most-common])))))

(defn parse-binary [bin-str]
  (Integer/parseInt bin-str 2))

(defn transpose [xs]
  (apply (partial map vector) xs))

(defn part-1-solver [input]
  (->> input
       (transpose)
       (map bit-analysis)
       (map vals)
       (transpose)
       (map (partial apply str))
       (map parse-binary)
       (apply *)))

(defn bit-criteria [criteria input]
  (reduce (fn [acc n]
            (let [get-bit #(nth % n)
                  analysis (bit-analysis (apply str (map get-bit acc)))
                  interesting-bit (if (= :equal analysis)
                                    (case criteria
                                      :most \1
                                      :least \0)
                                    (case criteria
                                      :most (:most-common analysis)
                                      :least (:least-common analysis)))
                  filtered (filter (comp (set [interesting-bit]) get-bit) acc)]
              (if (= 1 (count filtered))
                (reduced (first filtered))
                filtered)))
          input
          (range (count (first input)))))

(defn part-2-solver [input]
  (* (parse-binary (bit-criteria :most input))
     (parse-binary (bit-criteria :least input))))

(t/deftest part-1
  (t/is (= 1082324 (part-1-solver input))))

(t/deftest part-2
  (t/is (= 1353024 (part-2-solver input))))
