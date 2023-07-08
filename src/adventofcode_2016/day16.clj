(ns adventofcode-2016.day16)

(def input "10001110011110000")

(defn parse-input [input]
  (mapv parse-long (re-seq #"\d" input)))

(defn dragon [n]
  (into (conj n 0) (mapv (fn [x] (if (= 1 x) 0 1)) (rseq n))))

(defn dragon-curve [d size]
  (subvec (->> (iterate dragon d)
               (drop-while (comp (partial > size) count))
               (first))
          0
          size))

(defn chksum [d]
  (if (odd? (count d))
    d
    (chksum (mapv (fn [[a b]]
                    (if (= a b) 1 0))
                  (partition 2 d)))))

(defn solver [input n]
  (let [chksum (-> (parse-input input)
                   (dragon-curve n)
                   (chksum))]
    (apply str chksum)))

(defn part-1-solver [input]
  (time (solver input 272)))

(defn part-2-solver [input]
  (time (solver input 35651584)))
