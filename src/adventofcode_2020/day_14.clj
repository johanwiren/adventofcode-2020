(ns adventofcode-2020.day-14
  (:require [adventofcode-2020.util :refer [slurp-input slurp-reference-input]]
            [clojure.string :as str]
            [clojure.test :as t]))

(def reference-input (slurp-reference-input))

(def input (slurp-input))

(defn parse-line [line]
  (case (str/starts-with? line "mask")
    true [:mask (apply str (re-seq #"[01X]" line))]
    false [:mem (let [[_ addr val] (re-matches #"mem\[(\d+)\] = (\d+)" line)]
                  [(Integer/parseInt addr) (Integer/parseInt val)])]))

(defn parse-input [in]
  (->> in
       (map parse-line)))

(defn ->and-not-mask [mask]
  (-> mask
      (str/replace "0" "K")
      (str/replace #"[1X]" "0")
      (str/replace "K" "1")
      (Long/parseLong 2)))

(defn ->and-mask [mask]
  (-> mask
      (str/replace "X" "1")
      (Long/parseLong 2)))

(defn ->or-mask [mask]
  (-> mask
      (str/replace "X" "0")
      (Long/parseLong 2)))

(defn set-mask [acc mask-str]
  (assoc acc
         :or-mask (->or-mask mask-str)
         :and-mask (->and-mask mask-str)))

(defn parse-mask [mask]
  (Long/parseLong mask 2))

(defn set-mem [{:keys [or-mask and-mask] :as acc} [addr val]]
  (let [v (-> val
              (bit-or or-mask)
              (bit-and and-mask))]
    (assoc-in acc
              [:mem addr] v)))

(defn part-1-solver [in]
  (->> (parse-input in)
       (reduce (fn [acc [line-type line-data]]
                 (case line-type
                   :mask (set-mask acc line-data)
                   :mem (set-mem acc line-data)))
               {:or-mask 0
                :and-mask 0
                :mem {}})
       (:mem)
       (vals)
       (apply +)))

(defn combos [mask]
  (let [loc (str/index-of mask "X")]
    (if loc
      (flatten [(combos (str/replace-first mask "X" 0))
                (combos (str/replace-first mask "X" 1))])
      mask)))

(defn add-xes [addr mask]
  (->> (concat (reverse addr) (repeat \0))
       (map (fn [m a]
              (if (= \X m)
                \X
                a))
            (reverse mask))
       (reverse)
       (apply str)))

(defn set-mem-2 [{:keys [mask] :as acc} [addr val]]
  (let [addr (Long/toBinaryString (bit-or addr (->or-mask mask)))
        masked-addr (add-xes addr mask)
        addrs (combos masked-addr)]
    (reduce (fn [acc addr']
              (assoc-in acc [:mem addr'] val))
            acc
            addrs)))

(defn part-2-solver [in]
  (->> (parse-input in)
       (reduce (fn [acc [line-type line-data]]
                 (case line-type
                   :mask (assoc acc :mask line-data)
                   :mem (set-mem-2 acc line-data)))
               {:mem {}})
       (:mem)
       (vals)
       (apply +)))

(def p2-reference-input
  ["mask = 000000000000000000000000000000X1001X"
   "mem[42] = 100"
   "mask = 00000000000000000000000000000000X0XX"
   "mem[26] = 1"])

(t/deftest part-1-test
  (t/is (= 165 (part-1-solver reference-input))))

(t/deftest part-2-test
  (t/is (= 208 (part-2-solver p2-reference-input)))
  (t/is (= 3442819875191 (time (part-2-solver input)))))
