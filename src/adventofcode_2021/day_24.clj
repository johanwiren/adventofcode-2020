(ns adventofcode-2021.day-24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.test :as t]))

(def input (->> "2021/day_24.txt"
                (io/resource)
                (io/reader)
                (line-seq)))

(defn parse-line [line]
  (let [[instr & args] (str/split line #" ")]
    {(keyword instr) (mapv edn/read-string args)}))

(defn parse-input [input]
  (map parse-line input))

(defn step [z W X Y Z]
  (let [check (not= (+ (mod z 26) X) W)]
    (cond-> (quot z Z)
      check (-> (* 26)
               (+ Y W)))))

;; D4 = D3 + 12 - 6
;; D7 = D6 + 16 - 9
;; D10 = D9 + 8 - 5
;; D11 = D8 + 7 - 9
;; D12 = D5 + 6 - 5
;; D13 = D2 + 10 - 2
;; D14 = D1 + 4 - 7
;; Highest
;; 9............6
;; 91..........96
;; 91..8......996
;; 91..8..9..7996
;; 91..8..9967996
;; 91..8299967996
;; 91368299967996
;; Lowest
;; 4............1
;; 41..........91
;; 41..1......291
;; 41..1..3..1291
;; 41..1..3141291
;; 41..1183141291
;; 41171183141291

(defn step-diffs [input]
  (->> input
       (partition 18)
       (map (juxt #(nth % 4) #(nth % 5) #(nth % 15)))))

(defn to-vars [step-diffs]
  (mapv (comp (partial into {} (map (fn [[k v]]
                                      [(keyword (str k)) v])))
              (partial mapcat vals))
        step-diffs))

(defn n-seq [n]
  (into []
        (map (partial #(rem % 10)))
        (->> n
             (iterate #(quot % 10))
             (take-while pos?)
             (reverse))))

(defn validate [n vars]
  (->> vars
       (map vector (n-seq n))
       (reduce (fn [Z [n {:keys [z x y]}]]
                 (step Z n x y z))
               0)
       (zero?)))

(t/deftest part-1
  (t/is (->> (parse-input input)
             (step-diffs)
             (to-vars)
             (validate 91398299697996))))

(t/deftest part-2
  (t/is (->> (parse-input input)
             (step-diffs)
             (to-vars)
             (validate 41171183141291))))
