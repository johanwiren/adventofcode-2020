(ns adventofcode-2022.day-25
  (:require [utils :as u]
            [clojure.set :as set]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(def to-snafu {5 \0, 0 \0, 1 \1, 2 \2, 3 \=, 4 \-})

(def from-snafu (set/map-invert to-snafu))

(defn snafu [x]
  (loop [snafu ""
         [digit & more] (->> (Long/toString x 5)
                             (map (comp parse-long str))
                             reverse)
         carry false]
    (if (or digit carry)
      (let [digit (if carry ((fnil inc 0) digit) digit)
            snafu-digit (to-snafu digit)]
        (recur (str snafu-digit snafu) more (<= 3 digit)))
      snafu)))

(defn number [snafu]
  (loop [number ""
         [snafu-digit & more] (reverse snafu)
         carry false]
    (if snafu-digit
      (let [base5-digit (from-snafu snafu-digit)
            digit (cond
                    (and carry (= 0 base5-digit)) 4
                    carry (dec base5-digit)
                    :else base5-digit)]
        (recur (str digit number) more (or (#{\= \-} snafu-digit)
                                           (and carry (= 0 base5-digit)))))
      (Long/parseLong number 5))))

(defn part-1-solver [input]
  (->> input
       (map number)
       (reduce +)
       (snafu)))

(t/deftest part-1-test
  (t/is (= "122-12==0-01=00-0=02" (time (part-1-solver input)))))

(t/deftest snafu-test
  (dotimes [_ 1000]
    (let [n (long (* Long/MAX_VALUE (rand)))]
      (t/is (= n (-> n snafu number))))))
