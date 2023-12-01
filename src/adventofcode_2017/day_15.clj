(ns adventofcode-2017.day-15)

(def input [703 516])

(defn generate [factor init]
  (iterate (fn [x]
             (rem (* factor x) 2147483647))
           init))

(def generate-a (partial generate 16807))
(def generate-b (partial generate 48271))

(defn part-1-solver [input]
  (let [[a b] input]
    (->> (map list
              (map (partial bit-and 65535) (generate-a a))
              (map (partial bit-and 65535) (generate-b b)))
         (take (inc 40000000))
         (filter (partial apply =))
         (count))))

(defn part-2-solver [input]
  (let [[a b] input]
    (->> (map list
              (map (partial bit-and 65535) (filter (comp zero? #(mod % 4)) (generate-a a)))
              (map (partial bit-and 65535) (filter (comp zero? #(mod % 8)) (generate-b b))))
         (take (inc 5000000))
         (filter (partial apply =))
         (count))))
