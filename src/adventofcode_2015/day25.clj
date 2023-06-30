(ns adventofcode-2015.day25)

(def input {:row 2981 :column 3075})

(defn codes []
  (iterate (fn [x]
             (rem (* x 252533) 33554393))
           20151125))

(defn n-seq [n i]
  (iterate (fn [[n i]]
             [(+ n i) (inc i)])
           [n i]))

(defn code-pos [row col]
  (let [col-1 (n-seq 1 1)
        [n i] (nth col-1 (dec row))]
    (first (nth (n-seq n (inc i)) (dec col)))))

(defn part-1-solver [input]
  (let [{:keys [row column]} input]
    (nth (codes) (dec (code-pos row column)))))



