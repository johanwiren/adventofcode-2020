(ns adventofcode-2015.day25)

(def input {:row 2981 :column 3075})

(defn codes []
  (iterate (fn [x]
             (rem (* x 252533) 33554393))
           20151125))

(defn nth-triangle [n]
  (/ (* n (inc n)) 2))

(defn code-pos [row col]
  (- (nth-triangle (+ (dec row) col)) (dec row)))

(defn part-1-solver [input]
  (let [{:keys [row column]} input]
    (nth (codes) (dec (code-pos row column)))))
