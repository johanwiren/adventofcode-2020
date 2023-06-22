(ns adventofcode-2021.day-10
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))

(def input (->> "2021/day_10.txt"
                (io/resource)
                (io/reader)
                (line-seq)))

(def opens [\( \[ \< \{])
(def closes [\) \] \> \}])

(def close (set closes))

(def opener (zipmap closes opens))
(def closer (zipmap opens closes))

(def score (zipmap closes [3 57 25137 1197]))

(defn error-or-completion [chars]
  (loop [opens (list)
         chars chars]
    (let [[char & more] chars]
      (if (and (close char)
               (not= (first opens) (opener char)))
        char
        (let [opens (if (close char)
                      (drop 1 opens)
                      (cons char opens))]
          (if-not (seq more)
            opens
            (recur opens more)))))))

(defn get-error [chars]
  (let [eoc (error-or-completion chars)]
    (when-not (seq? eoc)
      eoc)))

(defn part-1-solver [input]
  (->> input
       (map seq)
       (keep get-error)
       (map score)
       (reduce +)))

(defn close-seq [chars]
  (map closer chars))

(def completion-score (zipmap closes [1 2 4 3]))

(defn score-completion [chars]
  (loop [score 0
         [char & more] chars]
    (if-not char
      score
      (recur (+ (* 5 score)
                (completion-score char))
             more))))

(defn middle-item [seq]
  (nth seq (quot (count seq) 2)))

(defn part-2-solver [input]
  (->> input
       (map seq)
       (map error-or-completion)
       (filter seq?)
       (map close-seq)
       (map score-completion)
       (sort)
       (middle-item)))

(t/deftest part-1
  (t/is (= 290691 (part-1-solver input))))

(t/deftest part-2
  (t/is (= 2768166558 (part-2-solver input))))
