(ns adventofcode-2017.day-17)

(def input 301)

(defn spin [steps]
  (let [[buf pos]
        (->> [[0] 0 1]
             (iterate (fn [[buf pos val]]
                        (let [insert-pos (inc (mod (+ steps pos) (count buf)))]
                          [(-> (subvec buf 0 insert-pos)
                               (conj val)
                               (into (subvec buf insert-pos)))
                           insert-pos
                           (inc val)])))
             (drop 2017)
             (first))]
    (get buf (inc pos))))

(defn fast-spin [input]
  (loop [pos 0
         val 1
         tgt nil]
    (if (< 50000000 val)
      tgt
      (let [insert-pos (inc (mod (+ input pos) val))]
        (recur insert-pos (inc val) (if (= 1 insert-pos) val tgt))))))

(defn part-1-solver [input]
  (spin input))

(defn part-2-solver [input]
  (fast-spin input))
