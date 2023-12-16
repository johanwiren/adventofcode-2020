(ns adventofcode-2023.day-14
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn compress-left [row]
  (->> (partition-by #{\#} row)
       (partition-all 2)
       (reduce (fn [acc [squares rounds+dots]]
                 (let [n-zeros (count (filter #{\O} rounds+dots))]
                   (into acc (concat squares
                                     (repeat n-zeros \O)
                                     (repeat (- (count rounds+dots) n-zeros) \.)))))
               [])))

(defn part-1-solver [input]
  (let [n-cols (count (first input))]
    (->> (into [(vec (repeat n-cols \#))] input)
         (u/transpose)
         (map compress-left)
         (u/transpose)
         (rest)
         (reverse)
         (map-indexed vector)
         (reduce (fn [acc [i row]]
                   (+ acc (* (inc i) (count (filter #{\O} row)))))
                 0))))

(defn tilt-east [rocks]
  (mapv (fn [row]
          (->> (partition-by #{\#} row)
               (map sort)
               (apply concat)
               (into [])))
        rocks))

(defn tilt-west [rocks]
  (mapv (fn [row]
          (->> (partition-by #{\#} row)
               (map (partial sort (comp - compare)))
               (apply concat)
               (into [])))
        rocks))

(defn tilt-north [rocks]
  (-> rocks
      u/transpose
      tilt-west
      u/transpose))

(defn tilt-south [rocks]
  (-> rocks
      u/transpose
      tilt-east
      u/transpose))

(defn tilt-cycle [rocks]
  (-> rocks
      tilt-north
      tilt-west
      tilt-south
      tilt-east))

(defn north-beam-load [rocks]
  (->> rocks
       (rseq)
       (map-indexed vector)
       (reduce (fn [acc [i row]]
                 (+ acc (* (inc i) (count (filter #{\O} row)))))
               0)))

(defn part-2-solver [input]
  (let [rocks (mapv vec input)
        [cycle-start cycle-3-loads]
        (->> rocks
             (iterate tilt-cycle)
             (map north-beam-load)
             (partition 3 1)
             (map-indexed vector)
             (reduce (fn [seen [i loads]]
                       (if (seen loads)
                         (reduced [i loads])
                         (conj seen loads)))
                     #{}))
        full-cycle (->> rocks
                        (iterate tilt-cycle)
                        (drop (inc cycle-start))
                        (map north-beam-load)
                        (partition 3 1)
                        (take-while (partial not= cycle-3-loads))
                        (map first)
                        (cons (first cycle-3-loads)))]
    (nth full-cycle (rem (- 1000000000 cycle-start) (count full-cycle)))))
