(ns adventofcode-2016.day08
  (:require [utils :as u]
            [clojure.string :as str]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (-> (mapv #(or (parse-long %) %) (str/split line #" by |[ =]"))
      (update 0 keyword)))

(defn parse-input [input]
  (map parse-line input))

(defn screen [rows cols]
  {:rows rows
   :cols cols
   :n-pixels (* rows cols)
   :pixels (vec (repeat (* cols rows) false))})

(def default-screen
  (screen 6 50))

(defn row-locs [{:keys [cols]} n]
  (range (* n cols) (* (inc n) cols)))

(defn col-locs [{:keys [n-pixels cols]} n]
  (range n (inc n-pixels) cols))

(defn rotator [coll n]
  (let [[hd tl] (split-at n coll)]
    (into (vec tl) hd)))

(defn light-up [pixels locs]
  (reduce (fn [pixels loc]
            (assoc pixels loc true))
          pixels
          locs))

(defn rotate-pixels [pixels locs n]
  (reduce (fn [pixels [v pos]]
            (assoc pixels pos v))
          pixels
          (map vector (map #(get pixels %) locs) (rotator locs n))))

(defn rect [{:keys [cols] :as screen} rect-spec]
  (let [[max-col max-row] (map parse-long (str/split rect-spec #"x"))
        locs (for [col (range max-col)
                   row (range max-row)]
               (+ (* cols row) col))]
    (update screen :pixels light-up locs)))

(defn rotate-col [screen col-n n]
  (update screen :pixels rotate-pixels (col-locs screen col-n) n))

(defn rotate-row [screen row-n n]
  (update screen :pixels rotate-pixels (row-locs screen row-n) n))

(defn rotate [screen loc-type _ loc n]
  (if (= "column" loc-type)
    (rotate-col screen loc n)
    (rotate-row screen loc n)))

(defn process-instr [screen [op & args]]
  (case op
    :rect (rect screen (first args))
    :rotate (apply rotate screen args)))

(defn part-1-solver [input]
  (->> (parse-input input)
       (reduce process-instr default-screen)
       (:pixels)
       (filter true?)
       (count)))

(defn screen-lines [{:keys [cols pixels]}]
  (->> pixels
       (map {false "." true "#"})
       (partition cols)))

(defn part-2-solver [input]
  (->> (parse-input input)
       (reduce process-instr default-screen)
       (screen-lines)
       (run! println)))
