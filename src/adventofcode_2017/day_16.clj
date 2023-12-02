(ns adventofcode-2017.day-16
  (:require [clojure.string :as str]
            [utils :as u]))

(def input (first (u/line-seq-input *ns*)))

(defn parse-input [input]
  (->> (str/split input #",")
       (map (fn [cmd]
              (case (first cmd)
                \x (into [:x] (map parse-long (re-seq #"\d+" (subs cmd 1))))
                \p (into [:p] (map keyword (re-seq #"\w+" (subs cmd 1))))
                \s [:s (parse-long (subs cmd 1))]
                nil)))))

(defn partner [programs p1 p2]
  (let [idx (zipmap programs (range))]
    (-> programs
        (assoc (get idx p1) (get programs (get idx p2)))
        (assoc (get idx p2) (get programs (get idx p1))))))

(defn dance [programs instructions]
  (let [n-programs (count programs)
        wrap (fn [x] (mod x n-programs))
        programs (mapv (comp keyword str) programs)
        danced
        (->> instructions
             (reduce (fn [{:keys [offset programs] :as state} [cmd a1 a2]]
                       (case cmd
                         :s (assoc state :offset (wrap (- (wrap (+ n-programs offset)) a1)))
                         :x (-> state
                                (assoc-in [:programs (wrap (+ offset a1))] (get programs (wrap (+ offset a2))))
                                (assoc-in [:programs (wrap (+ offset a2))] (get programs (wrap (+ offset a1)))))
                         :p (update state :programs partner a1 a2)))
                     {:offset 0
                      :programs programs}))
        output (apply str (map name (:programs danced)))]
    (str (subs output (:offset danced))
         (subs output 0 (:offset danced)))))

(defn part-1-solver [input]
  (dance "abcdefghijklmnop" (parse-input input)))

(defn dance-cycle [input]
  (let [init "abcdefghijklmnop"
        dance #(dance % input)]
    (->> init
         (dance)
         (iterate dance)
         (take-while (partial not= init))
         (cons init))))

(defn part-2-solver [input]
  (let [input (parse-input input)
        cycle (dance-cycle input)]
    (nth cycle (rem 1000000000 (count cycle)))))
