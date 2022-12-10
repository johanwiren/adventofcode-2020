(ns adventofcode-2022.day-10
  (:require [adventofcode-2022.utils :as u]
            [clojure.string :as str]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (let [[op val] (str/split line #" ")]
    (cond-> [(keyword op)]
      val (conj (parse-long val)))))

(defn parse-input [input]
  (map parse-line input))

(defn run-instr [{xs :xs [[instr val] & _instrs] :instrs :as state}]
  (cond-> (-> state
              (update :xs concat [(last xs)])
              (update :cycle inc)
              (update :instrs next))
    (= :addx instr)
    (-> (update :xs concat [(+ (last xs) val)])
        (update :cycle inc))))

(defn part-1-solver [input]
  (let [cycles (take 6 (iterate (partial + 40) 20))
        xs     (->> {:xs (list 1) :cycle 0 :instrs (parse-input input)}
                    (iterate run-instr)
                    (take-while (comp (partial > (last cycles)) :cycle))
                    (last)
                    (:xs))]
    (->> cycles
         (map (fn [cycle]
                (* cycle (last (take cycle xs)))))
         (reduce +))))

(defn part-2-solver [input]
  (->> {:xs (list 1) :cycle 0 :instrs (parse-input input)}
       (iterate run-instr)
       (take-while (comp (partial > 240) :cycle))
       (last)
       (:xs)
       (partition 40)
       (map (partial map-indexed (fn [i x]
                                   (if (<= (dec i) x (inc i))
                                     "#"
                                     " "))))
       (map (partial apply str))))

(t/deftest part-1-test
  (t/is (= 14760 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= ["#### ####  ##  #### ###  #  # ###  #### "
            "#    #    #  # #    #  # #  # #  # #    "
            "###  ###  #    ###  #  # #  # #  # ###  "
            "#    #    # ## #    ###  #  # ###  #    "
            "#    #    #  # #    # #  #  # # #  #    "
            "#### #     ### #### #  #  ##  #  # #### "]
           (time (part-2-solver input)))))
