(ns adventofcode-2016.day12
  (:require [utils :as u]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (let [[instr & rest] (str/split line #"[ ,]+")]
    (into [(keyword instr)]
          (comp (map edn/read-string)
                (map (fn [x]
                       (if (symbol? x)
                         (keyword x)
                         x))))
          rest)))

(defn parse-input [input]
  (mapv parse-line input))

(def impl {:cpy (fn [cpu x y] (assoc cpu y (if (keyword? x) (get cpu x) x)))
           :inc (fn [cpu x] (update cpu x inc))
           :dec (fn [cpu x] (update cpu x dec))
           :jnz (fn [cpu x y] (if (not (zero? (if (keyword? x) (get cpu x) x)))
                                (update cpu :ip + (if (keyword? y) (get cpu y) y))
                                (update cpu :ip inc)))})

(defn step [{:keys [ip instrs impl] :as cpu}]
  (let [[instr & args] (get instrs ip)
        instr-impl (impl instr)]
    (cond-> (apply (partial instr-impl cpu) args)
      (not (#{:jnz} instr)) (update :ip inc))))

(defn steps [cpu]
  (iterate step cpu))

(defn run [cpu]
  (->> (iterate step cpu)
       (drop-while (fn [{:keys [ip instrs]}]
                     (<= 0 ip (dec (count instrs)))))
       (first)))

(defn init-cpu [input]
  {:impl impl
   :instrs (parse-input input)
   :ip 0
   :a 0
   :b 0
   :c 0
   :d 0})

(defn part-1-solver [input]
  (-> (init-cpu input)
      (run)
      (:a)))

(defn part-2-solver [input]
  (-> (init-cpu input)
      (assoc :c 1)
      (run)
      (:a)))
