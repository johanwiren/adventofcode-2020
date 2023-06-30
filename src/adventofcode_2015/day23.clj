(ns adventofcode-2015.day23
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

(def impl {:hlf (fn [cpu r] (update cpu r / 2))
           :tpl (fn [cpu r] (update cpu r * 3))
           :inc (fn [cpu r] (update cpu r inc))
           :jmp (fn [cpu offset] (update cpu :ip + offset))
           :jie (fn [cpu r offset] (if (even? (get cpu r))
                                     (update cpu :ip + offset)
                                     (update cpu :ip inc)))
           :jio (fn [cpu r offset] (if (#{1} (get cpu r))
                                     (update cpu :ip + offset)
                                     (update cpu :ip inc)))})

(defn step [{:keys [ip instrs] :as cpu}]
  (let [[instr & args] (get instrs ip)
        instr-impl (impl instr)]
    (cond-> (apply (partial instr-impl cpu) args)
      (not (#{:jmp :jie :jio} instr)) (update :ip inc))))

(defn run [cpu]
  (->> (iterate step cpu)
       (drop-while (fn [{:keys [ip instrs]}]
                     (<= 0 ip (dec (count instrs)))))
       (first)))

(defn init-cpu [input]
  {:instrs (parse-input input)
   :ip 0
   :a 0
   :b 0})

(defn part-1-solver [input]
  (-> (init-cpu input)
      (run)
      (:b)))

(defn part-2-solver [input]
  (-> (init-cpu input)
      (assoc :a 1)
      (run)
      (:b)))
