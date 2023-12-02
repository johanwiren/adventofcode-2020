(ns adventofcode-2017.day-18
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [utils :as u]))

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

(defn get-val [cpu x]
  (if (keyword? x) (get cpu x) x))

(defn my-mod [cpu r x]
  (let [x (get-val cpu x)]
    (assoc cpu r (mod (get cpu r) x))))

(def impl {:set (fn [cpu r x] (assoc cpu r (get-val cpu x)))
           :mul (fn [cpu r x] (update cpu r * x))
           :jgz (fn [cpu x y]
                  (let [x (get-val cpu x)
                        y (get-val cpu y)]
                    (if (pos? x)
                      (update cpu :ip + y)
                      (update cpu :ip inc))))
           :add (fn [cpu r x] (let [x (get-val cpu x)]
                                (update cpu r + x)))
           :mod #'my-mod
           :rcv (fn [cpu r] (when (not (zero? (get cpu r)))
                              (assoc cpu :rcv (get cpu :snd))))
           :snd (fn [cpu x] (assoc cpu :snd (get cpu x)))})

(defn step [{:keys [ip instrs impl] :as cpu}]
  (let [[instr & args] (get instrs ip)
        instr-impl (impl instr)]
    (cond-> (apply (partial instr-impl cpu) args)
      (not (#{:jgz} instr)) (update :ip inc))))

(defn run [cpu]
  (->> (iterate step cpu)
       (drop-while (comp nil? :rcv))
       (first)))

(defn init-cpu [input]
  (merge {:impl impl
          :instrs (parse-input input)
          :ip 0}
         (zipmap (map (comp keyword str) "abcdefghijklmnop") (repeat 0))))

(defn part-1-solver [input]
  (-> (init-cpu input)
      (run)
      (:rcv)))

(defn current-instr [{:keys [ip instrs]}]
  (get-in instrs [ip 0]))

(defn running? [{:keys [rcv] :as cpu}]
  (or (not= :rcv (current-instr cpu))
      (seq rcv)))

(defn run-until-rcv-block [cpu]
  (->> (iterate step cpu)
       (drop-while running?)
       (first)))

(defn ipcs-cpu [cpu]
  (-> cpu
      (assoc :sent 0)
      (assoc :snd clojure.lang.PersistentQueue/EMPTY)
      (assoc :rcv clojure.lang.PersistentQueue/EMPTY)
      (assoc-in [:impl :rcv] (fn [cpu r] (-> cpu
                                             (assoc r (peek (:rcv cpu)))
                                             (update :rcv pop))))
      (assoc-in [:impl :snd] (fn [cpu r]
                               (update cpu :snd conj (get cpu r))))))

(defn ipcs-step [{:keys [running] :as system}]
  (let [next-cpu ({:cpu0 :cpu1 :cpu1 :cpu0} running)
        blocked-state (update system running run-until-rcv-block)
        snd (get-in blocked-state [running :snd])]
    (-> blocked-state
        (update-in [running :sent] + (count snd))
        (assoc-in [running :snd] clojure.lang.PersistentQueue/EMPTY)
        (update-in [next-cpu :rcv] into snd)
        (assoc :running next-cpu))))

(defn sys-running? [{:keys [cpu0 cpu1]}]
  (or (running? cpu0)
      (running? cpu1)))

(defn part-2-solver [input]
  (let [[cpu0 cpu1] (map (fn [n] (-> (init-cpu input)
                                     (ipcs-cpu)
                                     (assoc :p n)))
                         [0 1])]
    (->> (iterate ipcs-step {:running :cpu0
                             :cpu0 cpu0
                             :cpu1 cpu1})
         (drop-while sys-running?)
         (first)
         (:cpu1)
         (:sent))))
