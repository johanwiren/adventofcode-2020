(ns adventofcode-2016.day23
  (:require [utils :as u]
            [clojure.math :as math]
            [adventofcode-2016.day12 :as bunny]))

(def input (u/line-seq-input *ns*))

(def toggle {:inc :dec
             :dec :inc
             :jnz :cpy
             :cpy :jnz
             :tgl :inc})

(def impl
  {:tgl (fn [{:keys [ip] :as cpu} x]
          (let [addr (+ (get cpu x) ip)]
            (cond-> cpu
              (and addr (get-in cpu [:instrs addr]))
              (update-in [:instrs addr 0] toggle))))
   :jnz (fn [cpu x y] (if (not (zero? (if (keyword? x) (get cpu x) x)))
                   (update cpu :ip + (if (keyword? y) (get cpu y) y))
                   (do
                     (println "SKIP JUMP" [x y] (dissoc cpu :instrs :impl))
                     (update cpu :ip inc))))})

(defn part-1-solver [input]
  (-> (bunny/init-cpu input)
      (update :impl merge impl)
      (assoc :a 7)
      (bunny/run)
      :a))

(defn part-2-solver
  ;; Observing the run with a=7 led to this
  ([]
   (part-2-solver 12))
  ([a]
   (let [b (dec a)]
     (loop [acc (* a b)
            b (dec b)]
       (if (zero? b)
         (+ acc (* 72 75))
         (recur (* acc b) (dec b)))))))

(comment
  ;; Check where the code loops
  (-> (bunny/init-cpu input)
      (update :impl merge impl)
      (assoc :a 7 #_12)
      (->> (iterate bunny/step)
           (take-while (fn [{:keys [ip instrs]}]
                         (<= 0 ip (dec (count instrs)))))
           (take 10000000)
           (map (fn [{:keys [ip instrs]}] [ip (get instrs ip)]))
           (frequencies)
           (into (sorted-map)))))

