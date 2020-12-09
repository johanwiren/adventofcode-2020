(ns adventofcode-2020.gcvm)

(defn apply-op [{:keys [ops ip] :as cpu}]
  (let [[op arg] (nth ops ip)
        next-cpu (-> cpu
                     (update :ip inc)
                     (update :ticks inc))]
    (case op
      :nop next-cpu
      :acc (update next-cpu :acc + arg)
      :jmp (let [new-ip (+ ip arg)]
             (cond-> (assoc next-cpu :ip new-ip)
               (nil? (get ops new-ip))
               (assoc :state :finished))))))

(defn step [{:keys [ip ops loop-detect] :as cpu}]
  (cond
    (loop-detect ip) (assoc cpu :state :inf-loop)
    (get ops ip) (-> cpu
                     (update :loop-detect conj ip)
                     apply-op)
    :else (assoc cpu :state :finished)))

(defn states [cpu]
  (iterate step cpu))

(defn run [cpu]
  (->> cpu
       (states)
       (drop-while (comp #{:running} :state))
       (first)))

(defn cpu [ops]
  {:ip 0
   :ticks 0
   :acc 0
   :ops (vec ops)
   :loop-detect #{}
   :state :running})
