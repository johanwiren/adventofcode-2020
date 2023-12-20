(ns adventofcode-2023.day-20
  (:require [clojure.string :as str]
            [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (let [[module signals] (str/split line #" -> ")
        type ({\% :flip-flop \& :conjunction} (#{\% \&} (first module)))
        id (cond-> module
             type (subs 1)
             :always keyword)
        destinations (mapv keyword (str/split signals #", "))]
    {:id id :type (or type :low) :destinations destinations}))

(defn parse-input [input]
  (into {}
        (comp (map parse-line)
              (map (juxt :id identity)))
        input))

(defn initialize [modules]
  (-> modules
      (update-vals (fn [{:keys [id type] :as module}]
                     (cond-> module
                       (= :flip-flop type)
                       (assoc :state false)

                       (= :conjunction type)
                       (assoc :state (->> (vals modules)
                                          (filter (comp (partial some #{id}) :destinations))
                                          (map (fn [{:keys [id]}] [id false]))
                                          (into {}))))))))

(defn send-signal [{:keys [id type destinations state] :as module} {:keys [from value] :as signal}]
  (case type

    :flip-flop
    {:module (cond-> module
               (false? value) (update :state not))
     :signals (if (false? value)
                (mapv (fn [signal] {:from id :to signal :value (not state)}) destinations)
                [])}

    :conjunction
    (let [state (assoc state from value)]
      {:module (assoc module :state state)
       :signals (let [value (not-every? true? (vals state))]
                  (mapv (fn [signal] {:from id :to signal :value value}) destinations))})
    :low
    {:module module
     :signals (mapv (fn [signal] {:from id :to signal :value false}) destinations)}

    {:module module
     :signals []}))

(defn send-signals [simulation init-signal]
  (iterate (fn [{:keys [q modules] :as simulation}]
             (if-let [{:keys [to] :as signal} (peek q)]
               (let [{:keys [module signals]} (send-signal (to modules) signal)]
                 (-> simulation
                     (update :q pop)
                     (update :q into signals)
                     (assoc-in [:modules to] module)
                     (update :signals conj signal)))
               (assoc simulation :done true)))
           (assoc simulation :q (into u/bfs-queue [init-signal]))))


(defn reset [simulation]
  (assoc simulation :done false :signals []))

(defn push-button [simulation]
  (->> (send-signals (reset simulation) {:from :button :to :broadcaster :value false})
       (drop-while (complement :done))
       (first)))

(defn simulation [modules]
  {:modules modules
   :signals []})

(defn part-1-solver [input]
  (let [simulation (simulation (initialize (parse-input input)))
        sent-signals (->> (iterate push-button simulation)
                          (drop 1)
                          (take 1000)
                          (mapcat :signals))
        counts (-> (group-by :value sent-signals)
                   (update-vals count)
                   (vals))]
    (reduce * counts)))

(defn part-2-solver [input]
  (let [simulation (simulation (initialize (parse-input input)))]
    (->> (iterate push-button simulation)
         (map :signals)
         (map-indexed vector)
         (filter (comp (partial some
                                (every-pred :value
                                            (comp #{:xc :kk :sk :vt} :from)))
                       second))
         (map first)
         (take 4)
         (apply u/lcm))))
