(ns adventofcode-2022.day-19
  (:require [adventofcode-2022.utils :as u]
            [clojure.string :as str]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))
#_(def input ["Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian."
"Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."])

(defn parse-blueprint [robot]
  (let [[_ type & cost]
        (re-matches #" Each (\w+) robot costs (\d+) (\w+)(?: and )?(\d+)? ?(\w+)?" robot)]
    [(keyword type) (into {}
                          (comp
                           (filter (comp some? first))
                           (map (fn [[cost resource]]
                                       [(keyword resource) (parse-long cost)])))
                          (partition 2 cost))]))

(defn buy [{:keys [resources recipe] :as state} robot]
  (let [resources (merge-with (fnil - 0) resources (recipe robot))]
    (when (every? nat-int? (vals resources))
      (-> state
          (assoc :resources resources)
          (update-in [:robots robot] (fnil inc 0))))))

(defn ore-equiv [recipe item]
  (if (= :ore item)
    (get-in recipe [item item])
    (->> (get recipe item)
         (map (fn [[item count]]
                (* count (ore-equiv recipe item))))
         (reduce +))))

(defn ore-equivs [recipe]
  (map (partial ore-equiv recipe) [:geode :obsidian :clay :ore]))

(defn mk-score-fn [{:keys [recipe step]}]
  (let [ore-equivs (ore-equivs recipe)]
    (fn [{:keys [robots]}]
      (let [{:keys [geode obsidian clay ore]} robots]
        (reduce + (map * [geode obsidian clay ore] #_ore-equivs))))))

(defn mk-exp-score-fn [{:keys [recipe]}]
  (let [ore-equivs (ore-equivs recipe)]
    (fn [{:keys [robots]}]
      (let [{:keys [geode obsidian clay ore]} robots]
        (reduce + (map * [8 4 2 1] [geode obsidian clay ore] ore-equivs))))))

(defn exp-robot-score [state]
  (let [{:keys [geode obsidian clay ore]} (:robots state)]
    (reduce + (map * [8 4 2 1] [geode obsidian clay ore]))))

(defn score [{:keys [robots resources step]}]
  (let [{:keys [geode obsidian clay ore]} (merge-with * robots resources)]
    (reduce + (map * [1 1 1 1] [geode obsidian clay ore]))))

(defn resource-score [{:keys [resources]}]
  (let [{:keys [geode obsidian clay ore]} resources]
    (reduce + (map * [1000000 10000 100 1] [geode obsidian clay ore]))))

(defn flat-resource-score [{:keys [resources]}]
  (reduce + (vals resources)))

(defn score-key [{:keys [robots resources step] :as state}]
  (let [{:keys [geode obsidian clay ore]} (merge-with (fn [a b]
                                                        (if (every? pos? [a b])
                                                          (* a b)
                                                          (+ a b)))
                                                      robots
                                                      resources)
        robot-str (format "%s-%s-%s-%s" geode obsidian clay ore)
        {:keys [geode obsidian clay ore]} resources
        resources-str (format "%s-%s-%s-%s" geode obsidian clay ore)]
    (keyword (format "%s-%s" robot-str resources-str #_(resource-score state)))))

(defn simple-dedup [{:keys [robots resources]}]
  (let [{:keys [geode obsidian clay ore]} robots 
        robot-str (format "%s-%s-%s-%s" geode obsidian clay ore)
        {:keys [geode obsidian clay ore]} resources
        resources-str (format "%s-%s-%s-%s" geode obsidian clay ore)]
    (keyword (format "%s-%s" robot-str resources-str))))

(defn needed? [{:keys [recipe robots resources step]} resource]
  (let [wanted-rate (->> (vals recipe)
                         (keep resource)
                         (apply max))]
    (< (robots resource) (- wanted-rate
                            (/ (resources resource)
                               step)))))

(defn needed-robots [state]
  (->> [:obsidian :clay :ore]
       (filter (partial needed? state))
       (cons :geode)))

(defn smart-needed-robots
  ([state]
   (smart-needed-robots state :geode))
  ([{:keys [recipe] :as state} needed-resource]
   (distinct (cons needed-resource (->> (recipe needed-resource)
                                        (keys)
                                        (remove (partial = needed-resource))
                                        (filter (partial needed? state))
                                        (mapcat (partial smart-needed-robots state)))))))

(comment
  (smart-needed-robots (-> rcp1
                           (assoc :step 1)
                           (assoc-in [:resources :ore] 1)
                           (assoc-in [:resources :clay] 1000)
                           (assoc-in [:robots :obsidian] 1)))


  #__)

(defn smart-buy [state]
  (->> (smart-needed-robots state)
       (keep (partial buy state))
       (cons state)))

(defn max-geodes [max-steps state]
  (let [init (assoc state :step max-steps)
        score-fn (mk-exp-score-fn init)]
    (loop [q (conj clojure.lang.PersistentQueue/EMPTY init)
           max-geodes 0
           seen #{}]
      (let [state (peek q)
            step (:step state)]
        (if (nil? state)
          max-geodes
          (if (pos? step)
            (let [new-resources (:robots state)
                  new-states (->> state
                                  smart-buy
                                  (map #(update % :resources (partial merge-with (fnil + 0)) new-resources))
                                  (map #(update % :step dec))
                                  (remove (comp seen simple-dedup))
                                  #_(remove (comp max-scores score-key)))
                  seen (into seen (map simple-dedup) new-states)]
              #_(when (zero? (rand-int 100000))
                  (println seen)
                  #_(println "SCORE KEYS" (map score-key new-states))
                  #_(run! (comp println :robots) new-states))
              (recur (into (pop q) new-states) max-geodes seen))
            (recur (pop q) (max max-geodes (get-in state [:resources :geode])) seen)))))))

(defn parse-line [line]
  (->> (str/split line #"[.:]")
       (rest)
       (map parse-blueprint)
       (into {})))

(defn parse-input [input]
  (->> input
       (map parse-line)
       (map-indexed (fn [i v] (assoc v :id (inc i))))
       (map (partial assoc
                     {:robots (into {:ore 1} (map vector [:clay :obsidian :geode] (repeat 0)))
                      :resources (into {} (map vector [:ore :clay :obsidian :geode] (repeat 0)))}
                     :recipe))))

(defn part-1-solver [input]
  (->> (parse-input input)
       (pmap (juxt (comp :id :recipe)
                   (partial max-geodes 24)))
       (map (partial apply *))
       (reduce +)))

(defn part-2-solver [input]
  (->> (parse-input input)
       (take 3)
       (pmap (juxt (comp :id :recipe)
                   (partial max-geodes 32)))
       (map (partial apply *))
       (reduce +)))

(t/deftest part-1-test
  (t/is (= 2193 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= :FIXME (time (part-2-solver input)))))
