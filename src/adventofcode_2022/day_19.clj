(ns adventofcode-2022.day-19
  (:require [adventofcode-2022.utils :as u]
            [clojure.string :as str]
            [clojure.test :as t]
            [clojure.math :as math]))

(def input (u/line-seq-input *ns*))

(defn parse-blueprint [robot]
  (let [[_ type & cost]
        (re-matches #" Each (\w+) robot costs (\d+) (\w+)(?: and )?(\d+)? ?(\w+)?" robot)]
    [(keyword type) (into {}
                          (comp
                           (filter (comp some? first))
                           (map (fn [[cost resource]]
                                       [(keyword resource) (parse-long cost)])))
                          (partition 2 cost))]))

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

(defn buy [{:keys [resources recipe] :as state} robot]
  (let [resources (merge-with - resources (recipe robot))]
    (when (every? nat-int? (vals resources))
      (-> state
          (assoc :resources resources)
          (update-in [:robots robot] inc)))))

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

(defn smart-needed-robots
  ([state]
   (smart-needed-robots state :geode))
  ([{:keys [recipe] :as state} needed-resource]
   (distinct (cons needed-resource
                   (->> (recipe needed-resource)
                        (keys)
                        (remove (partial = needed-resource))
                        (filter (partial needed? state))
                        (mapcat (partial smart-needed-robots state)))))))

(defn mine [{:keys [robots] :as state}]
  (update state :resources (partial merge-with +) robots))

(defn mine-to-buy-robot [{:keys [robots resources recipe step] :as state} robot]
  (let [rcp (recipe robot)
        to-mine (->> rcp
                     (merge-with - (select-keys resources (keys rcp)))
                     (filter (comp neg? second))
                     (map (fn [[k v]]
                            [k (- v)]))
                     (into {}))]
    (if (empty? to-mine)
      (-> state
          (buy robot)
          (update :resources (partial merge-with +) robots)
          (update :step dec))
      (when (every? pos? (vals (select-keys robots (keys to-mine))))
        (let [steps (->> (merge-with (comp int math/ceil /) to-mine (select-keys robots (keys to-mine)))
                         (vals)
                         (apply max))]
          (when (<= (inc steps) step)
            (let [mined-resources (update-vals robots (partial * steps))]
              (-> state
                  (update :resources (partial merge-with +) mined-resources)
                  (update :step - steps)
                  (buy robot)
                  (update :resources (partial merge-with +) robots)
                  (update :step dec)))))))))

(defn mine-to-buy [state]
  (let [new-states (->> (smart-needed-robots state)
                        (keep (partial mine-to-buy-robot state)))]
    (if (seq new-states)
      new-states
      [(update (mine state) :step dec)])))

(defn max-geodes [max-steps state]
  (let [init (assoc state :step max-steps)]
    (loop [q (conj [] init)
           max-geodes 0
           seen #{}]
      (let [state (peek q)
            step (:step state)]
        (if (nil? state)
          max-geodes
          (if (pos? step)
            (let [new-states (->> (mine-to-buy state)
                                  (remove (comp seen simple-dedup)))
                  seen (into seen (map simple-dedup) new-states)]
              (recur (into (pop q) new-states) max-geodes seen))
            (recur (pop q) (max max-geodes (get-in state [:resources :geode])) seen)))))))

(defn part-1-solver [input]
  (->> (parse-input input)
       (pmap (juxt (comp :id :recipe)
                   (partial max-geodes 24)))
       (map (partial apply *))
       (reduce +)))

(defn part-2-solver [input]
  (->> (parse-input input)
       (take 3)
       (pmap (partial max-geodes 32))
       (reduce *)))

(t/deftest part-1-test
  (t/is (= 2193 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 7200 (time (part-2-solver input)))))
