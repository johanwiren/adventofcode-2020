(ns adventofcode-2015.day21
  (:require [utils :as u]
            [clojure.string :as str]))

(def input (u/line-seq-input *ns*))

(def store
  ["Weapons:    Cost  Damage  Armor"
   "Dagger        8     4       0"
   "Shortsword   10     5       0"
   "Warhammer    25     6       0"
   "Longsword    40     7       0"
   "Greataxe     74     8       0"
   ""
   "Armor:      Cost  Damage  Armor"
   "Leather      13     0       1"
   "Chainmail    31     0       2"
   "Splintmail   53     0       3"
   "Bandedmail   75     0       4"
   "Platemail   102     0       5"
   ""
   "Rings:      Cost  Damage  Armor"
   "Damage +1    25     1       0"
   "Damage +2    50     2       0"
   "Damage +3   100     3       0"
   "Defense +1   20     0       1"
   "Defense +2   40     0       2"
   "Defense +3   80     0       3"])

(defn parse-item [item]
  (let [[name & vals] (str/split item #"  +")]
    (into [name] (map parse-long vals))))

(defn parse-items [items]
  (let [[type & keys] (str/split (first items) #"[: ]+")
        type (-> type (str/lower-case) keyword)
        keys (into [:type :name] (map (comp keyword str/lower-case) keys))]
    (->> (rest items)
         (map parse-item)
         (map (fn [item]
                (zipmap keys (into [type] item)))))))

(defn parse-store [store]
  (->> store
       (partition-by #{""})
       (remove #{[""]})
       (mapcat parse-items)))

(defn parse-line [line]
  (let [[k v] (str/split line #": ")]
    [(-> k
         (str/lower-case)
         (str/replace #" " "-")
         keyword)
     (parse-long v)]))

(defn parse-input [input]
  (into {} (map parse-line input)))

(defn init-game [input]
  (let [boss (parse-input input)
        store (parse-store store)]
    {:boss boss
     :store store
     :player {:hit-points 100 :damage 0 :armor 0}
     :turn :player
     :cost 0}))

(def defender {:player :boss
               :boss :player})

(defn turn [{:keys [turn] :as game}]
  (let [defender (defender turn)
        armor (get-in game [defender :armor])
        damage (max 1 (- (get-in game [turn :damage]) armor))]
    (-> game
        (update-in [defender :hit-points] - damage)
        (assoc :turn defender))))

(defn loser [game]
  (->> (select-keys game [:boss :player])
       (filter (fn [[_ stats]]
                 (<= (:hit-points stats) 0)))
       ffirst))

(defn play [game]
  (->> game
       (iterate turn)
       (drop-while (complement loser))
       (first)))

(defn abilities [item]
  (select-keys item [:damage :armor]))

(defn costs [game]
  (let [{:keys [store]} game
        rings (conj (filter (comp #{:rings} :type) store) {:cost 0})]
    (for [weapon (filter (comp #{:weapons} :type) store)
          armor (conj (filter (comp #{:armor} :type) store) {:cost 0})
          left rings
          right (remove (fn [ring]
                          (and (:name left)
                               (= (:name left) (:name ring))))
                        rings)
          :let [items [weapon armor left right]
                game (->> items
                          (reduce (fn [game {:keys [cost] :as item}]
                                    (-> game
                                        (update :player (partial merge-with +) (abilities item))
                                        (update :cost + cost)))
                                  game)
                          (play))]]
      {:winner (defender (:turn game))
       :cost (:cost game)})))

(defn part-1-solver [input]
  (->> (init-game input)
       (costs)
       (filter (comp #{:player} :winner))
       (map :cost)
       (sort)
       (first)))

(defn part-2-solver [input]
  (->> (init-game input)
       (costs)
       (filter (comp #{:boss} :winner))
       (map :cost)
       (sort)
       (last)))
