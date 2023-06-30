(ns adventofcode-2015.day22
  (:require [utils :as u]
            [adventofcode-2015.day21 :as day21]))

(def input (u/line-seq-input *ns*))

(defn spell-effects [game]
  (->> (filter (comp pos? :turns) (get-in game [:player :spells]))
       (reduce (fn [game {:keys [boss player name]}]
                 (cond-> (update game :boss (partial merge-with +) boss)
                   (= :shield name) (update :player merge player)
                   (not= :shield name) (update :player (partial merge-with +) player)))
               game)))

(defn boss-turn [game]
  (let [armor (get-in game [:player :armor])
        damage (max 1 (- (get-in game [:boss :damage]) armor))]
    (update-in game [:player :hit-points] - damage)))

(defn spell-timeout [game]
  (let [decreased (update-in game [:player :spells] (partial map (fn [spell] (update spell :turns dec))))]
    (cond-> (update-in decreased [:player :spells] (partial remove (comp zero? :turns)))
      (seq (filter (every-pred (comp #{:shield} :name)
                               (comp zero? :turns))
                   (get-in decreased [:player :spells])))
      (assoc-in [:player :armor] 0))))

(defn spell-turn [game]
  (-> game
      spell-effects
      spell-timeout))

(defn init-game [input]
  {:boss (day21/parse-input input)
   :player {:hit-points 50 :armor 0 :mana 500 :mana-spent 0 :spells []}
   :spells [{:name :magic-missile
             :cost 53
             :boss {:hit-points -4}
             :turns 1}
            {:name :drain
             :cost 73
             :boss {:hit-points -2}
             :player {:hit-points 2}
             :turns 1}
            {:name :shield
             :cost 113
             :player {:armor 7}
             :turns 6}
            {:name :poison
             :cost 173
             :boss {:hit-points -3}
             :turns 6}
            {:name :recharge
             :cost 229
             :player {:mana 101}
             :turns 5}]})

(defn losers [game]
  (->> (select-keys game [:boss :player])
       (filter (fn [[_ stats]]
                 (<= (:hit-points stats) 0)))
       (map first)
       (set)))

(defn hard-mode [game]
  (update-in game [:player :hit-points] dec))

(defn do-turns [game turns]
  (reduce (fn [game step]
            (if (seq (losers game))
              (reduced game)
              (step game)))
          game
          turns))

(defn least-mana-win
  ([game]
   (least-mana-win game identity))
  ([game hard-mode]
   (loop [q [game]
          least-mana-win Integer/MAX_VALUE]
     (let [game (some-> (peek q)
                        (do-turns [hard-mode spell-turn]))]
       (if (nil? game)
         least-mana-win
         (let [mana (get-in game [:player :mana])
               active-spells (into #{}
                                   (comp (filter (comp pos? :turns))
                                         (map :name))
                                   (get-in game [:player :spells]))
               cast-spells (into []
                                 (comp (filter (fn [{:keys [cost]}]
                                                 (<= cost mana)))
                                       (remove (comp active-spells :name)))
                                 (:spells game))
               new-games (->> cast-spells
                              (map (fn [spell]
                                     (-> game
                                         (update-in [:player :spells] conj spell)
                                         (update-in [:player :mana] - (:cost spell))
                                         (update-in [:player :mana-spent] + (:cost spell))
                                         (update :history conj (select-keys (:player game) [:armor :hit-points]))
                                         (do-turns [spell-turn boss-turn]))))
                              (remove (comp (partial <= least-mana-win) :mana-spent :player)))
               losers (losers game)
               win? (= #{:boss} losers)]
           (recur (if (seq losers)
                    (pop q)
                    (into (pop q) new-games))
                  (if win?
                    (min least-mana-win (get-in game [:player :mana-spent]))
                    least-mana-win))))))))

(defn part-1-solver [input]
  (-> (init-game input)
      (least-mana-win)))

(defn part-2-solver [input]
  (-> (init-game input)
      (least-mana-win hard-mode)))

