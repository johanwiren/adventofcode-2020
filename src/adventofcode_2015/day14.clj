(ns adventofcode-2015.day14)

(def input ["Vixen can fly 8 km/s for 8 seconds, but then must rest for 53 seconds."
            "Blitzen can fly 13 km/s for 4 seconds, but then must rest for 49 seconds."
            "Rudolph can fly 20 km/s for 7 seconds, but then must rest for 132 seconds."
            "Cupid can fly 12 km/s for 4 seconds, but then must rest for 43 seconds."
            "Donner can fly 9 km/s for 5 seconds, but then must rest for 38 seconds."
            "Dasher can fly 10 km/s for 4 seconds, but then must rest for 37 seconds."
            "Comet can fly 3 km/s for 37 seconds, but then must rest for 76 seconds."
            "Prancer can fly 9 km/s for 12 seconds, but then must rest for 97 seconds."
            "Dancer can fly 37 km/s for 1 seconds, but then must rest for 36 seconds."])

(defn parse-line [line]
  (let [[name speed time rest] (rest (re-matches #"(.*) can fly (.*) km/s for (.*) seconds, but then must rest for (.*) seconds." line))]
    {:name name
     :speed (Integer/parseInt speed)
     :time (Integer/parseInt time)
     :rest (Integer/parseInt rest)}))

(defn parse-input [input]
  (map parse-line input))

(defn ready [{:keys [time speed rest] :as deer}]
  (assoc deer :steps (flatten (repeat (concat (repeat time speed) (repeat rest 0))))))

(defn part-1-solver [input]
  (->> (parse-input input)
       (map ready)
       (map #(update % :steps (comp (partial reduce +)
                                    (partial take 2503))))
       (sort-by :steps)
       (first)
       :steps))

(defn move [{:keys [steps loc] :as deer}]
  (-> deer
      (update :steps (partial drop 1))
      (assoc :loc (+ (or loc 0) (first steps)))))

(defn score [{:keys [deers] :as game}]
  (let [leaders (->> deers
                     (group-by :loc)
                     (sort-by first)
                     (last)
                     (second)
                     (map :name))
        new-scores (zipmap leaders (repeat 1))]
    (update game :scores #(merge-with + % new-scores))))

(defn tick [game]
  (-> game
      (update :deers (partial map move))
      (score)))

(defn part-2-solver [input]
  (->> (iterate tick {:deers (->> (parse-input input)
                                  (map ready))
                      :scores {}})
       (drop 2503)
       (first)
       (:scores)
       (sort-by second)
       (last)))


(comment

  (part-1-solver input)


  )
