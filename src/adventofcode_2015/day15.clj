(ns adventofcode-2015.day15
  (:require [clojure.string :as str]))

(def input ["Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5"
            "Candy: capacity 0, durability 5, flavor -1, texture 0, calories 8"
            "Butterscotch: capacity -1, durability 0, flavor 5, texture 0, calories 6"
            "Sugar: capacity 0, durability 0, flavor -2, texture 2, calories 1"])

(defn parse-line [line]
  (let [[name & kvs] (-> line
                         (str/replace #"[:,]" "")
                         (str/split #" "))]
    (->> (partition 2 kvs)
         (map (fn [[k v]] [(keyword k) (Integer/parseInt v)]))
         (into {:name name}))))

(defn parse-input [input]
  (map parse-line input))

(defn add-ingr [cookie ingr]
  (merge-with + cookie (-> ingr
                           (assoc :ingredients 1)
                           (dissoc :name))))

(defn no-cal-score [cookie]
  (let [vs (->> (dissoc cookie :calories)
                (vals)
                (map (partial max 0)))
        non-zeros (->> vs
                       (remove zero?)
                       (count))
        score (reduce * vs)]
    [non-zeros score]))

(defn add-best-ingr [ingredients cookie]
  (->> ingredients
       (map (partial add-ingr cookie))
       (sort-by no-cal-score)
       (last)))

(defn cal-score [{:keys [ingredients calories] :as cookie}]
  (if (< (- 100 ingredients) (- 500 calories))
    (no-cal-score cookie)
    [Integer/MIN_VALUE]))

(defn add-best-ingr-2 [add-ingredients cookie]
  (->> add-ingredients
       (map (partial add-ingr cookie))
       (sort-by cal-score)
       (last)))

(defn part-1-solver [input]
  (let [ingredients (parse-input input)
        cookie (->> {}
                    (iterate (partial add-best-ingr ingredients))
                    (drop 100)
                    first)]
    cookie
    #_(-> cookie
        (dissoc :calories :ingredients)
        (vals)
        (->> (apply *)))))

(defn perms
  ([max levels]
   (perms max max (dec levels) []))
  ([max remaining level result]
   (if (zero? level)
     [(conj result remaining)]
     (mapcat #(perms max (- remaining %) (dec level) (conj result %))
             (range (inc remaining))))))

(defn part-2-solver [input]
  (let [ingredients (parse-input input)
        by-name (into {} (map (juxt :name identity)) ingredients)
        fc (get-in by-name ["Frosting" :calories])
        cc (get-in by-name ["Candy" :calories])
        bc (get-in by-name ["Butterscotch" :calories])
        sc (get-in by-name ["Sugar" :calories])
        fvs (vals (dissoc (get by-name "Frosting") :calories :name))
        cvs (vals (dissoc (get by-name "Candy") :calories :name))
        bvs (vals (dissoc (get by-name "Butterscotch") :calories :name))
        svs (vals (dissoc (get by-name "Sugar") :calories :name))
        scores (for [f (range 0 (inc 100))
                     c (range 0 (inc (- 100 f)))
                     b (range 0 (inc (- 100 f c)))
                     s (range (- 100 f c b) (inc (- 100 f c b)))
                     :when (= 500 (+ (* fc f) (* cc c) (* bc b) (* sc s)))]
                 (->> (map +
                           (map (partial * f) fvs)
                           (map (partial * c) cvs)
                           (map (partial * b) bvs)
                           (map (partial * s) svs))
                      (map (partial max 0))
                      (apply *)))]
    (->> scores
         sort
         last)))

(comment

  (recipes 4 2)

  (parse-input input)

  (part-1-solver input)


  (time (part-2-solver input))

  o
;;
  )
