(ns adventofcode-2023.day-05
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-mapping [mapping]
  (let [[_ & mappings] mapping]
    (map (fn [mapping-line]
           (map parse-long (re-seq #"\d+" mapping-line)))
         mappings)))

(defn parse-input [input]
  (let [seeds (map parse-long (re-seq #"\d+" (first input)))]
    {:seeds seeds
     :mappings (->> input
                    (drop 2)
                    (partition-by #{""})
                    (remove #{[""]})
                    (map parse-mapping))}))

(defn mk-mapping [mappings]
  (let [fs (map (fn [[dst src range]]
                  (let [y (- src dst)]
                    (fn [x]
                      (when (<= src x (+ src (dec range)))
                        (- x y)))))
                mappings)]
    (fn [x]
      (or (some (fn [f] (f x)) fs) x))))

(defn part-1-solver [input]
  (let [{:keys [seeds mappings]} (parse-input input)
        composed-mapping (->> mappings
                              (map mk-mapping)
                              (reverse)
                              (apply comp))]
    (->> seeds
         (map composed-mapping)
         (reduce min))))

(defn range-intersection [[a b] [x y]]
  (when (or (<= a x b) (< a y b) (< x a y) (< x b y) (<= a x b y))
    [(max a x) (min b y)]))

(defn range-disj [[a b] [x y]]
  (cond
    (<= x a b y) []
    (<= a b x y) [[a b]]
    (<= x y a b) [[a b]]
    (<= x a y b) [[y b]]
    (<= a x b y) [[a x]]
    :else [[a x] [y b]]))

(defn i-range [[start end]]
  [start (- end start)])

(defn to-range [[start len]]
  [start (+ start len)])

(defn map-range [mapping i-range*]
  (let [range (to-range i-range*)
        [map-dst map-src map-len] mapping
        map-src-range (to-range [map-src map-len])
        map-diff (- map-dst map-src)
        mapped (some->> (range-intersection range map-src-range)
                        (mapv + [map-diff map-diff]))
        unmapped (range-disj range map-src-range)]
    [(or (some-> mapped (i-range) (vector)) []) (map i-range unmapped)]))

(defn plant-seed [mappings seed]
  (reduce (fn [acc mapping]
            (->> mapping
                 (reduce (fn [{:keys [unmapped] :as acc} map-range']
                           (if (empty? unmapped)
                             (reduced acc)
                             (let [map-res (map (partial map-range map-range') unmapped)]
                               (-> acc
                                   (update :mapped into (mapcat first map-res))
                                   (assoc :unmapped (mapcat second map-res))))))
                         {:unmapped acc
                          :mapped #{}})
                 (vals)
                 (apply into)))
          #{seed}
          mappings))

(defn part-2-solver [input]
  (let [{:keys [seeds mappings]} (parse-input input)
        seed-ranges (partition 2 seeds)]
    (->> seed-ranges
         (mapcat (partial plant-seed mappings))
         (map first)
         (reduce min))))
