(ns adventofcode-2016.day17
  (:require [utils :as u]))

(def input "udskfozm")

(defn open? [c]
  (#{\b \c \d \e \f} c))

(defn open-doors [passcode [x y]]
  (let [[u d l r] (map open? passcode)]
    (cond-> []
      u (conj [x (dec y)])
      d (conj [x (inc y)])
      l (conj [(dec x) y])
      r (conj [(inc x) y]))))

(defn in-bbox? [[[min-x min-y] [max-x max-y]] [x y]]
  (and (<= min-x x max-x)
       (<= min-y y max-y)))

(defn direction [from to]
  ({[ 1  0] "R"
    [-1  0] "L"
    [ 0  1] "D"
    [ 0 -1] "U"}
   (mapv - to from)))

(defn find-path [neigh-fn start goal]
  (loop [q (into clojure.lang.PersistentQueue/EMPTY [{:pos start
                                                      :path ""}])]
    (let [{:keys [pos path]} (peek q)]
      (if (or (nil? pos)
              (= pos goal))
        path
        (let [neighbours (neigh-fn pos path)]
          (recur (into (pop q)
                       (map (fn [pos']
                              {:pos pos'
                               :path (str path (direction pos pos'))}))
                       neighbours)))))))

(defn mk-neigh-fn [password bbox]
  (fn [pos path]
    (->> (open-doors (u/md5-hex (str password path)) pos)
         (filter (partial in-bbox? bbox)))))

(defn find-longest-path [neigh-fn start goal]
  (loop [q (into clojure.lang.PersistentQueue/EMPTY [{:pos start
                                                      :path ""}])
         longest-path ""]
    (let [{:keys [pos path]} (peek q)]
      (if (nil? pos)
        longest-path
        (let [neighbours (when (not= goal pos) (neigh-fn pos path))]
          (recur (into (pop q)
                       (map (fn [pos']
                              {:pos pos'
                               :path (str path (direction pos pos'))}))
                       neighbours)
                 (if (and (= goal pos)
                          (< (count longest-path)
                             (count path)))
                   path
                   longest-path)))))))

(defn part-1-solver [input]
  (find-path (mk-neigh-fn input [[0 0] [3 3]]) [0 0] [3 3]))

(defn part-2-solver [input]
  (time (count (find-longest-path (mk-neigh-fn input [[0 0] [3 3]]) [0 0] [3 3]))))
