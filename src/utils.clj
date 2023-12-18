(ns utils
  (:require [clojure.java.io :as io]
            [clojure.math :as math])
  (:import (java.security MessageDigest)))

(defn line-seq-input [ns]
  (some->> (str ns)
           (re-matches #".*-(\d+)\.[^\d]*(\d+)")
           (rest)
           (apply format "%s/day-%s.txt")
           (io/resource)
           (io/reader)
           (line-seq)))

(defn xgcd
  "Extended Euclidean Algorithm. Returns [gcd(a,b) x y] where ax + by = gcd(a,b)."
  [a b]
  (if (= a 0)
    [b 0 1]
    (let [[g x y] (xgcd (mod b a) a)]
      [g (- y (* (math/floor-div b a) x)) x])))

(defn lcm
  ([x] x)
  ([x y]
   (/ (* x y)
      (first (xgcd x y))))
  ([x y & more]
   (reduce lcm
           (lcm x y)
           more)))

(defn hex [ba]
  (apply str (map (partial format "%02x") ba)))

(defn md5 [s]
  (.digest (MessageDigest/getInstance "MD5") (.getBytes s)))

(def md5-hex (comp hex md5))

(def bfs-queue clojure.lang.PersistentQueue/EMPTY)

(def dfs-queue [])

(defn search [state exit next]
  (loop [state state]
    (if-let [result (exit state)]
      result
      (when-let [next (next state)]
        (recur next)))))

(defn pairs [xs]
  (let [v (vec xs)
        n (count v)]
    (for [i (range n)
          j (range (inc i) n)]
      [(get v i) (get v j)])))

(defn benchmark [{:keys [year day]}]
  (time
   (doseq [day (if day [day] (range 1 26))]
     (let [ns    (symbol (format "adventofcode-%d.day-%02d" year day))
           _     (require ns)
           p1    (ns-resolve ns 'part-1-solver)
           p2    (ns-resolve ns 'part-2-solver)
           input @(ns-resolve ns (symbol "input"))]

       (print (format "Day %s Part 1: " day))
       (time (p1 input))
       (when p2
         (print (format "Day %s Part 2: " day))
         (time (p2 input))))))
  (System/exit 0))

(defn transpose [coll]
  (apply mapv vector coll))

(defn bbox-2d [points]
  (reduce (fn [[mn mx] point]
            [(mapv min mn point)
             (mapv max mx point)])
          [(first points) (first points)]
          points))

(defn to-xy-point-map [val-fn input]
  (let [n-col (count (first input))
        point (fn [x] [(rem x n-col) (quot x n-col)])]
    (transduce (map-indexed (fn [i x]
                              (when-let [val (val-fn x)]
                                [(point i) val])))
               conj
               {}
               (apply concat input))))

(def a*-open-set (sorted-set-by (fn [{af :f} {bf :f}] (- af bf))))

(defn- a*impl [open-set goal neighbours-fn h d]
  (iterate (fn [{:keys [open-set g-score came-from] :as search-state}]
             (let [current (first open-set)
                   current-node (:node current)]
               (if (and current-node (not= goal current-node))
                 (let [neighbours (neighbours-fn search-state current-node)]
                   (->> neighbours
                        (reduce (fn [acc neighbour]
                                  (let [tentative-score (+ (get g-score current-node Double/POSITIVE_INFINITY)
                                                           (d current-node neighbour))]
                                    (if (< tentative-score (get g-score neighbour Double/POSITIVE_INFINITY))
                                      (-> acc
                                          (update :came-from assoc neighbour current-node)
                                          (update :g-score assoc neighbour tentative-score)
                                          (update :open-set conj {:node neighbour
                                                                  :f (+ tentative-score (h neighbour))}))
                                      acc)))
                                (update search-state :open-set disj current))))
                 {:done? true :came-from came-from :current-node current-node :g-score g-score})))
           {:open-set (into a*-open-set (map (fn [node] {:node node :f (h node)})) open-set)
            :g-score (zipmap open-set (repeat 0))
            :came-from {}}))

(defn a* [start goal neighbours-fn h d]
  (->> (a*impl start goal neighbours-fn h d)
       (drop-while (comp not :done?))
       (first)))

(defn sorted-set-by-key [k]
  (sorted-set-by (fn [{ak k :as a} {bk k :as b}]
                   (cond
                     (= a b) 0
                     (= ak bk) -1
                     :else (- ak bk)))))
