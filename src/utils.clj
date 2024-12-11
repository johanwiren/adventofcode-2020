(ns utils
  (:require [clojure.java.io :as io]
            [clojure.math :as math]
            [clojure.string :as str])
  (:import (java.security MessageDigest)))


(def aoc-session (System/getenv "AOC_SESSION"))

(def aoc-cache-dir (str (System/getenv "HOME") "/.cache/aoc"))

(defn- get-input
  "No libs .."
  [year day]
  (assert aoc-session "Please set AOC_SESSION")
  (let [con (-> (format "https://adventofcode.com/%s/day/%s/input" year day)
                (io/as-url)
                (.openConnection))]
    (doto con
      (.setRequestMethod "GET")
      (.setRequestProperty "Cookie" (str "session=" aoc-session)))
    (when (= 200 (.getResponseCode con))
      (let [is (.getInputStream con)
            body (slurp is)]
        (.close is)
        body))))

(defn- wrap-file-cache [f]
  (fn [& args]
    (let [file (io/file (apply str aoc-cache-dir "/" args))]
      (if (.exists file)
        (slurp file)
        (when-let [res (apply f args)]
          (io/make-parents file)
          (spit file res)
          res)))))

(def cached-get-input (wrap-file-cache get-input))

(defn str-input [ns]
  (let [[year day] (some->> (str ns)
                            (re-matches #".*-(\d+)\.[^\d]*(\d+)")
                            (rest)
                            (map parse-long))]
    (cached-get-input year day)))

(defn line-seq-input [ns]
  (str/split-lines (str-input ns)))

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
     (try
       (let [ns    (symbol (format "adventofcode-%d.day-%02d" year day))
             _     (require ns)
             p1    (ns-resolve ns 'part-1-solver)
             p2    (ns-resolve ns 'part-2-solver)
             input @(ns-resolve ns (symbol "input"))]

         (print (format "Day %s Part 1: " day))
         (time (p1 input))
         (when p2
           (print (format "Day %s Part 2: " day))
           (time (p2 input))))
       (catch Exception _
         nil))))
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

(defn sorted-set-by-key [k]
  (sorted-set-by (fn [{ak k :as a} {bk k :as b}]
                   (cond
                     (= a b) 0
                     (= ak bk) -1
                     :else (- ak bk)))))

(def a*-open-set (sorted-set-by-key :f))

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

(defn dijkstra
  "Implementation of dijkstra using a sorted-set as priority queue"
  [{:keys [start neighbours-fn cost-fn result-fn return-states?]
    :or {return-states? false}}]
  (let [search-state {:q (into (sorted-set-by-key :cost) [{:cost 0 :node start}])
                      :g-score {start 0}
                      :came-from {}}
        next-state (fn [{:keys [q g-score] :as state}]
                     (let [q-item (first q)
                           {:keys [node]} q-item
                           neighbours (neighbours-fn node)]
                       (reduce (fn [state neighbour]
                                 (let [score (+ (cost-fn node neighbour)
                                                (g-score node Double/POSITIVE_INFINITY))]
                                   (if (< score (g-score neighbour Double/POSITIVE_INFINITY))
                                     (-> state
                                         (update :q conj {:cost score :node neighbour})
                                         (update :came-from assoc neighbour node)
                                         (update :g-score assoc neighbour score))
                                     state)))
                               (update state :q disj q-item)
                               neighbours)))
        take-or-drop-while (if return-states? take-while (comp result-fn first drop-while))]
    (->> (iterate next-state search-state)
         (take-while (comp first :q))
         (take-or-drop-while (comp nil? result-fn)))))

(defn lagrange-interpolation [xs f xi]
  (let [xs (into [] (map biginteger) xs)
        n (count xs)
        is (range n)
        js (range n)]
    (reduce (fn [res i]
              (+ res
                 (reduce (fn [res' j]
                           (if (not= i j)
                             (/ (* res' (- xi (xs j)))
                                (- (xs i)
                                   (xs j)))
                             res'))
                         (f (xs i))
                         js)))
            0
            is)))

(defn range-disj [[a b] [x y]]
  (cond
    (<= x a b y) []
    (<= a b x y) [[a b]]
    (<= x y a b) [[a b]]
    (<= x a y b) [[y b]]
    (<= a x b y) [[a x]]
    :else [[a x] [y b]]))

(def primes
  "Naive lazy-seq of primes"
  (->> (iterate (fn [primes]
                  (conj primes
                        (loop [candidate (+ (first primes) 2)]
                          (if (and (.isProbablePrime (biginteger candidate) 1)
                                   (not-any? #(zero? (rem candidate %)) primes))
                            candidate
                            (recur (+ candidate 2))))))
                '(3 2))
       (cons '(2))
       (map first)))

(defn factor [x]
  (let [sqrt (math/sqrt x)
        primes (take-while #(<= % sqrt) primes)
        divisor (loop [primes primes]
                  (when (seq primes)
                    (if (zero? (rem x (first primes)))
                      (first primes)
                      (recur (rest primes)))))]
    (if divisor
      (cons divisor (factor (/ x divisor)))
      (list x))))
