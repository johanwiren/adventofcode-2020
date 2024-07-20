(ns adventofcode-2022.day-07
  (:require [utils :as u]
            [clojure.string :as str]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn make-size-map [input]
  (->> input
       (map #(str/split % #" "))
       (reduce (fn [[cwd size-map :as acc] args]
                 (cond
                   (= ["$" "cd"] (pop args))
                   (let [path-arg (last args)
                         path     (if (= ".." path-arg)
                                    (pop cwd)
                                    (conj cwd path-arg))]
                     [path size-map])

                   (or (= ["$" "ls"] args)
                       (= "dir" (first args)))
                   acc

                   :else ;; file-entry
                   (let [size (parse-long (first args))]
                     [cwd (->> (iterate butlast cwd)
                               (take-while some?)
                               (reduce (fn [size-map path]
                                         (update size-map path (fnil + 0) size))
                                       size-map))])))
               [[] {}])
       (second)))

(defn part-1-solver [input]
  (->> input
       (make-size-map)
       (vals)
       (filter (partial > 100000))
       (reduce +)))

(defn part-2-solver [input]
  (let [size-map   (make-size-map input)
        total-used (get size-map ["/"])
        free       (- 70000000 total-used)
        needed     (- 30000000 free)]
    (->> (vals size-map)
         (filter (partial < needed))
         (sort)
         (first))))

(t/deftest part-1-test
  (t/is (= 1778099 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 1623571 (time (part-2-solver input)))))
