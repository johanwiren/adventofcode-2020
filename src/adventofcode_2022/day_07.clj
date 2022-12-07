(ns adventofcode-2022.day-07
  (:require [adventofcode-2022.utils :as u]
            [clojure.string :as str]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn make-tree-map [input]
  (let [m (->> input
               (map #(str/split % #" "))
               (reduce (fn [{:keys [cwd] :as tree-map} args]
                         (cond
                           (= ["$" "cd"] (butlast args))
                           (let [path-arg (last args)
                                 path     (if (= ".." path-arg)
                                            (vec (butlast cwd))
                                            (conj cwd path-arg))]
                             (assoc tree-map :cwd path))

                           (= ["$" "ls"] args)
                           tree-map

                           (= "dir" (first args))
                           (update-in tree-map
                                      (vector cwd :dirs)
                                      conj
                                      (conj cwd (last args)))
                           :else ;; file-entry
                           (update-in tree-map
                                      (vector cwd :size)
                                      (fnil + 0)
                                      (parse-long (first args)))))
                       {:cwd []}))]
    (dissoc m :cwd)))

(defn resolve-sizes [tree-map]
  (->> (keys tree-map)
       (sort-by (comp - count))
       (reduce (fn [acc path]
                 (let [{:keys [dirs]} (get tree-map path)]
                   (update-in acc
                              (vector path :size)
                              (fnil + 0)
                              (->> dirs
                                   (keep (comp :size acc))
                                   (reduce +)))))
               tree-map)))

(defn part-1-solver [input]
  (->> input
       (make-tree-map)
       (resolve-sizes)
       (vals)
       (keep :size)
       (filter (partial > 100000))
       (reduce +)))

(defn part-2-solver [input]
  (let [tree-map   (->> input
                        (make-tree-map)
                        (resolve-sizes))
        total-used (get-in tree-map (vector ["/"] :size))
        free       (- 70000000 total-used)
        needed     (- 30000000 free)]
    (->> (vals tree-map)
         (keep :size)
         (filter (partial < needed))
         (sort)
         (first))))

(t/deftest part-1-test
  (t/is (= 1778099 (time (part-1-solver input)))))

(t/deftest part-2-test
  (t/is (= 1623571 (time (part-2-solver input)))))
