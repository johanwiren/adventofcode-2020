(ns adventofcode-2022.utils
  (:require [clojure.java.io :as io]))

(defn line-seq-input [ns]
  (some->> (str ns)
           (re-matches #".*-(\d+)\.(.*)")
           (rest)
           (apply format "%s/%s.txt")
           (io/resource)
           (io/reader)
           (line-seq)))

(defn benchmark [{:keys [day n]
                  :or   {n 1000}}]
  (doseq [day (if day [day] (range 1 13))]
    (let [ns    (symbol (str "adventofcode-2022.day-" (format "%02d" day)))
          _     (require ns)
          p1    (ns-resolve ns 'part-1-solver)
          p2    (ns-resolve ns 'part-2-solver)
          input @(ns-resolve ns (symbol "input"))]

      (println (format "== Day %s Part 1 - %s times ==" day n))
      (time (dotimes [_ n]
              (p1 input)))
      (println (format "== Day %s Part 2 - %s times ==" day n))
      (time (dotimes [_ n]
              (p2 input))))))
