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

(defn benchmark [{:keys [day]}]
  (doseq [day (if day [day] (range 1 26))]
    (let [ns    (symbol (str "adventofcode-2022.day-" (format "%02d" day)))
          _     (require ns)
          p1    (ns-resolve ns 'part-1-solver)
          p2    (ns-resolve ns 'part-2-solver)
          input @(ns-resolve ns (symbol "input"))]

      (print (format "Day %s Part 1: " day))
      (time (p1 input))
      (when p2
        (print (format "Day %s Part 2: " day))
        (time (p2 input)))))
  (System/exit 0))
