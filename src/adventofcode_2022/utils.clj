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
