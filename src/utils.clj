(ns utils
  (:require [clojure.java.io :as io]))

(defn line-seq-input [ns]
  (some->> (str ns)
           (re-matches #".*-(\d+)\.[^\d]*(\d+)")
           (rest)
           (apply format "%s/day-%s.txt")
           (io/resource)
           (io/reader)
           (line-seq)))

