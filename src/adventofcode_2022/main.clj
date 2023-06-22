(ns adventofcode-2022.main
  (:require [adventofcode-2022.utils :as utils]
            [clojure.edn :as edn])
  (:gen-class))

(defn -main [& args]
  (let [args-map (->> args
                      (map edn/read-string)
                      (apply hash-map))]
    (utils/benchmark args-map)))
