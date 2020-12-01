(ns adventofcode-2020.util
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defmacro puzzle []
  `(-> *ns*
       str
       (clojure.string/split #"\.")
       last))

(defn ->line-seq [filename]
  (some-> filename
          io/resource
          io/reader
          line-seq))

(defmacro slurp-input []
  `(->line-seq (puzzle)))

(defmacro slurp-reference-input []
  `(->line-seq (str (puzzle) "-reference")))
