(ns adventofcode-2015.day19
  (:require [utils :as u]
            [clojure.string :as str]
            [clojure.test :as t]))

(def input (u/line-seq-input *ns*))

(defn parse-input [input]
  (let [[xlate _ [molecule]] (partition-by #{""} input)]
    {:molecule molecule
     :replacements (->> xlate
                        (map #(str/split % #" => ")))}))


(defn replace-molecule [molecule replacements]
  (for [i (range (count molecule))
        [find replace] replacements
        :let [left (subs molecule 0 i)
              right (subs molecule i (count molecule))]
        :when (str/starts-with? right find)]
    (str left (str/replace-first right (re-pattern find) replace))))

(defn part-1-solver [input]
  (-> (let [{:keys [molecule replacements]} (parse-input input)]
        (replace-molecule molecule replacements))
      set
      count))

(defn part-2-solver [input]
  (let [{:keys [molecule replacements]} (parse-input input)
        rev-replacements (->> replacements
                              (map reverse)
                              (sort-by (comp count first) >))]
    (loop [q [[molecule 0]]]
      (let [[next-molecule steps] (peek q)]
        (if (= next-molecule "e")
          steps
          (let [steps (inc steps)
                new-molecules (replace-molecule next-molecule rev-replacements)]
            (recur (into (pop q)
                         (map (fn [m] [m steps]))
                         new-molecules))))))))
