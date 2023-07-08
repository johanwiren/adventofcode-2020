(ns adventofcode-2016.day04
  (:require [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse-line [line]
  (-> (zipmap [:name :sector-id :checksum] (rest (re-matches #"(.*)-([^-]+)\[(.*)\]" line)))
      (update :sector-id parse-long)))

(defn parse-input [input]
  (map parse-line input))

(defn neg [n]
  (* -1 n))

(defn make-checksum [name]
  (->> (remove #{\-} name)
       (frequencies)
       (sort-by (juxt (comp neg second) first))
       (take 5)
       (map first)
       (apply str)))

(defn part-1-solver [input]
  (->> (parse-input input)
       (filter (fn [{:keys [name checksum]}]
                 (= checksum (make-checksum name))))
       (map :sector-id)
       (reduce +)))

(defn decrypt [{:keys [name sector-id] :as room}]
  (let [decrypted
        (->> name
             (map (fn [ch]
                    (if (= \- ch)
                      \space
                      (char (+ (mod (+ (- (int ch) 97)
                                       sector-id)
                                    26)
                               97)))))
             (apply str))]
    (assoc room :name decrypted)))

(defn part-2-solver [input]
  (->> (parse-input input)
       (filter (fn [{:keys [name checksum]}]
                 (= checksum (make-checksum name))))
       (map decrypt)
       (filter (comp #{"northpole object storage"} :name))
       (first)
       (:sector-id)))
