(ns adventofcode-2024.day-09
  (:require
   [clojure.test :as t]
   [utils :as u]))

(def input (u/line-seq-input *ns*))

(defn parse [input]
  (->> input
       (first)
       (re-seq #"\d")
       (map parse-long)
       (partition-all 2)
       (map-indexed vector)
       (map (fn [[file-id [blocks free]]]
              [(repeat blocks file-id)
               (repeat (or free 0) nil)]))
       (flatten)))

(defn part-1-solver [input]
  (let [blocks (->> (parse input)
                    (vec))
        free-list (->> blocks
                       (map-indexed vector)
                       (filter (comp nil? second))
                       (map first))]
    (->> (range (count blocks) -1 -1)
         (filter #(get blocks %))
         (reduce (fn [{:keys [blocks free-list] :as acc} block-id]
                   (cond
                     (nil? (get blocks block-id))
                     acc

                     (empty? free-list)
                     (reduced acc)

                     (< block-id (first free-list))
                     (reduced acc)

                     :else
                     (-> acc
                         (update :blocks assoc block-id nil)
                         (update :blocks assoc (first free-list) (get blocks block-id))
                         (update :free-list rest))))
                 {:blocks blocks
                  :free-list free-list})
         :blocks
         (filter some?)
         (map-indexed vector)
         (map (fn [[i file-id]]
                (* i file-id)))
         (reduce +))))

(defn part-2-solver [input]
  (let [fs
        (->> (first input)
             (re-seq #"\d")
             (map parse-long)
             (partition-all 2)
             (map-indexed vector)
             (reduce (fn [{:keys [block-id] :as acc}
                          [file-id [file-blocks free-blocks]]]
                       (-> acc
                           (update :block-id + file-blocks (or free-blocks 0))
                           (assoc-in [:blocks block-id] {:file-id file-id
                                                         :blocks file-blocks})
                           (assoc-in [:free (+ block-id file-blocks)] {:blocks free-blocks})))
                     {:free (sorted-map)
                      :blocks (sorted-map-by >)
                      :block-id 0}))]
    (->> (:blocks fs)
         (reduce (fn [fs [start-block {:keys [blocks] :as block}]]
                   (if-let [target (->> (:free fs)
                                        (filter (comp :blocks second))
                                        (filter (comp #(<= blocks %) :blocks second))
                                        (first))]
                     (if (< (first target) start-block)
                       (cond-> fs
                         :t (update :free dissoc (first target))
                         :t (update :free assoc start-block block)
                         :t (update :blocks dissoc start-block)
                         :t (update :blocks assoc (first target) block)
                         (< blocks (-> target second :blocks))
                         (update :free
                                 assoc
                                 (+ (first target) blocks)
                                 {:blocks (- (-> target second :blocks) blocks)}))
                       fs)
                     fs))
                 fs)
         :blocks
         (reduce (fn [acc [start-block {:keys [file-id blocks]}]]
                   (+ acc (->> (iterate inc start-block)
                               (take blocks)
                               (map #(* file-id %))
                               (reduce +))))
                 0))))

(t/deftest part-1-test
  (t/is (= 6390180901651 (part-1-solver input))))

(t/deftest part-2-test
  (t/is (= 6412390114238 (part-2-solver input))))
