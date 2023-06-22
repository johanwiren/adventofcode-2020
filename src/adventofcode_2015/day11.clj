(ns adventofcode-2015.day11)

(defn ->base26seq [password]
  (loop [ns (list)
         pass password]
    (if (empty? pass)
      ns
      (let [n (-> pass
                  last
                  int
                  (- 97))]
        (recur (cons n ns) (subs pass 0 (dec (count pass))))))))

(defn ->pass [base26seq]
  (->> base26seq
       (map int)
       (map (partial + 97))
       (map char)
       (apply str)))

(defn skip-iol [base26seq]
  (map (fn [i] (if ((set (->base26seq "iol")) i)
                 (inc i)
                 i))
       base26seq))

(defn has-two-pairs? [base26seq]
  (->> base26seq
       (partition-by identity)
       (filter (comp (partial <= 2) count))
       (count)
       (<= 2)))

(defn has-straight? [[n & more :as base26seq]]
  (if (= (take 3 base26seq) (range n (+ n 3)))
    true
    (if-not more
      false
      (recur more))))

(defn inc-b26 [base26seq]
  (let [n (last base26seq)
        pre (vec (butlast base26seq))]
    (if (= 25 n)
      (conj (inc-b26 pre) 0)
      (conj pre (inc n)))))

(defn next-pass [base26seq]
  (-> base26seq
      inc-b26
      skip-iol))

(defn new-passwords [pass]
  (->> (iterate next-pass (->base26seq pass))
       (filter (every-pred has-straight? has-two-pairs?))
       (map ->pass)))

(defn part-1-solver [input]
  (first (new-passwords input)))

(defn part-2-solver [input]
  (second (new-passwords input)))

(comment

  (part-2-solver "vzbxkghb")


  )
