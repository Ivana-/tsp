(ns shared.utils)

(defn rand-n [n] (int (* n (Math/random))))

(defn rand-from-to [from to] (+ from (int (* (- to from -1) (Math/random)))))

(defn safe-min-key [f coll] (when-not (empty? coll) (apply min-key f coll)))
(defn safe-max-key [f coll] (when-not (empty? coll) (apply max-key f coll)))

(defn swap-vec [v i j] (assoc v i (v j) j (v i)))

(defn delete-vec [v i]
  (into (subvec v 0 i) (subvec v (inc i))))

(defn insert-vec [v i e]
  (into (conj (subvec v 0 i) e) (subvec v i)))

(defn swap-range
  "swap elements order in a given vector from index i to j included both!"
  [v i j]
  (let [from (min i j)
        to (max i j)]
    (loop [curr from
           result (transient v)]
      (if (> curr to)
        (persistent! result)
        (recur (inc curr) (assoc! result curr (v (+ from (- to curr)))))))))
