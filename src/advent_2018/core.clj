(ns advent-2018.core
  (:gen-class))

(defn f1 [args]
  (let [l (clojure.string/split-lines (slurp *in*))]
    (println (reduce + (map #(Integer/parseInt %) l)))
    ))

(defn seq-f2
  ([s]
   (seq-f2 0 s))
  ([c s] (lazy-seq
          (cons c (seq-f2 (+ c (first s)) (rest s))))))

(defn f2 [args]
  (let [l  (clojure.string/split-lines (slurp *in*))
        fd (map #(Integer/parseInt %) l)
        f  (seq-f2 (cycle fd))]
    (loop [seen-set (hash-set)
           current-freq (first f)
           frequencies f]
      (if (contains? seen-set current-freq)
        (println current-freq)
        (recur (conj seen-set current-freq) (first (rest frequencies)) (rest frequencies))))))

(defn -main
  "I don't do a whole lot ... yet."
  [h & args]
  (case (BigDecimal. h)
    1.1M (f1 args)
    1.2M (f2 args)
    )
  )
