(ns advent-2018.core
  (:gen-class))

; day 1
(defn f1-1 [args]
  (let [l (clojure.string/split-lines (slurp *in*))]
    (println (reduce + (map #(Integer/parseInt %) l)))
    ))

(defn seq-f2
  ([s]
   (seq-f2 0 s))
  ([c s] (lazy-seq
          (cons c (seq-f2 (+ c (first s)) (rest s))))))

(defn f1-2 [args]
  (let [l  (clojure.string/split-lines (slurp *in*))
        fd (map #(Integer/parseInt %) l)
        f  (seq-f2 (cycle fd))]
    (loop [seen-set (hash-set)
           current-freq (first f)
           frequencies f]
      (if (contains? seen-set current-freq)
        (println current-freq)
        (recur (conj seen-set current-freq) (first (rest frequencies)) (rest frequencies))))))

;; day 2
(defn f2-1 [args]
  (let [l (clojure.string/split-lines (slurp *in*))
        f-2-3 (frequencies (flatten
                      (map (fn [r] (->> r
                                       frequencies
                                       vals
                                       distinct
                                       (filter #(<= 2  % 3))
                                       ;;                              ((juxt #(first (not-empty (filter #{2} %))) #(first (not-empty (filter #{3} %)))))
                                       ))
                           l)))]
    (println (apply * (vals f-2-3)))))

(defn word-dist
  [a b]
  (count (filter #(apply not= (take 2 %)) (map #(vector %1 %2) a b))))

(defn f2-2 [args]
  (let [l (clojure.string/split-lines (slurp *in*))
        wcd (word-dist (first l) (second l))
        distances (for [a l
                        b l]
                    (when (not= a b)
                      [(word-dist a b) a b]))
        close (filter #(= (first %) 1) distances)
        first-close-items (rest (first close))]
    (println "close item " close)
    (println "close item " (clojure.string/join (map first
                                                     (filter #(apply = (take 2 %)) (apply map #(vector %1 %2) first-close-items)))))))


;; Day 9


(defn insert-marble
  [m n player-num]
  (cond (zero? n)
        {0 [0 0]
         :current 0
         :scores {player-num 0}}
        (= 1 n) {:current 1 0 [1 1] 1 [0 0] :scores {0 0}}
        (= 0 (mod n 23))
        (let [current (:current m)
              seven-left (-> current m first m first m first m first m first m first m first)
              eight-left (-> seven-left m first)
              six-left (-> seven-left m second)
              working-map (assoc m
                                 :current six-left
                                 eight-left [(-> eight-left m first) six-left]
                                 six-left [eight-left (-> six-left m second)])]
          (update-in (dissoc working-map seven-left)
                     [:scores player-num] (fnil + 0) (+ seven-left n)))
        :else
        (let
         [current (:current m)
          t1 (-> current m second)
          t2 (-> t1 m second)
          t2-next (-> t2 m second)]
          (assoc m
                 t1 [current n]
                 t2 [n t2-next]
                 n [t1 t2]
                 :current n))))

(defn play-upto [players max-marble]
  (loop  [marble 0
          m nil]
    (when (zero? (mod marble 100000))
      (println "progress" (format "%.0f%%" (float (* 100 (/ marble max-marble))))))
    (if (> marble max-marble)
      m
      (recur (inc marble) (insert-marble m marble (mod marble players))))))

(defn max-points [m]
  (apply max (vals (:scores m))))

(defn f9 [args]
  (println "maxpoints="
   (max-points (apply play-upto (map #(Integer/parseInt %) args)))))

(defn -main
  "I don't do a whole lot ... yet."
  [h & args]
  (case (BigDecimal. h)
    1.1M (f1-1 args)
    1.2M (f1-2 args)
    2.1M (f2-1 args)
    2.2M (f2-2 args)
    9.1M (f9 args)
    )
  )
