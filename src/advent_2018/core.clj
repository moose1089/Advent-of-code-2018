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

;; Day 3

(defn match-to-claim
  [matches]
  {:id     (nth matches 0)
   :x      (nth matches 1)
   :y      (nth matches 2)
   :xlimit (+  (nth matches 1) (nth matches 3))
   :ylimit (+  (nth matches 2) (nth matches 4))
   :xsize  (nth matches 3)
   :ysize  (nth matches 4)})

(defn contained [x y claim]
  (and (< (+ -1 (:x claim)) x (+ 2 (:xlimit claim)))
       (< (+ -1 (:y claim)) y (+ 2 (:ylimit claim)))))

(defn corners [c]
  [[(inc (:x c)) (inc (:y c))]
   [(inc (:x c)) (:ylimit c)]
   [(:xlimit c) (:ylimit c)]
   [(:xlimit c) (inc (:y c))]]
  )

(defn overlapsub [a b]
  (let [four-corners (corners a)]
;    (println "corners" four-corners)
    (some true? (map #(contained (first %) (second %) b) four-corners))))

(defn overlap [a b]
  (or (overlapsub a b) (overlapsub b a)))

(defn f3-1 [args]
  (let [l      (clojure.string/split-lines (slurp *in*))
        claims (map (fn [x] (match-to-claim (map #(Integer/parseInt %) (rest (re-matches #"#(.*) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)" x))))) l)
        maxx   (reduce max 0 (map :xlimit claims))
        maxy   (reduce max 0 (map :ylimit claims))]
    (println "maxes " maxx maxy)
    (let [r (count (filter true?
                           (for [x (range (inc maxx))
                                 y (range (inc maxy))]
                             (do
                               (let [numclaims (count (filter true? (for [c claims] (contained x y c))))]
                                 ;;(println "checking " x y "= " numclaims)
                                 (< 1 numclaims))))))]
      (println r "inches claimed by more than 1 elf"))))


(defn f3-2 [args]
  (let [l      (clojure.string/split-lines (slurp *in*))
        claims (map (fn [x] (match-to-claim (map #(Integer/parseInt %) (rest (re-matches #"#(.*) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)" x))))) l)]
    (let [r (filter some? (for [c1 claims]
                            (do
                              (let [overlaps
                                    (count (filter true? (for [c2 claims]
                                                           (when (not= c1 c2)
                                                             (overlap c1 c2)))))]
                                (when (= 0 overlaps) c1)))))]
      (println "non overlapping claims" (count r))
      (println "non overlapping claims" r))))

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
    3.1M (f3-1 args)
    3.2M (f3-2 args)
    9.1M (f9 args)
    )
  )
