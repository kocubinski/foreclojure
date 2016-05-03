(ns foreclojure.triangle)


;; proces all combinations
;; find min & max across x,y - 4 points
;; two points should be identical (if not, not a triange)
;; calculate area -- if not equal to (count 1s), not well-formed triangle
;; max area wins

;;http://mathforum.org/library/drmath/view/55169.html

(defmacro dbg-prn [& body]
  `(let [res# ~@body]
     (println res#)
     res#))

(defn gcd [x y]
  (let [[x y] (sort [(Math/abs x) (Math/abs y)])]
    (println "gcd" x y)
    (if-let [r (when-not (zero? x) (mod y x))]
      (if (zero? r) x (gcd r x)) y)))

(defn area [[x1 y1] [x2 y2] [x3 y3]]
  (println x1 y1 x2 y2 x3 y3)
  (let [a (/ (+ (- (* x1 y2) (* x2 y1))
                (- (* x2 y3) (* x3 y2))
                (- (* x3 y1) (* x1 y3)))
             2)
        b (/ (+ (gcd (- x1 x2) (- y1 y2))
                (gcd (- x2 x3) (- y2 y3))
                (gcd (- x3 x1) (- y3 y1))) 2)]
    (println (double a) (double b))
    (Math/abs (- (if (neg? a) (dec a) (inc a)) (double b)))))

(def s-127
  (fn [ns]
    (letfn [(most [f g coll] (reduce (fn [m n] (if (f (g n) (g m)) n m)) coll))
            ;; (gcd [x y] (let [[x y] (sort [(Math/abs x) (Math/abs y)])]
            ;;              (println x y)
            ;;              (if-let [r (when-not (zero? x) (mod y x))]
            ;;                (if (zero? r) x (gcd r x)) y)))
            ;; (area [[x1 y1] [x2 y2] [x3 y3]]
            ;;   (let [a (/ (+ (- (* x1 y2) (* x2 y1))
            ;;                 (- (* x2 y3) (* x3 y2))
            ;;                 (- (* x3 y1) (* x1 y3))) 2)
            ;;         b (/ (+ (gcd (- x1 x2) (- y1 y2))
            ;;                 (gcd (- x2 x3) (- y2 y3))
            ;;                 (gcd (- x3 x1) (- y3 y1))) 2)]
            ;;     (- (inc a) (double b))))
            (bits [n] (->> (Integer/toBinaryString n)
                           (map-indexed vector)
                           (filter #(= \1 (second %)))
                           (map first)))]
      (let [ps (->> ns (map-indexed (fn [i n] [i (bits n)]))
                    (mapcat (fn [[x ys]] (map (partial vector x) ys))))]
        (->> (range (int (Math/pow 2 (count ps))) 2 -1)
             (map bits)
             (map (partial map (partial nth ps)))
             ;; reduce complexity by binding to 4 sorted lists of points
             (map (fn [ps] [ps (-> (list (most < first ps) (most < second ps)
                                        (most > first ps) (most > second ps))
                                  set vec)]))
             (filter (fn [[_ vs]] (= 3 (count vs))))
             (map (fn [[ps [v1 v2 v3 :as v]]]
                    [v (area v1 v2 v3)]))
             ;; (map (fn [[ps [v1 v2 v3]]]
             ;;        (let [a (-> (area v3 v2 v1) int Math/abs)]
             ;;          (if (= a (count ps)) a 0))))
             ;;(apply max)
             )
        ))
  ))

(def s-127-a
  (fn [ns]
    (letfn [(most [f g coll] (reduce (fn [m n] (if (f (g n) (g m)) n m)) coll))
            (bits [n] (->> (Integer/toBinaryString n)
                           (map-indexed vector)
                           (filter #(= \1 (second %)))
                           (map first)))]
      (let [ps (->> ns (map-indexed (fn [i n] [i (bits n)]))
                    (mapcat (fn [[x ys]] (map (partial vector x) ys))))]
        (->> (range (int (Math/pow 2 (count ps))) 2 -1)
             (map bits)
             (map (partial map (partial nth ps)))
             (map (fn [ps] [ps (-> (list (most < first ps) (most < second ps)
                                        (most > first ps) (most > second ps))
                                  set vec)]))
             ;; (filter (fn [[_ vs]] (= 3 (count vs))))
             )
        ))
    ))

;; visit each 1
;; try to draw lines @ 90 deg in all directions
;; a hit is +1 in each dir
;; on hit, attempt to fill to complete triangle
;; area is running filled for a given visited 1

(defn bits [n]
  (->> (Integer/toBinaryString n)
       (map-indexed vector)
       (filter #(= \1 (second %)))
       (map first)))

(def s-127-vectors
  (fn [ns]
    (let [ms (->> ns (map-indexed (fn [i n] [i (Integer/toBinaryString n)]))
                  (mapcat (fn [[x ys]] (map-indexed (fn [y b] [[x y] b]) ys)))
                  (filter #(= \1 (second %))) (into {}))]
      (letfn [(add [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])
              (tri [[x y :as p] v1 v2]
                )]
        ms)))
  )
