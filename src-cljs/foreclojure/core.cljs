(ns foreclojure.core)

(def row-height 70)

(defn by-id [id]
  (. js/document getElementById id))

(defn two []
  (let [div (by-id "canvas")]
    (when-let [two (aget (.-children div) 0)]
      (.removeChild div two))
    (.appendTo (js/Two. (clj->js {:width (.-clientWidth div)
                                  :height 600}))
               div)))

(defn explore [t]
  (let [ys (transient {})]
    ((fn step [[h & rs] x y]
       (when h
         (when-not (coll? h)
           (assoc! ys y (inc (get ys y 0))))
         (cons (if (coll? h) (step h 0 (inc y)) h)
               (step rs (inc x) y))))
     t 0 1)
    (persistent! ys)))

(defn draw-node [two n x y]
  (let [y (* y row-height)]
    (println "draw" n "at" x y)
    (. two makeCircle x y 20)
    (. two makeText (str n) x y)))

(defn draw-line [two x1 y1 x2 y2]
  (. two makeLine x1 y1 x2 y2))

(defn draw-binary [t]
  (let [two (two)
        w 70 ]
    ((fn step [n x y]
       (if (coll? n)
         (let [[p l r] n
               dx (* (. two -width) (- (/ 1 (+ y 1)) (/ 1 (+ y 2))))
               walk (fn [n nx ny]
                      (draw-line two x (* y row-height) nx (* ny row-height))
                      (step n nx ny))]
           (walk p x y)
           (and l (walk l (- x dx) (inc y)))
           (and r (walk r (+ x dx) (inc y))))
         (draw-node two n x y)))
     t (/ (. two -width) 2) 1)
    (. two update)))

(defn draw-any [t]
  (let [two (two)
        w 70
        ys (explore t)]
    ((fn step [n x y]
       (if (coll? n)
         (let [[p l r] n
               dx (* (. two -width) (- (/ 1 (+ (ys y) 1)) (/ 1 (+ (ys y) 2))))
               walk (fn [n nx ny]
                      (draw-line two x (* y row-height) nx (* ny row-height))
                      (step n nx ny))]
           ;; find drawing general cases
           (walk p x y)
           (and l (walk l (- x dx) (inc y)))
           (and r (walk r (+ x dx) (inc y))))
         (draw-node two n x y)))
     t (/ (. two -width) 2) 1)
    (. two update)))

(def test-tree
  [1 [2
      [3
       ['a]
       ['b]]
      [4
       ['c]
       ['d]]]
   [5
    [6
     ['e]
     ['f]]
    [7
     ['g]
     ['h]]]])

(def strange-tree
  '(c
    (d)
    (e)
    (b
     (f
      (g)
      (h))
     (a
      (i
       (j
        (k)
        (l))
       (m
        (n)
        (o)))))))
