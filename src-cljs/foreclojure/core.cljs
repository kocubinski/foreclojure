(ns foreclojure.core)

(defn by-id [id]
  (. js/document getElementById id))

(defn two []
  (let [div (by-id "canvas")]
    (when-let [two (aget (.-children div) 0)]
      (.removeChild div two))
    (.appendTo (js/Two. (clj->js {:width (.-clientWidth div)
                                  :height 600}))
               div)))

(def size {:width 30 :height 30 :padding 5})

(defn loc [x y w]
  (let [{:keys [width height padding]} size]
    ))

(defn walk [t f]
  ((fn step [[h & rs] x y]
     (when h
       (f h x y)
       (cons (if (coll? h) (step h 0 (inc y)) h)
             (step rs (inc x) y))))
   t 0 0))



(defn draw [t]
  (let [two (two)
        ys (transient {})]
    (walk t (fn [h x y] (when-not (coll? h)
                         (assoc! ys y (inc (get ys y 0))))))
    (walk t (fn [n x y]
              (when-not (coll? n)
                (let [r (/ 1 (inc (get ys y)))
                      cx (* x r (.-width two)) cy (* y 70)]
                  (println n r x y "@" cx cy)
                  (. two makeCircle cx cy 25)
                  (. two makeText (str n) cx cy)))))
    (. two update)
    (persistent! ys)))

(defn draw-node [two n x y]
  (println "draw" n "at" x y)
  (let [y (* y 70)]
    (. two makeCircle x y 20)
    (. two makeText (str n) x y)))

(defn draw-2 [t]
  (let [two (two)
        w 70 ;; todo resolve from depth-first walk
        ]
    ((fn step [n x y]
       (if (coll? n)
         (let [[p l r] n]
           (step p x y)
           (and l (step l (- x w) (inc y)))
           (and r (step r (+ x w) (inc y))))
         (draw-node two n x y)))
     t (/ (.-width two) 2) 1)
    (. two update)))

(defn init []
  (let [two (two)
        circle (. two makeCircle 72 100 50)
        rect (. two makeRectangle 213 100 100 100)]
    (set! (.-fill circle) "#FF8000")
    (set! (.-stroke circle) "orangered")
    (set! (.-lineWidth circle) 5)
    (. two update)))
