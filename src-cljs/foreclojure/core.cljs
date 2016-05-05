(ns foreclojure.core)

(defn by-id [id]
  (. js/document getElementById id))

(defn log [msg]
  (. js/console log msg))

(defn two []
  (let [div (by-id "canvas")]
    (when-let [two (by-id "two")]
      (.removeChild div two))
    (.appendTo (js/Two. (clj->js {:width (.-clientWidth div)
                                  :height 600}))
               div)))

(def size {:width 30 :height 30 :padding 5})

(defn loc [x y w]
  (let [{:keys [width height padding]} size]
    ))

(defn draw [t]
  (let [ws (transient {})]
    (letfn [(traverse [t f]
              ((fn step [[h & rs] x y]
                 (when h
                   (f h x y)
                   (when-not (coll? h)
                     (assoc! ws y (inc (get ws y 0))))
                   (cons (if (coll? h) (step h 0 (inc y)) h)
                         (step rs (inc x) y))))
               t 0 0))]
      (persistent! ws))))

(defn init []
  (let [two (two)
        circle (. two makeCircle 72 100 50)
        rect (. two makeRectangle 213 100 100 100)]
    (set! (.-fill circle) "#FF8000")
    (set! (.-stroke circle) "orangered")
    (set! (.-lineWidth circle) 5)
    (. two update)))
