(ns foreclojure.core
  (:require
   [goog.events :as gevents]
   [clojure.string :as str]
   [cljs.reader :as reader]))

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
        lines (. two makeGroup)
        w 70
        ys (explore t)
        seen (atom (zipmap (keys ys) (repeat 0)))]
    ((fn step [n x y]
       (doseq [node n]
         (if-not (coll? node)
           (draw-node two node x y)
           (let [ny (inc y)
                 nx (* (. two -width) (/ (inc (@seen ny)) (inc (ys ny))))]
             (swap! seen update ny inc)
             (.add lines (draw-line two x (* y row-height) nx (* ny row-height)))
             (step node nx ny)))))
     t (/ (. two -width) 2) 1)
    (. two update)))

(defn str-tree [t]
  ((fn step [[c & r] indent]
     (when c
       (condp re-matches c
         #"\[|\(" (str (if-not (= indent 0) "\n") (apply str (repeat (inc indent) \space)) c
                       (step r (inc indent)))
         #"\]|\)" (str c (step r (dec indent)))
         (str c (step r indent)))))
   (str t) 0))

(defn on-text-change [e]
  (let [v (-> e .-target .-value)
        t (try (reader/read-string v) (catch js/Error e nil))]
    (. js/console log v t)
    (when (and t (not= 32 (.-keyCode e)))
      (set! (-> e .-target .-value)
            (str-tree
             (-> v
                 (str/replace #" " "")
                 (str/replace #"\n" ""))))
      (draw-any t))))

(defonce init
  (gevents/listen
   (by-id "txt-tree") goog.events.EventType.KEYUP
   (fn [e] (on-text-change e))))

(def test-tree
  [1 [2
      [3
       ['a]
       ['b]]
      [4
       ['c]
       ]]
   [5
    [6
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
        (l)
        (q)
        (r))
       (m
        (n)
        (o)))))))
