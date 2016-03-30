(ns foreclojure.core
  (:use [clojure.test]))

;; solution 58
;; imperative -- probably faster
(defn solution-58-loop [& fns]
  (fn [& args]
    (let [fns (reverse fns)]
      (loop [fs (next fns)
             res (apply (first fns) args)]
        (if fs
          (recur (next fs)
                 ((first fs) res))
          res)))
    )
  )

;; golfy -- we're creating an unecessary array each time.
(defn solution-58 [& fns]
  (fn [& args]
    (first
     (reduce #(vector (apply %2 %1)) args (reverse fns)))))

;; -> ([1 2 3 4]) (reverse rest)
;; 1> (vector (apply reverse ([1 2 3 4]))) -> [[4 3 2 1]]
;; 2> (vector (apply rest [[4 3 2 1]])) -> [[3 2 1]]
;; 3> (first [[3 2 1]]) -> [3 2 1]

(is (= [3 2 1] ((solution-58 rest reverse) [1 2 3 4])))


(is (= true ((solution-58 zero? #(mod % 8) +) 3 5 7 9)))

;; exploring reduce
(let [s "some bunch of words with a bunch of words in it"]
  (reduce #(assoc %1 %2 (inc (get %1 %2 0)))
          {}
          (re-seq #"\w+" s)))

;; solution 59

(defn solution-59 [& fns]
  (fn [& args]
    (reduce #(conj %1 (apply %2 args)) [] fns)))

;; should have been 'map' since this is a one-to-one transformation

(defn solution-59-fixed [& fns]
  (fn [& args]
    (map #(apply % args) fns)))

(is (= [21 6 1] ((solution-59-fixed + max min) 2 3 5 1 6 4)))

(is (= ["HELLO" 5] ((solution-59 #(.toUpperCase %) count) "hello")))

(is (= [2 6 4] ((solution-59 :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10})))

;; solution 60

(def solution-60
  (fn my-reduce
    ([f val coll]
     (letfn [(step [v c]
               (lazy-seq
                (if c
                  (cons v (step (f v (first c)) (next c)))
                  (list v)))) ; this will be the 2nd arg to 'cons', so it must be a list to complete the sequence.
             ]
       (step val coll)))
    ([f coll]
     (my-reduce f (first coll) (rest coll))))
  )

;; does this help? http://stackoverflow.com/questions/21712145/lazy-sequence-using-loop-recur
;; this answer is amazing and deserves a blog post to fully digest.

;(Is (= (take 5 (solution-60 + (range))) [0 1 3 6 10]))
