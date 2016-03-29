(ns foreclojure.core
  (:use [clojure.test]))

;; solution 58
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

(defn solution-58 [& fns]
  (fn [& args]
    (first
     (reduce #(vector (apply %2 %1)) args (reverse fns)))))


(is (= [3 2 1] ((solution-58 rest reverse) [1 2 3 4])))

(is (= true ((solution-58 zero? #(mod % 8) +) 3 5 7 9)))
