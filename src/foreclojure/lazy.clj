(ns foreclojure.lazy)

;; question:

(defn yadda-iter
  [v1 v2 v3]

  (+ (first v1)
     (first v2)
     (first v3)))

(defn yadda-lazy
  [len]

  (letfn [(inner [v1 v2 v3]
            (cons (yadda-iter v1 v2 v3)
                  (lazy-seq (inner (rest v1)
                                   (rest v2)
                                   (rest v3)))))]
    (let [base (cycle (range len))]
      (inner base
             (map #(* %1 %1) base)
             (map #(* %1 %1 %1) base)))))

(defn yadda-loop
  [len iters]

  (let [base (cycle (range len))]
    (loop [result nil
           i 0
           v1 base
           v2 (map #(* %1 %1) base)
           v3 (map #(* %1 %1 %1) base)]
      (if (= i iters)
        result
        (recur (cons (yadda-iter v1 v2 v3) result)
               (inc i)
               (rest v1)
               (rest v2)
               (rest v3))))))

;; Is there a way to create a lazy sequence using the same style as loop/recur? I like yadda-loop better, because:

;; It's more obvious what the initial conditions are and how the algorithm progresses to the next iteration.
;; It won't suffer from a stack overflow due to tail optimization.
