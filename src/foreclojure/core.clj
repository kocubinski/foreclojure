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

;; solution 61
(def solution-61
  (fn [ks vs]
    (into {} (map vector ks vs)))
  )

(def solution-62
  (fn step [f x]
    (lazy-seq
     (cons x (step f (f x))))
    )
  )

(def solution-63
  (fn [f s]
    (reduce (fn [m v]
              (update-in m [(f v)] #(if % (conj % v) [v]))) {} s))
  )

(def solution-65
  (fn [s]
    (let [len (count s)]
      (cond
        (= (inc len) (count (conj s [::x ::x] [::x ::y]))) :map
        (= (inc len) (count (conj s ::x ::x))) :set
        (= (first (conj s ::x ::y)) ::y) :list
        (= (last (conj s ::x ::y)) ::y) :vector
        )))
  )

(def solution-66
  (fn [x y]
    (loop [n (min x y)]
      (if (and (= (mod x n) 0)
               (= (mod y n) 0)) n
          (recur (dec n)))))
  )

(def solution-67
  (fn [len]
    (loop [i 1
           primes [2]
           ns (iterate inc 3)]
      (if (= i len)
        primes
        (let [n (first ns)]
          (if (some zero? (map #(mod n %) primes))
            (recur i primes (rest ns))
            (recur (inc i) (conj primes n) (rest ns)))
          ))))
  )

(def solution-69
  (fn [f & ms]
    (into {}
          (map (fn [k]
                 (let [vals (filter identity (map #(get % k) ms))]
                   [k (if (= (count vals) 1) (first vals) (apply f vals))]))
               (-> (map keys ms) flatten set))))
  )

(def solution-70
  (fn [s]
    (sort-by #(.toLowerCase %) (re-seq #"\w+" s)))
  )

;; this sucks
(def solution-73
  (fn [board]
    (letfn [(get-points [sym m]
              (filter identity
                      (for [x (range (count m))
                            y (range (count (m 0)))]
                        (when (= ((m x) y) sym)
                          [x y]))))
            (line? [[x1 y1] [x2 y2]]
              (or (= x1 x2)
                  (= y1 y2)
                  (= (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))))
            (slope [[x1 y1] [x2 y2]]
              (let [dx (- x2 x1)
                    dy (- y2 y1)]
                [x2 y2 (cond
                         (and (= dx 0) (= dy 0)) ::NaN
                         (= dx 0) ::Inf
                         :else (/ dy dx))]))
            (winner? [sym]
              (let [points (get-points sym board)
                    clumps (map (fn [point]
                                  (->> points
                                       (filter #(line? point %))
                                       (map #(slope point %))))
                                points)
                    lines (map (fn [clump]
                                 (map (fn [p]
                                        (map #(line? % p) clump))
                                      clump))
                               clumps)]
                (some #(and (> (count %) 2)
                            (apply = (->> % (map last) (filter (partial not= ::NaN)))))
                      clumps)))]
      (cond
        (winner? :x) :x
        (winner? :o) :o)))
  )

;; god dammit
(defn jbear-73 [b]
  (first (some #{[:x :x :x] [:o :o :o]}
               (concat b (apply map list b)
                       (map
                        #(map (fn [a i] (a i)) b %)
                        [[0 1 2] [2 1 0]])))))

(def board-1 [[:x :e :o]
              [:x :e :e]
              [:x :e :o]])

(def board-2 [[:o :e :o]
              [:o :o :e]
              [:x :e :x]])

(def board-3 [[:x :e :o]
              [:x :x :e]
              [:o :x :o]])

(def solution-74
  (fn [s]
    (clojure.string/join
     ","
     (filter (fn [n]
               (let [r (Math/sqrt (Integer/parseInt n))]
                 (= (-> r int double) r)))
             (clojure.string/split s #","))))
  )

(def solution-75
  (fn [n]
    (if (= n 1) 1
        (letfn [(gcd [m]
                  (->> (reverse (range 1 (inc m)))
                       (filter #(= 0 (mod n %) (mod m %)))
                       first))]
          (count (filter #(= (gcd %) 1) (reverse (range 1 n)))))))
  )

(def out-76
  (letfn
     [(foo [x y] #(bar (conj x y) y))
      (bar [x y] (if (> (last x) 10)
                   x
                   #(foo x (+ 2 y))))]
     (trampoline foo [] 1)))

(def solution-77
  (fn [words]
    (->> words
         (group-by sort)
         vals
         (filter #(> (count %) 1))
         (map set)
         set))
  )

(def solution-78
  (fn [f & args]
    (loop [f (apply f args)]
      (if (fn? f)
        (recur (f))
        f)))
  )

(def solution-79
  (fn [graph]
    (let [g (vec graph)
          nodes
          (->> g
               (map #(-> % count range))
               (map (fn [y v]
                      (map (fn [x] [y x]) v))
                    (range)))
          paths
          (loop [paths [ [[0 0]] ]
                 ns (rest nodes)]
            (if-not ns
              paths
              (recur
               (apply concat
                      (map (fn [path]
                             (let [[px py] (last path)]
                               (->> (first ns)
                                    (map (fn [[x y]]
                                           (when (or (= y py) (= y (inc py)))
                                             (conj path [x y]))))
                                    (filter identity))))
                           paths))
               (next ns))))]
      (apply min
       (map (fn [path] (apply + (map
                                (fn [[x y]] (get (get g x) y))
                                path)))
            paths))))
  )

;; 2 1 -> 3 1 or 3 2
;; 3 1 -> 4 1 or 4 2

;; REALLY?
(defn jbear-79 [t]
  (first (reduce (fn [a s]
                   (println a s)
                   (map +
                        s
                        (map (fn [n]
                               ;(println n)
                               (apply min n))
                             (partition 2 1 a))))
                 (reverse t)))
  )

(def graph-1
  '([1]
    [2 4]
    [5 1 4]
    [2 3 4 5]))

(def graph-2
     '([3]
      [2 4]
     [9 9 3]
    [9 9 9 4]
   [4 6 6 7 8]
  [5 1 5 5 1 4]))

(def solution-80
  (fn [n]
    (= n
       (->> (range 1 n)
            (filter #(= 0 (mod n %)))
            (apply +))))
  )

(def solution-81
  (fn [s1 s2]
    (set
     (filter #(and (contains? s1 %) (contains? s2 %))
             (concat s1 s2))))
  )

;; cat -> cot -> coat -> oat -> hat -> hot -> hog -> dog

(defmacro dbg-prn [& body]
  `(let [res# ~@body]
     (println res#)
     res#))

(defn chain? [w1 w2]
  (let [compare (fn [w1 w2]
                  (count (filter false? (map = w1 w2))))
        l1 (count w1) l2 (count w2)
        [small large] (if (< l1 l2) [w1 w2] [w2 w1])]
    (cond
      (= l1 l2) (<= (compare w1 w2) 1)

      (= 1 (Math/abs (- l1 l2)))
      (some #{0}
            (map
             (partial compare small)
             (map (fn [i]
                    (->> (map vector (range) large)
                         (remove #(= (first %) i))
                         (map second)
                         (apply str)))
                  (range (count large)))))

      :else nil
      )))

(defn old-chain []
  (fn [w1 w2]
    (and (not= w1 w2)
         (<= (Math/abs (- (count w1) (count w2))) 1)
         (<= (count (clojure.set/difference (set w1) (set w2))) 1)))
  )

(def solution-82
  (fn [words]
    (let [chain?
          (fn [w1 w2]
            (let [compare (fn [w1 w2]
                            (count (filter false? (map = w1 w2))))
                  l1 (count w1) l2 (count w2)
                  [small large] (if (< l1 l2) [w1 w2] [w2 w1])]
              (cond
                (= l1 l2) (<= (compare w1 w2) 1)

                (= 1 (Math/abs (- l1 l2)))
                (some #{0}
                      (map
                       (partial compare small)
                       (map (fn [i]
                              (->> (map vector (range) large)
                                   (remove #(= (first %) i))
                                   (map second)
                                   (apply str)))
                            (range (count large)))))

                :else nil
                )))
          rows (repeat (count words) (into '() words))
          chain (first
                  (reduce (fn [ps cs]
                            (for [p ps
                                  c cs
                                  :when (and (not (some #{c} p))
                                             (chain? (last p) c))]
                              (conj p c)))
                          (map vector (first rows))
                          (rest rows)))]
      (if chain true false)))
  )

(defn permute [n]
  (let [rows (repeat n (range n))]
    (reduce (fn [ps cs]
              (for [p ps
                    c cs
                    ;:when (not (some #{c} p))
                    ]
                (conj p c)
                ))
            (map vector (first rows))
            (rest rows))
    ))

(def solution-84
  (fn [br]
    (let [ps (into {} br)
          ns (->> br (reduce conj '()) flatten set)]
      ;; some logic errors with n -> v, but the structure for reducing the chains is (I think) sound
      (reduce (fn [chains n]
                (let [v (ps n)]
                  (if-let [[i f]
                           (->>
                            chains
                            (map (fn [i c] (condp = v
                                            (peek c) [i #(->> % (cons v) vec)]
                                            (first c) [i #(conj % v)]
                                            nil))
                                 (range))
                            (filter identity) first)]
                    (update-in chains [i] f)
                    (conj chains n)))))))
  )
