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

(defn chain [k m]
  (println k m)
  (if-let [v (m k)]
    (dbg-prn (cons k (chain v (dissoc m k))))
    (list k)))

(def solution-84
  (fn [br]
    (letfn [(chain [k m]
              (if-let [v (m k)]
                (cons k (chain v (dissoc m k)))
                (list k)))
            (build [m]
              (if (seq m)
                (if-let [c (apply max-key count (map #(chain % m) (keys m)))]
                  (cons c (build (apply dissoc m c))))))
            (expand [s]
              (when (next s)
                (cons (map #(vector (first s) %) (rest s))
                      (expand (next s))))
              )]
      (->> (into {} br) build (map expand) (apply concat) (apply concat) set)))
  )

;; ok, fuck you jbear
(def jbear-84
  (fn transitive-closure [rels]
    (let [tc (reduce (fn [rs [a b]]
                       (into rs (keep #(if (= b (first %))
                                         [a (second %)])
                                      rs)))
                     rels rels)]
      (if (= tc rels) tc (recur tc)))))

;; my favorite
(def adereth-84
  #(loop [s %]
     (let [n (into s
                   (for [[a b] s [c d] s
                         :when (= b c)]
                     [a d]))]
       (if (= n s) n (recur n)))))

(def solution-85
  (fn power-set [s]
    (set
     (conj
      (if (empty? s) #{}
          (let [e (first s)
                t (disj s e)
                pt (power-set t)]
            (cons t (concat pt (map #(conj % e) pt)))))
      s)))
  )

(def solution-86
  (fn happy [n]
    (letfn [(digits [n]
              (if (< n 10) (list (int n))
                  (cons (mod n 10) (digits (Math/floor (/ n 10))))))]
      (let [ns (digits n)
            s (reduce #(+ %1 (Math/pow %2 2)) (Math/pow (first ns) 2) (rest ns))]
        (if (= n 1)
          true
          (if (= n 4) false
              (recur (int s)))))))
  )

(def graph-89-1
  [[:a :b] [:a :b] [:a :c] [:c :a]
   [:a :d] [:b :d] [:c :d]])

(def s-89
  (fn [g]
    (let [vs (set (flatten g))]
      (map (fn [v] (count (filter #(some #{v} %) g))) vs)))
  )

(def s-89-golf
  (fn [g]
    (or (= (count g) 1)
        (->> (set (flatten g))
             (map (fn [v] (count (filter #(some #{v} %) g))))
             (every? even?))))
  )

(def s-91-connected
  #{[1 2] [2 3] [3 1]
    [4 5] [5 6] [6 4] [3 4]})

(def s-91-unconnected
  #{[1 2] [2 3] [3 1]
    [4 5] [5 6] [6 4]})

(def s-91
  (fn [g]
    (let [vs (-> g vec flatten set)]
      (reduce (fn [ss v]
                (println v)
                (let [ns (->> g (filter #(some #{v} %)) flatten set)
                      ff (first (filter #(seq (clojure.set/intersection ns %)) ss))]
                  (println ss ff ns)
                  (if ff (assoc-in ss [(.indexOf ss ff)] (clojure.set/union ns ff))
                      (conj ss ns))))
              [] vs)))
  )

(def s-91-golf
  (fn [g]
    (= 1
       (count
        (reduce
         (fn [ss v]
           (let [ns (->> g (filter #(some #{v} %)) flatten set)
                 ff (first (filter #(seq (clojure.set/intersection ns %)) ss))]
             (if ff (assoc-in ss [(.indexOf ss ff)] (clojure.set/union ns ff))
                 (conj ss ns))))
         [] (-> g vec flatten set)))))
  )

(def s-92
  (fn [ns]
    (let [vs {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000 \0 0}]
      (:num
       (reduce
        (fn [{:keys [num last times] :as s} n]
          (let [subtract? (< (vs last) (vs n))]
            (if (= n last)
              (update-in s [:times] inc)
              (assoc s :last n :times (if subtract? 0 1)
                     :num (+ num (if subtract?
                                   (- (vs n) (* times (vs last)))
                                   (* times (vs last))))))))
        {:num 0 :last (first ns) :times 1}
        (rest (str ns 0)))))))

(def s-93
  (fn [seq]
    (letfn [(unwind [[s & more :as seq]]
              (when seq
                (if-not (sequential? (first s))
                  (cons s (unwind more))
                  (unwind (concat s more)))))]
      (unwind seq))))

(def s-94
  (fn [b]
    (let [w (-> b first count)
          h (-> b count)
          ds [-1 0 1]]
      (letfn [(neighbors [[x y]]
                (filter (fn [[x2 y2]]
                          (and (not (and (= x x2) (= y y2)))
                               (and (not= x2 -1) (< x2 w))
                               (and (not= y2 -1) (< y2 h))))
                        (for [dx ds dy ds]
                          [(+ x dx) (+ y dy)])))
              (next [[x y] ns]
                (let [live
                      (->> ns
                           (map (fn [[x y]] (-> b (get x) (get y))))
                           (filter #(= % \#))
                           count)]
                  (if (= \space (-> b (get x) (get y)))
                    (if (= live 3) \# \space)
                    (condp >= live
                      1 \space
                      3 \#
                      \space))))]
        (->>
         (for [x (range w)
               y (range h)]
           [x y])
         (map #(next % (neighbors %)))
         (partition w)
         (map (partial apply str))
         ))))
  )

(def s-94-g-1
  ["      "
   " ##   "
   " ##   "
   "   ## "
   "   ## "
   "      "]
  )

(def s-94-g2
  ["     "
   "     "
   " ### "
   "     "
   "     "])

(defn s-94-test-1 []
  (= (s-94
      ["      "
       " ##   "
       " ##   "
       "   ## "
       "   ## "
       "      "])
     ["      "
      " ##   "
      " #    "
      "    # "
      "   ## "
      "      "]))

(def s-95
  (fn [t]
    (letfn [(step [s]
              (if (or (not= 3 (count s))
                      (some #(and (not (nil? %)) (not (sequential? %))) (rest s)))
                (list false)
                (->> s (filter sequential?) (mapcat step))))]
      (empty? (step t))))
  )

(def s-95-better
  (fn tree? [t]
    (or (nil? t)
        (and
         (coll? t)
         (= 3 (count t))
         (every? tree? (next t)))))
  )

(def s-96
  (fn [[_ left right]]
    (= left
       ((fn mirror [t]
          (when t
            (cons (first t) (reverse (map mirror (next t))))))
        right)))
  )

(def s-97
  (fn [n]
    (reduce #(conj % (* %2 (last %))) [1]
            (map #(/ %1 %2) (reverse (range 1 n)) (range 1 n))))
  )

(def s-98
  (fn [f s]
    (set (map set (vals (group-by f s)))))
  )


(def s-100
  (fn [& ns]
    (let [min (apply min ns)]
      (->> (iterate (partial + min) min)
           (filter #(apply = 0 (map (partial mod %) ns)))
           first)))
  )

;; levenshtein distance

;; very inefficient - from wikipedia

(def s-101-1
  (memoize
   (fn l-dist [x y]
     (let [lx (count x) ly (count y)
           bx (butlast x) by (butlast y)
           c (if (= (last x) (last y)) 0 1)]
       (condp = 0
         lx ly
         ly lx
         (min (inc (l-dist bx y))
              (inc (l-dist x by))
              (+ c (l-dist bx by)))))))
  )

(defn deep-recursion-1 []
  (let [x [:a :b :c]
        y [1 2 3]
        z ["a" "b" "c"]]
    (letfn [(step [x y z]
              (when (and x y z)
                (println x y z)
                (cons [(first x) (first y) (first z)]
                      (concat (step (next x) y z)
                              (step x (next y) z)
                              (step x y (next z))))))]
      (step x y z)
      ;(for [xx x yy y zz z] (list xx yy zz))
      ))
  )

(def s-101-2
  (fn [x y]
    (->
     (reduce (fn [[j p] c]
               (println "outer" j p c)
               [(inc j)
                (second
                 (reduce (fn [[i r] d]
                           (println "inner" i r d)
                           (let [pi (dec i)]
                             [(inc i)
                              (conj r (if (= d c)
                                        (get p pi)
                                        (inc (min (get r pi) (get p i) (get p pi)))))]))
                         [1 [j]] x))])
             [1 (vec (-> x count inc range))] y)
     last last))
  )

(def s-102
  (fn [s]
    (apply str
           (reduce (fn [[s b] c] [(str s (if (= \- b) "" b))
                                 (if (= \- b) (Character/toUpperCase c) c)]) ["" ""] s)))
  )

(def s-103
  (fn [n s]
    (set
     (reduce (fn [ks xs]
               (for [k ks x xs
                     :when (not (get k x))]
                 (conj k x)))
             (map (comp set list) s)
             (repeat (dec n) s))))
  )

(def s-104
  (fn [s]
    (apply str
           (reverse
            (map
             (fn [[x y z] n]
               (apply str
                      (cond
                        (< n 4) (repeat n x)
                        (= n 4) [x y]
                        (= n 5) [y]
                        (< n 9) (cons y (repeat (mod n 5) x))
                        (= n 9) [x z])))
             (partition-all 3 2 "IVXLCDM")
             (reverse (map #(Character/digit % 10) (str s)))))))
  )

(def s-105
  (fn [xs]
    (second
     (reduce (fn [[k m] x] (if (keyword? x)
                            [x (assoc m x [])]
                            [k (assoc m k (conj (get m k) x))]))
             [nil {}] xs)))
  )

;; + 2 path if dist > 1 and (mod dist 2) = 0
;; (= (/ (Math/log (/ x y)) (Math/log 2)) (an integer) -- can half to solution

;; needs early-out with repetition checking
(def s-106
  (fn [x y]
    (let [m Integer/MAX_VALUE
          path (atom 10)]
      ((fn step [i n]
         (let [f (partial step (inc i))]
           (if (or (= n y) (>= i @path))
             (reset! path i)
             (min (f (* n 2))
                  (if (even? n) (f (/ n 2)) m)
                  (f (+ 2 n))))))
       1 x)))
  )

(def s-106-1
  (fn [x y]
    (let [a #(+ % 2)
          d #(/ % 2)
          m #(* % 2)
          max 10
          path (atom max)]
      ((fn step [i o n]
         (let [f #(step (inc i) %1 %2)]
           (if (or (= n y) (>= i @path))
             (reset! path i)
             (min (if (not= o d) (f m (m n)) max)
                  (if (and (not= o m) (even? n)) (f d (d n)) max)
                  (f a (a n))))))
       1 nil x)))
  )

(def s-106-austintaylor
  (fn [a b]
    (loop [paths [a] i 1]
      (println paths i)
      (if (some #(= % b) paths)
        i
        (recur (mapcat #(concat [(* 2 %) (+ 2 %)]
                                (if (zero? (mod % 2)) [(/ % 2)] [])) paths) (inc i))))))

(def s-106-condotti
  (fn [s e]
    ((fn [xs n]
       (if (some #(= % e) xs) n
           (recur (concat (map #(* % 2) xs)
                          (map #(+ % 2) xs)
                          (keep #(if (even? %) (/ % 2)) xs))
                  (inc n))))
     [s] 1)))

(def s-107
  (fn [n] #(int (Math/pow % n)))
  )

(def s-108
  (fn [& seqs]
    (letfn [(search [[x :as s] y]
              (when s
                (cond (< x y) (search (next s) y)
                      (= x y) true
                      (> x y) false)))]
      (->>
       (apply map
              (fn [& xs]
                (first (filter (fn [x] (every? #(search % x) seqs)) xs)))
              seqs)
       (filter identity)
       first)))
  )

(def s-110
  (fn [s]
    (rest
     (iterate (fn [s]
                (flatten
                 (reduce
                  (fn [[v i m] n]
                    (if (= n m)
                      [v (inc i) m]
                      [(conj v i m) 1 n]))
                  [[] 1 (first s)] (rest s))))
              s)))
  )

(def s-111
  (fn [w b]
    (->> b
         (map #(filter (partial not= \space) %)) ; remove spaces
         ((fn [b] (concat b (apply map (fn [& xs] xs) b)))) ; append verticals
         (mapcat #(partition-by (partial = \#) %)) ; split by #
         (some (fn [x]
                 (and (= (count x) (count w))
                      (every? identity (map #(or (= %1 %2) (= %2 \_)) w x))))) ; compare each to input word
         true?))
  )

(def s-112
  (fn [n s]
    (or
     (let [sum (atom 0)]
       ((fn step [[f :as s]]
          (when (integer? f) (swap! sum + f))
          (when (and s (<= @sum n))
            (cons (if (sequential? f) (step f) f)
                  (step (next s)))))
        s))
     '()))
  )

;; muuuuch better.
(def s-112-daowen
  (fn horrible [x node]
    (if-let [h (first node)]
      (cond
        (coll? h) (if-let [t (horrible x h)] (list t))
        (<= h x) (cons h (horrible (- x h) (rest node)))
        :else '()))))

;; TODO -- iterate down nested seqs with both daowens and (cons (recur) (recur)) pattern

(def s-114
  (fn step [n p [h :as s]]
    (let [n (if (p h) (dec n) n)]
      (when-not (= n 0) (cons h (step n p (next s))))))
  )

(def s-115
  (fn [n]
    (let [l (count (str n))
          [left right] (->> (str n) (map #(Character/digit % 10))
                            (split-at (int (/ l 2))))
          f #(apply + %)]
      (cond
        (= l 1) true
        (odd? l) (= (f left) (f (rest right)))
        (even? l) (= (f left) (f right)))))
  )

;; remember every non-prime number is composed of prime factors
(def s-116
  (fn []))
