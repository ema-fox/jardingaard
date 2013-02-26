(ns jardingaard.util
  (:use [clojure.set :only [union]]
        [clojure.core.reducers :only [cat]]
        jardingaard.reducers))

(def tau (* 2 Math/PI))

(defmacro key-> [coll key & forms]
  `(let [coll# ~coll
         key# ~key]
     (assoc coll# key# (-> (get coll# key#) ~@forms))))

(defmacro key->> [coll key & forms]
  `(key-> ~coll ~key (->> ~@forms)))

(defn swap-args [f]
  (fn [a b]
    (f b a)))

(defn mapmap [f m]
  (map (fn [[key value]]
	 [key (f key value)])
       m))

(defn map-kv [f coll]
  (persistent! (reduce-kv (fn [acc key value]
                            (assoc! acc key (f key value)))
                          (transient (empty coll))
                          coll)))

(defn ttmap [f y xs]
  (let [foo (reductions (fn [[y _] x]
                          (f y x))
                        [y] xs)]
    [(first (last foo))
     (map second (rest foo))]))

(defmacro dounroll [[n xs] & body]
  (cons 'do (for [x xs]
              `(let [~n ~x]
                 ~@body))))

(defn assoc-seq [col & keys-vals]
  (let [keys (butlast keys-vals)
        vals (last keys-vals)]
    (into col (map vector keys vals))))

(defn insert-at [i xs x]
  (concat (take i xs) [x] (drop i xs)))

(defn lowest-scored [xs]
  (->> (sort-by first xs)
       first
       second))

(defn ^:static minus [[pa0 pa1] [pb0 pb1]]
  [(- pa0 pb0) (- pa1 pb1)])

(defn ^{:static true
        :inline (fn [pa pb]
                  `(let [[pa0# pa1#] ~pa
                         [pb0# pb1#] ~pb]
                     [(+ pa0# pb0#) (+ pa1# pb1#)]))}
  plus
  ([[pa0 pa1] [pb0 pb1]]
     [(+ pa0 pb0) (+ pa1 pb1)])
  ([]
     [0 0]))

(defn ^:static mult [[p0 p1] x]
  [(* p0 x) (* p1 x)])

(defn ^:static div [[p0 p1] x]
  [(/ p0 x) (/ p1 x)])

(defn ^:static floor [x]
  (int (Math/floor x)))

(defn ^:static round [[p0 p1]]
  [(floor (+ 0.5 p0))
   (floor (+ 0.5 p1))])

(defn mult2 [[pa0 pa1] [pb0 pb1]]
  [(* pa0 pb0) (* pa1 pb1)])

(defn ^:static direction [[pa0 pa1 :as pa] [pb0 pb1 :as pb]]
  (if (not= (map float pa) (map float pb))
    (let [d0 (- pb0 pa0)
	  d1 (- pb1 pa1)
	  dist (Math/sqrt (+ (* d0 d0) (* d1 d1)))]
      [(/ d0 dist) (/ d1 dist)])
    [0 0]))
    
(defn ^:static ^Float distance [[pa0 pa1] [pb0 pb1]]
  (let [d0 (- (float pb0) (float pa0))
        d1 (- (float pb1) (float pa1))
        dist (Math/sqrt (+ (* d0 d0) (* d1 d1)))]
    dist))

(defn normalize-rect [[pa0 pa1] [pb0 pb1]]
  (let [[pd0 pe0] (sort [pa0 pb0])
	[pd1 pe1] (sort [pa1 pb1])]
    [[pd0 pd1]
     [pe0 pe1]]))

(defn rect-contains? [pa pb [pc0 pc1]]
  (let [[[pd0 pd1]
	 [pe0 pe1]] (normalize-rect pa pb)]
    (and (<= pd0 pc0 pe0)
	 (<= pd1 pc1 pe1))))

(defn avg-point [pa pb x]
  (plus (mult pa x) (mult pb (- 1 x))))

(defn p-on-line [pa pb pc]
  (let [delta (minus pb pa)
	t (/ (apply + (mult2 (minus pc pa) delta))
	     (let [foo (apply + (mult2 delta delta))]
	       (if (== foo 0)
		 1
		 foo)))]
    (avg-point pb pa (min 1 (max 0 t)))))

(defn ^:static arc<-dir [[^float p0 ^float p1]]
  (if (< 0 (Math/asin p0))
    (Math/acos p1)
    (- tau (Math/acos p1))))

(defn ^:static dir<-arc [a]
  [(Math/sin a) (Math/cos a)])

(defn ^:static avec<-dvec [p]
  [(arc<-dir (direction [0 0] p))
   (distance [0 0] p)])

(set! *warn-on-reflection* true)

(defn ^:static dvec<-avec [[a dist]]
  (mult (dir<-arc a) dist))

(defn pairs [xs]
  (map (fn [x y] [x y])
       xs
       (rest xs)))

(defn alter-in [r key f & args]
  (alter r assoc key (apply f (get @r key) args)))

(defn dbg [x & msg]
  (prn msg x)
  x)

(defn loud-agent [& args]
  (apply agent (concat args
                       [:error-handler (fn [a e]
                                         (prn a e))])))

(defn forkIO [f] (.start (Thread. f))) ; cool bilingual joke or just tacky? 

(defn ^Integer prng [& args]
  (let [^Integer foo (reduce (fn [res x]
                               (let [^Integer bar (if (sequential? x)
                                                    (apply prng x)
                                                    x)]
                                 (bit-xor res
                                          bar
                                          (bit-shift-left bar (rem bar 2))
                                          (bit-shift-left bar (rem bar 3))
                                          (bit-shift-right bar 3))))
                             13
                             args)]
    (bit-xor foo (rem foo 7)
             (bit-shift-right foo 1))))

(declare gen-patch)

(let [not-there (Object.)]
  (defn gen-map-patch [a b]
    (persistent! (reduce (fn [res k]
                           (if (= (a k) (b k))
                             res
                             (assoc! res k (gen-patch (a k) (get b k not-there)))))
                         (transient {})
                         (cat (keys a) (keys b)))))

  (defn gen-vec-patch [a b]
    (persistent! (reduce (fn [res k]
                           (if (= (a k) (b k))
                             res
                             (assoc! res k (gen-patch (a k) (get b k not-there)))))
                         (transient {})
                         (rrange 0 (count a)))))

  (defn gen-patch [a b]
    (cond (and (map? a)
               (map? b))
          (gen-map-patch a b)
          (and (vector? a)
               (vector? b)
               (= (count a) (count b)))
          (gen-vec-patch a b)
          (= b not-there)
          nil
          true
          [b])))

(defn apply-patch [a p]
  (cond (vector? p)
        (first p)
        (map? p)
        (reduce (fn [b [k v]]
                  (if (= v nil)
                    (dissoc b k)
                    (assoc b k (apply-patch (a k) v))))
                a
                p)))

(declare pr-summary)

(defn pr-summary-seq [[y & ys] n]
  (if y
    (if (< 0 n)
      (recur ys (+ (/ n 2) (let [m (pr-summary y (/ n 2))]
                             (if (first ys)
                               (do (print " ")
                                   (dec m))
                               m))))
      (let [sn (str (inc (count ys)))]
        (print "..." sn "more")
          (- n 9 (count sn))))
    n))

(defn pr-summary-coll [x n start end]
  (print start)
  (let [n (pr-summary-seq x n)]
    (print end)
    (- n (count start) (count end))))

(defn pr-summary
  ([x]
     (pr-summary x 5000)
     (prn))
  ([x n]
     (cond (vector? x)
           (pr-summary-coll x n "[" "]")
           (map? x)
           (pr-summary-coll (apply concat x) n "{" "}")
           (set? x)
           (pr-summary-coll (seq x) n "#{" "}")
           (seq? x)
           (pr-summary-coll x n "(" ")")
           true
           (let [out (str x)]
             (print out)
             (- n (count out))))))