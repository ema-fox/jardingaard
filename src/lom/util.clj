(ns lom.util)

(def tau (* 2 Math/PI))

(defn mapmap [f m]
  (map (fn [[key value]]
	 [key (f key value)])
       m))

(defn ttmap [f y xs]
  (let [foo (reductions (fn [[y _] x]
                          (f y x))
                        [y] xs)]
    [(first (last foo))
     (map second foo)]))

(defn insert-at [i xs x]
  (concat (take i xs) [x] (drop i xs)))

(defn lowest-scored [xs]
  (->> (sort-by first xs)
       first
       second))

(defn ^:static minus [[pa0 pa1] [pb0 pb1]]
  [(- pa0 pb0) (- pa1 pb1)])

(defn ^:static plus
  ([[pa0 pa1] [pb0 pb1]]
     [(+ pa0 pb0) (+ pa1 pb1)])
  ([]
     [0 0]))

(defn ^:static mult [[p0 p1] x]
  [(* p0 x) (* p1 x)])

(defn ^:static div [[p0 p1] x]
  [(/ p0 x) (/ p1 x)])

(defn round [[p0 p1]]
  [(int (+ 0.5 p0))
   (int (+ 0.5 p1))])

(defn mult2 [[pa0 pa1] [pb0 pb1]]
  [(* pa0 pb0) (* pa1 pb1)])

(defn ^:static direction [[pa0 pa1 :as pa] [pb0 pb1 :as pb]]
  (if (not= (map float pa) (map float pb))
    (let [d0 (- pb0 pa0)
	  d1 (- pb1 pa1)
	  dist (Math/sqrt (+ (* d0 d0) (* d1 d1)))]
      [(/ d0 dist) (/ d1 dist)])
    [0 0]))
    

(defn ^:static distance [[pa0 pa1] [pb0 pb1]]
  (let [d0 (- pb0 pa0)
        d1 (- pb1 pa1)
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

