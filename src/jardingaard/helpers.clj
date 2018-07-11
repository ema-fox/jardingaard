(ns jardingaard.helpers
  (:use [jardingaard util reducers]
        [clojure.set :only [intersection]]
        [clojure.core.protocols]))

(def step-fns (ref []))
(def gen-fns (ref []))

(defn check-world [state]
  (:bworld state))

(defn check-players-map [state]
  (map? (:players state)))

(defn check-players-p [state]
  (every? #(get-in % [1 :p])
          (:players state)))

(defmacro defstep [args & body]
  `(dosync (alter step-fns conj (fn [{:keys ~args :as ~'state}]
                                  {:post [(check-world ~'%)
                                          (check-players-map ~'%)
                                          (check-players-p ~' %)]}
                                  ~@body))))

(def ^:dynamic *seed*)

(defn walkable? [tt]
  (or (not tt) (= :door tt)))

(def low-mask 31)

(def high-mask (bit-not low-mask))

(defn assoc-in-map [m [p0 p1] v]
  (let [p0a (bit-and p0 high-mask)
        p0b (bit-and p0 low-mask)
        p1a (bit-and p1 high-mask)
        p1b (bit-and p1 low-mask)]
    (if-let [mchunk (m [p0a p1a])]
      (let [line (mchunk p0b)]
        (assoc m [p0a p1a] (assoc mchunk p0b (assoc line p1b v))))
      m)))

(defn get-in-map [m [p0 p1]]
  (let [p0 (int p0)
        p1 (int p1)
        high-mask (int high-mask)
        low-mask (int low-mask)
        p0a (bit-and p0 high-mask)
        p0b (bit-and p0 low-mask)
        p1a (bit-and p1 high-mask)
        p1b (bit-and p1 low-mask)]
    (nth (nth (get m [p0a p1a]) p0b) p1b)))

(defn map-part [m [p0 p1] [s0 s1]]
  (reify
    CollReduce
    (coll-reduce [_ f val]
      (let [p0 (floor2 p0)
            p1 (floor2 p1)
            s0 (int s0)
            s1 (int s1)
            high-mask (int high-mask)
            low-mask (int low-mask)
            ha0 (bit-and p0 high-mask)
            hb0 (bit-and (+ p0 s0) high-mask)
            ha1 (bit-and p1 high-mask)
            hb1 (bit-and (+ p1 s1) high-mask)]
        (reduce (fn [val1 hi0]
                  (reduce (fn [val2 hi1]
                            (let [tss (get m [hi0 hi1])]
                              (reduce (fn [val3 li0]
                                        (let [ts (nth tss li0)]
                                          (reduce (fn [val4 li1]
                                                    (f val4 [(+ hi0 li0) (+ hi1 li1) (nth ts li1)]))
                                                  val3
                                                  (rrange (if (= hi1 ha1)
                                                            (bit-and p1 low-mask)
                                                            0)
                                                          (if (= hi1 hb1)
                                                            (inc (bit-and (+ p1 s1) low-mask))
                                                            32)
                                                          2))))
                                      val2
                                      (rrange (if (= hi0 ha0)
                                                (bit-and p0 low-mask)
                                                0)
                                              (if (= hi0 hb0)
                                                (inc (bit-and (+ p0 s0) low-mask))
                                                32)
                                              2))))
                          val1
                          (range ha1 (+ hb1 32) 32)))
                val
                (rrange ha0 (+ hb0 32) 32))))))

(defn line-affects [start goal]
  (let [goal (if (== (first goal) (first start))
               (plus goal [0.0001 0])
               goal)
        [d0 d1] (minus goal start)
        slope (/ d1 d0)
        [a b] (sort [(first start) (first goal)])
        xline (concat [a]
                      (range (+ 0.5 (floor (+ 0.5 a)))
                             (+ 0.5 (floor (+ 0.5 b))))
                      [b])]
    (mapcat (fn [a b]
              (let [x (floor (/ (+ a b 1) 2))
                    ya (floor (+ (second start) (* slope (- a (first start))) 0.5))
                    yb (floor (+ (second start) (* slope (- b (first start))) 0.5))]
                (map (fn [y]
                       [x y])
                     (range (min ya yb) (inc (max ya yb))))))
            xline
            (rest xline))))

(defn p->walk-speed [p walk-speeds bworld]
  (or (walk-speeds (get-in-map bworld p))
      1))

(defn new-pos [{:keys [p path] :as entity} walk-speeds bworld]
  (if (first path)
    (loop [p p
           [next-pos & rest-path :as path] path
           energy 0.01]
      (if (and next-pos (> energy 0))
        (let [ws (p->walk-speed (round2 p) walk-speeds bworld)
              needed-energy (* (distance p next-pos) ws)]
          (if (> needed-energy energy)
            (recur (plus p (mult (direction p next-pos)
                                 (/ energy ws)))
                   path
                   0)
            (recur next-pos
                   rest-path
                   (- energy needed-energy))))
        (assoc entity
          :p p
          :path path)))
    entity))

(defn walking? [{:keys [path]}]
  (seq path))

(defn manhatten [[a0 a1] [b0 b1]]
  (+ (Math/abs (int (- a0 b0)))
     (Math/abs (int (- a1 b1)))))

(defn unchecked-ngbrs [p]
  [(plus p [0 1])
   (plus p [0 -1])
   (plus p [1 0])
   (plus p [-1 0])])

(defn ngbrs [p w]
  (filter #(get-in-map w %) (unchecked-ngbrs p)))

(defrecord path-stub [ps d h])

(defn fooneighbors [p]
  (if (odd? (second p))
    (map reverse (fooneighbors (reverse p)))
    (map #(plus % p) [[2 0]
                      [1 -1]
                      [-1 -1]
                      [-2 0]
                      [-1 1]
                      [1 1]])))

(defn barneighbors [p]
  (map #(plus (round2 p) %) [[-1 0]
                             [0 1]
                             [1 0]
                             [0 -1]]))

(defn evaluate2 [[_ a b]] (+ a b))

(defn evaluate [[_ [_ a b]]] (+ a b))

(defn candidates [p [ps d h] goal ns p->ws]
  (into {} (for [n ns]
             [n [(cons n ps)
                 (+ d (* (p->ws (round2 (half-point p n)))
                         (distance p n)))
                 (distance n goal)]])))

(defn route2 [start goal walk-speeds bworld]
  (let [p->ws (fn [p]
                (p->walk-speed p walk-speeds bworld))]
    (loop [open (candidates start [() 0 (distance start goal)] goal (barneighbors start) p->ws)
           closed #{}]
      (let [[closest info] (first (sort-by evaluate open))]
        (reverse (first info))
        (if ((set (barneighbors (round2 goal))) closest)
          (reverse (conj (first info) goal))
          (recur (merge-with (fn [a b]
                               (if (< (evaluate2 a) (evaluate2 b))
                                 a
                                 b))
                             (dissoc open closest)
                             (candidates closest info goal (filter (comp not closed)
                                                                   (fooneighbors closest))
                                         p->ws))
                 (conj closed closest)))))))

(defn rand-spawnpoint [{:keys [bworld mworld spawn-point]}]
  (first (concat (filter #(and (not (get-in-map mworld %))
                               (= :tall-grass (get-in-map bworld %)))
                         (for [_ (range 50)]
                           (map rand-int spawn-point)))
                 [spawn-point])))

(defn step-bullets&entities [f pf bullet-speed bus entities]
  (ttmap (fn [bullets entity]
           (let [p (pf entity)
                 newbullets 
                 (filter (fn [{bp :p m :m}]
                           (< 0.5 (distance p (p-on-line bp
                                                         (plus bp (mult m bullet-speed))
                                                         p))))
                         bullets)]
             [newbullets
              (f entity (- (count bullets) (count newbullets)))]))
         bus
         entities))

(defn harm-player [{:keys [hp died] :as player} damage spawn-point]
  (let [newhp (- hp damage)]
    (if (< newhp 1)
      (assoc player
        :hp 200
        :p (plus spawn-point [(rand-int 5) (rand-int 5)])
        :path nil
        :died (inc died))
      (assoc player
        :hp newhp))))

(defn process-bullets [{:keys [bullets bullet-speed] :as state} key]
  (step-bullets&entities (fn [entity damage]
                           (if (< 0 damage)
                             (assoc entity
                               :dead true)
                             entity))
                         :p
                         bullet-speed
                         bullets
                         (state key)))
