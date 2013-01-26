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
      (let [p0 (int p0)
            p1 (int p1)
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
                                                            32)))))
                                      val2
                                      (rrange (if (= hi0 ha0)
                                                (bit-and p0 low-mask)
                                                0)
                                              (if (= hi0 hb0)
                                                (inc (bit-and (+ p0 s0) low-mask))
                                                32)))))
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

(defn new-pos [{:keys [p path] :as entity} walk-speeds bworld]
  (let [npath (if (and (first path)
                       (< (distance p (first path)) 0.1))
                (rest path)
                path)]
    (assoc (if (first npath)
             (assoc entity
               :p (plus p (mult (direction p (first npath))
                                (min (/ 0.25
                                        (or (walk-speeds (get-in-map bworld
                                                                     (round p)))
                                            1))
                                     (distance p (first npath))))))
             entity)
      :path npath)))

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

(def directions (apply concat (take 4 (iterate #(map (fn [[p0 p1]]
                                                       [p1 (* -1 p0)])
                                                     %)
                                               [[1 0]
                                                [1 1]
                                                [2 1]
                                                [1 2]
                                                [3 1]
                                                [1 3]
                                                [3 2]
                                                [2 3]]))))

(def tdstore (into {} (map (fn [d]
                             [d (disj (set (line-affects [0 0] d))
                                      [0 0])])
                           directions)))

(def fdstore (into {} (mapmap (fn [_ xs]
                                (eval `(fn [[^Integer ~'p0 ^Integer ~'p1] ~'w]
                                         (and ~@(for [[x0 x1] xs]
                                                  `(walkable? (get-in-map ~'w [(+ ~x0 ~'p0)
                                                                               (+ ~x1 ~'p1)])))))))
                              tdstore)))

(defn test-direction [p d w]
  ((fdstore d) p w))

(defn route [start goal walk-speeds mw]
  {:pre [(every? integer? start)
         (every? integer? goal)]}
  (loop [open {start (path-stub. ()
                                 0
                                 (manhatten start goal))}
         other-open {goal (path-stub. (list goal)
                                      0
                                      (manhatten start goal))}
         swapped false
         closed #{}]
    (if (first open)
      (let [[endp {:keys [ps d h]}] (reduce (fn a [[_ {da :d ha :h} :as a] [_ {db :d hb :h} :as b]]
                                              (if (< (+ da ha) (+ db hb))
                                                a
                                                b))
                                            open)]
        (if (other-open endp)
          (let [cp (reduce (fn [a b]
                             (if (< (+ (:d (open a)) (:d (other-open a)))
                                    (+ (:d (open b)) (:d (other-open b))))
                               a
                               b))
                           (intersection (set (keys open)) (set (keys other-open))))
                other-side (other-open cp)]
            (if swapped
              (concat (reverse (:ps other-side)) (rest ps))
              (concat (reverse ps) (rest (:ps other-side)))))
          (recur other-open
                 (persistent! (loop [m (dissoc! (transient open) endp)
                                     [p & newps] (->> directions
                                                      (keep (fn c [%]
                                                              (let [p (plus endp %)]
                                                                (if (and (not (closed p))
                                                                         (< (manhatten p goal) 30)
                                                                         (test-direction endp % mw))
                                                                  p)))))]
                                (if p
                                  (let [d (+ (distance endp p) d)
                                        h (distance p (if swapped
                                                        start
                                                        goal))
                                        a (m p)]
                                    (recur (if (and a
                                                    (< (+ (:d a) (:h a))
                                                       (+ d h)))
                                             m
                                             (assoc! m p (path-stub. (conj ps p)
                                                                     d
                                                                     h)))
                                           newps))
                                  m)))
                 (not swapped)
                 (conj closed endp))))
      nil)))

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
        :hp 20
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
