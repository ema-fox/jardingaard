(ns jardingaard.helpers
  (:use [jardingaard rules util reducers]
        [clojure.set :only [intersection]]
        [clojure.core.protocols]))

(def step-fns (ref []))
(def gen-fns (ref []))

(defn check-world [state]
  (:world state))

(defn check-players-map [state]
  (map? (:players state)))

(defn check-players-p [state]
  (every? :p (vals (:players state)))
  (every? :p (vals (:lumberjacks state))))

(defmacro defstep [args & body]
  `(dosync (alter step-fns conj (fn [{:keys ~args :as ~'state}]
                                  {:post [(check-world ~'%)
                                          (check-players-map ~'%)
                                          (check-players-p ~' %)]}
                                  ~@body))))

(def ^:dynamic *tick*)

(defn spawn-progress [{:keys [spawn type working broken]}]
  (if (or working broken)
    0
    (min 1 (/ (- *tick* spawn) (work-times type)))))

(defn broken-progress [{:keys [type broken]}]
  (/ broken (*broken* type)))

(defn ready?
  ([spawn ticks]
   (>= *tick* (+ spawn ticks)))
  ([object]
   (= (spawn-progress object) 1)))

(defn health-fraction [{:keys [type hp]}]
  (/ hp (+hp+ type)))

(defn walkable? [tt]
  (or (not tt) (= :door tt)))

(defn map-part [m p s]
  (filter (fn [tile]
            (rect-contains? (minus p s) (plus p s) (:p tile)))
          (vals m)))

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

(defn make-p->ws [walk-speeds world]
  (fn [p]
    (or (walk-speeds (:ground (world p)))
        2)))

(defn new-pos [{:keys [p path type] :as entity} p->ws]
  (if (first path)
    (loop [p p
           [next-pos & rest-path :as path] path
           energy (+step-size+ type)]
      (if (and next-pos (> energy 0))
        (let [ws (p->ws (round2 p))
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
  (filter w (unchecked-ngbrs p)))

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

(defn route2 [start goal p->ws]
  (if (= (round2 start) (round2 goal))
    [goal]
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

#_(defn rand-spawnpoint [{:keys [bworld mworld spawn-point]}]
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

(defn get-map-part [world p s]
  (map-part world (minus p s) (mult s 2)))
