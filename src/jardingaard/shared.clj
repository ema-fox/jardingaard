(ns jardingaard.shared
  (:use [jardingaard util]))

(def num-zombies 10)
(def num-bunnies 100)

(def step-fns [])

(defn check-world [state]
  (:world state))

(defn check-players-map [state]
  (map? (:players state)))

(defn check-players-p [state]
  (every? #(get-in % [1 :p])
          (:players state)))

(defmacro defstep [args & body]
  `(def step-fns (conj step-fns (fn [{:keys ~args :as ~'state}]
                                  {:post [(check-world ~'%)
                                          (check-players-map ~'%)
                                          (check-players-p ~' %)]}
                                  ~@body))))

(def ^:dynamic *seed*)

(def human-walk-speeds {:dirt 1
                  :door 1
                  :grass 1.1
                  :tall-grass 1.3})

(def walkable? (set (keys human-walk-speeds)))

(def zombie-walk-speeds (into {} (map (fn [[k v]]
                                        [k (if (= k :door)
                                             (* 16 v)
                                             (* 4 v))])
                                      human-walk-speeds)))

(defn pick [xs]
  (let [foo (apply + (map second xs))
        bar (rand foo)]
    (loop [yar bar
           ys xs]
      (if (< yar (second (first ys)))
        (ffirst ys)
        (recur (- yar (second (first ys)))
               (rest ys))))))

(defn gen-chunk [[p0 p1 :as p] [s0 s1 :as s] bla]
  (if (= 0 (apply * (map dec s)))
    (into {} (for [b0 (range s0)
                     b1 (range s1)]
               [(plus p [b0 b1]) (pick bla)]))
    (let [nbla (map (fn [[x y]]
                      [x (* y (+ (rand)))])
                    bla)
          [d0 d1 :as d] (map #(int (/ % 2)) s)]
      (merge (gen-chunk p d nbla)
             (gen-chunk [(+ p0 d0) p1] [(- s0 d0) d1] nbla)
             (gen-chunk [p0 (+ p1 d1)] [s0 (- s1 d1)] nbla)
             (gen-chunk (plus p d) (minus s d) nbla)))))

(defn gen-world [world-size]
  (let [bla (gen-chunk [0 0] [world-size world-size]
                       [[:wall 6]
                        [:grass 20]
                        [:tall-grass 400]
                        [:dirt 10]
                        [:tree 1]
                        [:shrub 2]])]
    (mapv (fn [p0]
            (mapv (fn [p1]
                    (bla [p0 p1]))
                  (range world-size)))
          (range world-size))))

(defn new-world [world-size bullet-speed]
  {:players {}
   :bullets []
   :c-sites []
   :zombies []
   :bunnies []
   :deadbunnies []
   :world (gen-world world-size)
   :bullet-speed bullet-speed
   :spawn-point [(/ world-size 2) (/ world-size 2)]})

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

(defstep [bullets world bullet-speed]
  (assoc state
    :bullets (->> bullets
                  (map (fn [{:keys [p ttl m] :as b}]
                         (let [newp (plus p (mult m bullet-speed))]
                           (assoc b
                             :p newp
                             :ttl (if (some #((if (> ttl 497)
                                                #{:wall :door :tree}
                                                #{:wall :door :tree :windowed-wall})
                                              (get-in world %))
                                            (line-affects p newp))
                                    0
                                    (dec ttl))))))
                  (filter #(< 0 (:ttl %))))))

(defn new-pos [{:keys [p path] :as entity} walk-speeds world]
  (let [npath (if (and (first path)
                       (< (distance p (first path)) 0.1))
                (rest path)
                path)]
    (assoc (if (first npath)
             (assoc entity
               :p (plus p (mult (direction p (first npath))
                                (min (/ 0.25
                                        (or (walk-speeds (get-in world
                                                                 (round p)))
                                            1))
                                     (distance p (first npath))))))
             entity)
      :path npath)))

(defn manhatten [[a0 a1] [b0 b1]]
  (+ (Math/abs (int (- a0 b0)))
     (Math/abs (int (- a1 b1)))))

(defn ngbrs [p w]
  (filter #(get-in w %) (map #(plus p %)
                             [[0 1]
                              [0 -1]
                              [1 0]
                              [-1 0]])))

(defn far-ngbrs [[p0 p1 :as p] d w]
  (filter #(and (not= p%)
                (get-in w %))
          (for [a0 (range (- p0 d) (+ 1 p0 d))
                a1 (range (- p1 d) (+ 1 p1 d))]
            [a0 a1])))

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

(defn get-in-map [w [^Integer p0 ^Integer p1]]
  (get (get w p0) p1))

(def fdstore (into {} (mapmap (fn [_ xs]
                                (eval `(fn [[^Integer ~'p0 ^Integer ~'p1] ~'w]
                                         (and ~@(for [[x0 x1] xs]
                                                  `(walkable? (get (get ~'w (+ ~x0 ~'p0)) (+ ~x1 ~'p1))))))))
                              tdstore)))

(defn test-direction [p d w]
  ((fdstore d) p w))
;  (every? #(walkable? (get-in-map w (plus p %))) (tdstore d)))

(defn route [start goal walk-speeds w]
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
        (if-let [other-side (other-open endp)]
          (if swapped
            (concat (reverse (:ps other-side)) (rest ps))
            (concat (reverse ps) (rest (:ps other-side))))
          (recur other-open
                 (persistent! (loop [m (dissoc! (transient open) endp)
                                     [p & newps] (->> directions
                                                      (keep (fn c [%]
                                                              (let [p (plus endp %)]
                                                                (if (and (not (closed p))
                                                                         (< (manhatten p goal) 30)
                                                                         (test-direction endp % w))
                                                                  p)))))]
                                (if p
                                  (let [d (distance endp p)
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

(defstep [zombies players spawn-point world]
  (assoc state
    :zombies (mapv (fn [{:keys [p path seed cooldown] :as zombie}]
                     (assoc (if (and (seq path)
                                     (< 0.1 (distance (last path) p)))
                              (new-pos zombie zombie-walk-speeds world)
                              (if-let [goal (and (= 0 (mod (prng *seed* seed (round p)) 9))
                                                 (some (fn [[_ {pp :p}]]
                                                         (if (< (distance p pp) 20)
                                                           pp))
                                                       players))]
                                (assoc zombie
                                  :path (route (round p) (round goal) zombie-walk-speeds world))
                                zombie))
                       :cooldown (max 0 (dec cooldown))))
                   (if (< (count zombies) num-zombies)
                     (conj zombies {:p spawn-point
                                    :cooldown 0
                                    :seed *seed*})
                     zombies))))

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

(defstep [zombies players spawn-point]
  (let [[zmbs pls] (ttmap (fn [zombies [pid {:keys [p] :as player}]]
                            (let [newzombies (map (fn [{zp :p :keys [cooldown] :as zombie}]
                                                    (if (and (= 0 cooldown)
                                                             (< (distance p zp) 1))
                                                      (assoc zombie
                                                        :cooldown 10)
                                                      zombie))
                                                  zombies)
                                  damage (count (filter not (map = zombies newzombies)))]
                              [newzombies
                               [pid (harm-player player damage spawn-point)]]))
                          zombies
                          players)]
    (assoc state
      :zombies zmbs
      :players (into {} pls))))

(defstep [bunnies spawn-point world]
  (let [nbs (mapv (fn [{:keys [p path seed] :as bunny}]
                    (cond (and (first path)
                               (< 0.1 (distance (last path) p)))
                          (new-pos bunny human-walk-speeds world)
                          (= 0 (mod (prng *seed* seed (round p)) 99))
                          (let [foo0 (mod (prng *seed* seed 4321 (round p)) 11)
                                foo1 (mod (prng *seed* seed 1234 (round p)) 11)
                                goal (round (plus p (minus [foo0 foo1] [5 5])))]
                            (assoc bunny
                              :path (if (= :tall-grass (get-in world goal))
                                      (route (round p) goal human-walk-speeds world))))
                          true
                          bunny))
                  (if (and (< (count bunnies) num-bunnies)
                           (= 0 (mod (prng *seed* 565) 9)))
                    (conj bunnies {:p spawn-point
                                   :seed *seed*})
                    bunnies))]
    (let [[nworld nbs2]
          (ttmap (fn [world {:keys [p path] :as bunny}]
                   (if (and false
                            (first path)
                            (< (distance (last path) p) 0.1)
                            (= :tall-grass (get-in world (round p))))
                     [(assoc-in world (round p) :grass)
                      bunny]
                     [world bunny]))
                 world
                 nbs)]
      (assoc state
        :world nworld
        :bunnies nbs2))))

(defstep [bullets bullet-speed zombies]
  (let [[bus zmbs] (step-bullets&entities
                    (fn [zombie damage]
                      (if (< 0 damage)
                        (assoc zombie
                          :dead true) ;<-- lol
                        zombie))
                    :p
                    bullet-speed
                    bullets
                    zombies)]
    (assoc state
      :bullets bus
      :zombies (vec (filter #(not (:dead %)) zmbs)))))

(defstep [bullets deadbunnies bunnies bullet-speed]
  (let [[bus bns] (step-bullets&entities
                   (fn [bunny damage]
                     (if (< 0 damage)
                       (assoc bunny
                         :dead true)
                       bunny))
                   :p
                   bullet-speed
                   bullets
                   bunnies)]
    (let [{ls nil ds true} (group-by :dead
                                     bns)]
      (assoc state
        :bullets bus
        :deadbunnies (concat deadbunnies ds)
        :bunnies ls))))

(defstep [players world]
  (assoc state
    :players (into {} (map (fn [[pid player]]
                             [pid (new-pos player human-walk-speeds world)])
                           players))))

(defstep [bullets players bullet-speed spawn-point world]
  (let [[bus pls] (step-bullets&entities
                   (fn [[pid {:keys [hp died] :as player}] damage]
                     [pid (harm-player player damage spawn-point)])
                   #(get-in % [1 :p])
                   bullet-speed
                   bullets
                   players)]
    (assoc state
      :bullets (vec bus)
      :players (into {} pls))))

(defstep [world players]
   (assoc state
    :world (reduce (fn [w pl]
                     (let [tilep (round (:p pl))
                           succ ({:tall-grass :grass
                                  :grass :dirt}
                                 (get-in w tilep))]
                       (if (and succ
                                (= 0 (mod (apply bit-xor *seed* tilep)
                                          9)))
                         (assoc-in w tilep succ)
                         w)))
                   world
                   (vals players))))

(defn walk [{:keys [world players] :as state} pid p]
  (let [goal (->> (line-affects (get-in players [pid :p]) p)
                  (sort-by #(distance % p))
                  (drop-while #(not (walkable? (get-in world %))))
                  first)]
    (if goal
      (let [pp (get-in players [pid :p])
            path (route (round pp)
                        (round goal)
                        human-walk-speeds
                        world)]
        (assoc-in state [:players pid :path] path))
      state)))

(defn steal-player [{:keys [inventar inventar-p] :as player} x n]
  (let [new-inventar (vec (keep (fn [y]
                                  (if (not= x (first y))
                                    y
                                    (if (>= n (second y))
                                      nil
                                      [x (- (second y) n)])))
                                inventar))]
    (assoc player
      :inventar new-inventar
      :inventar-p (min inventar-p (dec (count new-inventar))))))

(defn shot [state pid goalp]
  (let [{:keys [p inventar inventar-p]} (get-in state [:players pid])
        selected (first (nth inventar inventar-p))
        tilep (round goalp)]
    (cond (= :gun selected)
          (update-in state [:bullets] conj (let [m (direction p goalp)]
                                             {:p (plus p (mult m 0.75))
                                              :ttl 500
                                              :m m}))
          (and (= :pickaxe selected)
               (#{:wall :shrub :windowed-wall :door :tree} (get-in (:world state) tilep))
               (not (some #(= tilep (:p %)) (:c-sites state))))
          (assoc-in (walk state pid tilep) [:players pid :do-at] tilep)
          (and (#{:wall :windowed-wall :door} selected)
               (#{:grass :dirt :tall-grass} (get-in (:world state) tilep))
               (not (some #(= tilep (:p %)) (:c-sites state))))
          (assoc-in (walk state pid tilep) [:players pid :do-at] tilep)
          true
          state)))

(defn give-player [{:keys [inventar] :as player} x n]
  (assoc player
    :inventar (if (some #(= (first %) x) inventar)
                (mapv #(if (= (first %) x)
                         [x (+ (second %) n)]
                         %)
                      inventar)
                (conj inventar [x n]))))

(defstep [players deadbunnies]
  (let [[dbs pls]
        (ttmap (fn [dbs [pid {:keys [p] :as player}]]
                 (let [newdbs (filter #(< 1 (distance (:p %) p))
                                      dbs)
                       foo (- (count dbs) (count newdbs))]
                   [newdbs
                    [pid
                     (if (= 0 foo)
                       player
                       (give-player player :bunny foo))]]))
               deadbunnies
               players)]
    (assoc state
      :deadbunnies (vec dbs)
      :players (into {} pls))))

(defstep [players c-sites world]
  (let [[new-c-sites new-players]
        (ttmap (fn [cs [pid {:keys [p do-at inventar inventar-p] :as player}]]
                 (let [selected (first (nth inventar inventar-p))]
                   (if (and do-at
                            (< (distance p do-at) 5)
                            (not (some #(= do-at (:p %)) cs)))
                     (cond (and (= :pickaxe selected)
                                (#{:wall :shrub :windowed-wall :door :tree} (get-in world do-at)))
                           [(conj cs {:p do-at :t 0 :owner pid})
                            [pid (assoc player
                                   :do-at nil
                                   :path nil)]]
                           (and (#{:wall :windowed-wall :door} selected)
                                (#{:grass :dirt :tall-grass} (get-in (:world state) do-at)))
                           [(conj cs {:p do-at :t 0 :x selected})
                            [pid (assoc (steal-player player selected 1)
                                   :do-at nil
                                   :path nil)]]
                           true
                           [cs [pid player]])
                     [cs [pid player]])))
               c-sites
               players)]
    (assoc state
      :players (into {} new-players)
      :c-sites new-c-sites)))

(defn decip [{:keys [inventar inventar-p] :as player}]
  (assoc player
    :inventar-p (if (= 0 inventar-p)
                  (dec (count inventar))
                  (dec inventar-p))))

(defn incip [{:keys [inventar inventar-p] :as player}]
  (assoc player
    :inventar-p (if (= (dec (count inventar)) inventar-p)
                  0
                  (inc inventar-p))))

(defn player-has? [{:keys [inventar]} x n]
  (some #(and (= (first %) x) (<= n (second %)))
        inventar))

(defn enhance [{:keys [inventar inventar-p] :as player}]
  (let [x (nth inventar inventar-p)]
    (condp = (first x)
      :wall (if (player-has? player :twig 5)
              (-> player
                  (give-player :windowed-wall 1)
                  (steal-player :wall 1)
                  (steal-player :twig 5))
              player)
      :twig (if (player-has? player :twig 13)
              (-> player
                  (give-player :door 1)
                  (steal-player :twig 13))
              player)
      player)))

(defn step-tile [p w]
  (let [tile (get-in w p)
        ns (frequencies (map #(get-in w %)
                             (ngbrs p w)))]
    (condp = tile
      :dirt (cond (:tree ns)
                  [nil tile]
                  (or (:grass ns) (:tall-grass ns) (:shrub ns))
                  [(rand-int 99999)
                   :grass]
                  true
                  [nil
                   tile])
      :grass (cond (:tree ns)
                   [(rand-int 999) :dirt]
                   true
                   [(rand-int 99999) :tall-grass])
      :tall-grass (cond (:tree ns)
                        [(rand-int 999) :dirt]
                        (:shrub ns)
                        [(rand-int 99999)
                         :shrub]
                        true
                        [nil
                         tile])
      :shrub (cond (:tree ns)
                   [(rand-int 999) :dirt]
                   (and (:shrub ns)
                        (< 3 (:shrub ns))
                        (not= 0 (mod (prng p) 3)))
                   [(rand-int 99)
                    :dirt]
                   true
                   [nil
                    tile])
      :tree (cond (:tree ns)
                  [(rand-int 999) :dirt]
                  true
                  [nil tile])
      [nil
       tile])))

(defn exec-message [state msg]
  {:pre [(:world state)]
   :post [(:world %)]}
  (condp = (first msg)
    :plcmd (let [pid (second msg)
                 cmd (nth msg 2)]
             (condp = (first cmd)
               :walk (walk state pid (second cmd))
               :shot (shot state pid (second cmd))
               :decip (update-in state [:players pid] decip)
               :incip (update-in state [:players pid] incip)
               :enhance (update-in state [:players pid] enhance)))
    :tile (assoc-in state (cons :world (second msg))
                    (second (step-tile (second msg) (:world state))))
    :sapling (let [[_ sapp treep] msg
                   world (:world state)]
               (if (and (#{:dirt :grass :tall-grass :shrub}
                         (get-in world sapp))
                        (not (:tree (frequencies (map #(get-in world %)
                                                      (far-ngbrs sapp 2 world)))))
                        (= :tree (get-in world treep)))
                 (assoc-in state (concat [:world] (second msg)) :tree)
                 state))
    :new-player (assoc-in state [:players (second msg)]
                          (assoc (nth msg 2) :p (:spawn-point state)))))

(defn exec-messages [state msgs]
  {:pre [(:world state)]
   :pose [(:world state)]}
  (reduce exec-message state msgs))

(defstep [world c-sites players]
  (assoc state
    :c-sites (keep (fn [cs]
                     (if (< (:t cs) 100)
                       (assoc cs :t (inc (:t cs)))))
                   c-sites)
    :world (reduce (fn [w {:keys [p t x]}]
                     (if (>= t 100)
                       (assoc-in w p (or x :dirt))
                       w))
                   world
                   c-sites)
    :players (reduce (fn [pls {:keys [p t x owner]}]
                       (if (and (>= t 100) (not x))
                         (let [tile (get-in world p)]
                           (assoc pls
                             owner (cond (#{:wall :windowed-wall :door} tile)
                                         (give-player (pls owner) tile 1)
                                         (= tile :shrub)
                                         (give-player (pls owner) :twig 4)
                                         (= tile :tree)
                                         (-> (pls owner)
                                             (give-player :twig 23)
                                             (give-player :trunk 1))
                                         true
                                         (pls owner))))
                         pls))
                     players
                     c-sites)))

(defn step [state msgs]
  {:pre [(:world state)]
   :post [(:world %)]}
  ;(prn dbg1)
  (reduce (fn [st f]
            (f st))
          (exec-messages state msgs)
          step-fns))
