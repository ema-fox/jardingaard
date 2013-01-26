(ns jardingaard.shared
  (:require [jardingaard zombie])
  (:use [jardingaard util reducers rules helpers]))

(def tick-duration 33)

(defstep [bullets mworld bullet-speed]
  (assoc state
    :bullets (->> bullets
                  (map (fn [{:keys [p ttl m] :as b}]
                         (let [newp (plus p (mult m bullet-speed))]
                           (assoc b
                             :p newp
                             :ttl (if (some #((if (> ttl 495)
                                                #{:wall :door :tree}
                                                #{:wall :door :tree :windowed-wall})
                                              (get-in-map mworld %))
                                            (line-affects p newp))
                                    0
                                    (dec ttl))))))
                  (filter #(< 0 (:ttl %))))))

(defn far-ngbrs [[p0 p1 :as p] d w]
  (filter #(and (not= p%)
                (get-in-map w %))
          (for [a0 (range (- p0 d) (+ 1 p0 d))
                a1 (range (- p1 d) (+ 1 p1 d))]
            [a0 a1])))

(defstep [bunnies bworld mworld]
  (let [nbs (map (fn [{:keys [p path seed energy] :as bunny}]
                   (assoc (cond (< energy 1)
                                (assoc bunny :dead true)
                                (and (first path)
                                     (< 0.1 (distance (last path) p)))
                                (new-pos bunny human-walk-speeds bworld)
                                (= 0 (mod (prng *seed* seed (round p)) 9))
                                (let [foo0 (mod (prng *seed* seed 4321 (round p)) 11)
                                      foo1 (mod (prng *seed* seed 1234 (round p)) 11)
                                      goal (round (plus p (minus [foo0 foo1] [5 5])))]
                                  (assoc bunny
                                    :path (if (= :tall-grass (get-in-map bworld goal))
                                            (route (round p) goal human-walk-speeds mworld))))
                                true
                                bunny)
                     :energy (dec energy)))
                 bunnies)]
    (let [[nbworld nbs2]
          (ttmap (fn [bworld {:keys [p path] :as bunny}]
                   (if (and (first path)
                            (< (distance (last path) p) 0.1)
                            (= :tall-grass (get-in-map bworld (round p))))
                     [(assoc-in-map bworld (round p) :grass)
                      (update-in bunny [:energy] (partial + 200))]
                     [bworld bunny]))
                 bworld
                 nbs)
          nbs3 (mapcat (fn [{:keys [energy seed] :as bunny}]
                         (if (< energy 50000)
                           [bunny]
                           [(assoc bunny :energy 2500)
                            (assoc bunny :energy 2500 :seed (+ *seed* seed))]))
                       nbs2)]
      (assoc state
        :bworld nbworld
        :bunnies (vec nbs3)))))

(defstep [bullets deadbunnies bunnies bullet-speed]
  (let [[bus bns] (process-bullets state :bunnies)]
    (let [{ls nil ds true} (group-by :dead
                                     bns)]
      (assoc state
        :bullets bus
        :deadbunnies (concat deadbunnies ds)
        :bunnies ls))))

(defstep [players bworld]
  (assoc state
    :players (into {} (map (fn [[pid player]]
                             [pid (new-pos player human-walk-speeds bworld)])
                           players))))

(defstep [bullets players bullet-speed spawn-point]
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

(defstep [bworld players]
   (assoc state
    :bworld (reduce (fn [bw pl]
                     (let [tilep (round (:p pl))
                           succ ({:tall-grass :grass
                                  :grass :dirt}
                                 (get-in-map bw tilep))]
                       (if (and succ
                                (= 0 (mod (apply bit-xor *seed* tilep)
                                          9)))
                         (assoc-in-map bw tilep succ)
                         bw)))
                   bworld
                   (vals players))))

(defn walk [{:keys [mworld players] :as state} pid p]
  (let [goal (->> (line-affects (get-in players [pid :p]) p)
                  (sort-by #(distance % p))
                  (drop-while #(not (walkable? (get-in-map mworld %))))
                  first)]
    (if goal
      (let [pp (get-in players [pid :p])
            path (route (round pp)
                        (round goal)
                        human-walk-speeds
                        mworld)]
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
          (= :spear selected)
          (assoc state
            :bullets (conj (:bullets state) (let [m (direction p goalp)]
                                              {:p (plus p (mult m 0.75))
                                               :ttl 500
                                               :m m}))
            :players (update-in (:players state) [pid] steal-player :spear 1))
          (and (interactions [selected (get-in-map (:mworld state) tilep)])
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

(defstep [players c-sites mworld]
  (let [[new-c-sites new-players]
        (ttmap (fn [cs [pid {:keys [p do-at inventar inventar-p] :as player}]]
                   (if (and do-at
                            (< (distance p do-at) 5)
                            (not (some #(= do-at (:p %)) cs)))
                     (let [selected (first (nth inventar inventar-p))
                           foo (interactions [selected (get-in-map mworld do-at)])]
                       (if foo
                         [(conj cs {:p do-at :t 0 :owner pid :tile (:tile foo) :give (:give foo)})
                          [pid (assoc (reduce (fn [pl [x n]]
                                                (steal-player pl x n))
                                              player
                                              (:take foo))
                                 :do-at nil
                                 :path nil)]]
                         [cs [pid player]]))
                     [cs [pid player]]))
               c-sites
               players)]
    (assoc state
      :players (into {} new-players)
      :c-sites new-c-sites)))

(defn decip [{:keys [inventar inventar-p] :as player}]
  (assoc player
    :inventar-p (mod (dec inventar-p) (count inventar))))

(defn incip [{:keys [inventar inventar-p] :as player}]
  (assoc player
    :inventar-p (mod (inc inventar-p) (count inventar))))

(defn player-has? [{:keys [inventar]} x n]
  (some #(and (= (first %) x) (<= n (second %)))
        inventar))

(defn build [{:keys [c-sites mworld bworld players] :as state} pid xs]
  (let [player (players pid)
        p (round (:p player))
        r (recipes xs)
        freespot (first (filter #(and (not (get-in-map mworld %))
                                      (not (some (fn [c] (= % (:p c))) c-sites)))
                                (far-ngbrs p 2 bworld)))]
    (if (and r
             freespot
             (every? (fn [[y n]]
                       (player-has? player y n))
                     r))
      (assoc state
        :players (assoc players
                   pid (reduce (fn [pl [y n]]
                                 (steal-player pl y n))
                               player
                               r))
        :c-sites (conj c-sites {:p freespot :t 0 :owner pid
                                :tile (get-in-map mworld freespot) :give (map (fn [x]
                                                                               [x 1])
                                                                             xs)}))
      state)))

(defn step-tile [p bw mw]
  (let [btile (get-in-map bw p)
        mtile (get-in-map mw p)
        bns (frequencies (map #(get-in-map bw %)
                              (unchecked-ngbrs p)))
        mns (frequencies (map #(get-in-map mw %)
                              (unchecked-ngbrs p)))
        no-shrub (fn []
                   (let [ns2 (frequencies (map #(get-in-map mw %)
                                               (far-ngbrs p 2 mw)))]
                     (and (< 5 (+ (or (:shrub ns2) 0) (or (:shrub-pear ns2) 0)))
                          (not= 0 (mod (prng p) 3)))))
        res (cond (or (#{:tree :wall :windowed-wall :door :campfire-on :chest} mtile)
                      (and (#{:shrub :shrub-pear} mtile)
                           (or (:tree mns)
                               (no-shrub))))
                  [(rand-int 999) btile nil]
                  (and (= :dirt btile) (:tree mns))
                  [nil btile mtile]
                  (and (= :dirt btile) (or (:grass bns) (:tall-grass bns)))
                  [(rand-int 99999) :grass mtile]
                  (and (#{:grass :tall-grass} btile) (:tree mns))
                  [(rand-int 999) :dirt mtile]
                  (= :grass btile)
                  [(rand-int 99999) :tall-grass mtile]
                  (and (= :tall-grass btile) (not mtile) (or (:shrub mns) (:shrub-pear mns))
                       (not (no-shrub)))
                  [(rand-int 99999) btile :shrub]
                  (= :shrub mtile)
                  [(rand-int 99999) btile :shrub-pear])]
    (if (or (not res) (and (= (nth res 1) btile) (= (nth res 2) mtile)))
      [nil btile mtile]
      res)))

(defn exec-message [state msg]
  (condp = (first msg)
    :plcmd (let [pid (second msg)
                 cmd (nth msg 2)]
             (condp = (first cmd)
               :walk (walk state pid (second cmd))
               :shot (shot state pid (second cmd))
               :decip (update-in state [:players pid] decip)
               :incip (update-in state [:players pid] incip)
               :build (build state pid (second cmd))))
    :tile (let [[_ btile mtile] (step-tile (second msg) (:bworld state) (:mworld state))]
            (assoc state
              :bworld (assoc-in-map (:bworld state) (second msg) btile)
              :mworld (assoc-in-map (:mworld state) (second msg) mtile)))
    :sapling (let [[_ sapp treep] msg
                   bworld (:bworld state)]
               (if (and (#{:dirt :grass :tall-grass}
                         (get-in-map bworld sapp))
                        (not (:tree (frequencies (map #(get-in-map bworld %)
                                                      (far-ngbrs sapp 2 bworld)))))
                        (= :tree (get-in-map bworld treep)))
                 (update-in state [:bworld] #(assoc-in-map % (second msg) :tree))
                 state))
    :new-player (assoc-in state [:players (second msg)]
                          (assoc (nth msg 2) :p (:spawn-point state)))))

(defn exec-messages [state msgs]
  (reduce exec-message state msgs))

(defstep [mworld c-sites players]
  (assoc state
    :c-sites (keep (fn [cs]
                     (if (< (:t cs) 100)
                       (assoc cs :t (inc (:t cs)))))
                   c-sites)
    :mworld (reduce (fn [mw {:keys [p t tile]}]
                     (if (>= t 100)
                       (assoc-in-map mw p tile)
                       mw))
                   mworld
                   c-sites)
    :players (reduce (fn [pls {:keys [p t give owner]}]
                       (if (and (>= t 100))
                         (assoc pls
                           owner (reduce (fn [pl [x n]]
                                           (give-player pl x n))
                                         (pls owner)
                                         give))
                         pls))
                     players
                     c-sites)))

(defn step [state msgs]
  (reduce (fn [st f]
            (f st))
          (exec-messages state msgs)
          @step-fns))
