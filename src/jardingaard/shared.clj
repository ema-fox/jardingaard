(ns jardingaard.shared
  ;(:require [jardingaard zombie])
  (:use [jardingaard util reducers rules helpers]))

(def tick-duration 16)

#_(defstep [mworld bullet-speed]
  (key->> state :bullets
          (map (fn [{:keys [p ttl m] :as b}]
                 (let [newp (plus p (mult m bullet-speed))]
                   (assoc b
                     :p newp
                     :ttl (if (some #((cond-> #{:wall :door :tree :granite}
                                              (> ttl 495)
                                              (conj :windowed-wall))
                                      (get-in-map mworld %))
                                    (line-affects p newp))
                            0
                            (dec ttl))))))
          (filter #(< 0 (:ttl %)))))

#_(defn far-ngbrs [[p0 p1 :as p] d w]
  (filter #(and (not= p %)
                (get-in-map w %))
          (for [a0 (range (- p0 d) (+ 1 p0 d))
                a1 (range (- p1 d) (+ 1 p1 d))]
            [a0 a1])))

#_(defstep [bunnies bworld]
  (let [[nbworld nbs]
        (ttmap (fn [bworld {:keys [p path] :as bunny}]
                 (if (and (not (first path))
                          (= :tall-grass (get-in-map bworld (round p))))
                   [(assoc-in-map bworld (round p) :grass)
                    (update-in bunny [:energy] (partial + 200))]
                   [bworld bunny]))
               bworld
               bunnies)]
    (assoc state
      :bworld nbworld
      :bunnies (vec nbs))))

#_(defstep []
  (key->> state :bunnies
          (mapcat (fn [{:keys [energy seed] :as bunny}]
                    (if (< energy 10000)
                      [bunny]
                      [(assoc bunny :energy 2500)
                       (assoc bunny :energy 2500 :seed (+ *tick* seed))])))))

#_(defstep [bullets deadbunnies bunnies bullet-speed]
  (let [[bus bns] (process-bullets state :bunnies)]
    (let [{ls nil ds true} (group-by :dead
                                     bns)]
      (assoc state
        :bullets bus
        :deadbunnies (concat deadbunnies ds)
        :bunnies ls))))

(defstep [spawn-point]
  (key->> state :players
    (map-kv (fn [pid {:keys [hp energy] :as player}]
              (cond-> player
                      (and (< hp 200) (< 160 energy))
                      (assoc :hp (+ hp (* 0.03 (/ (- energy 160) 40))))
                      (< energy 20)
                      (harm-player 0.01 spawn-point))))))

(defstep [world]
  (update-in state [:players]
             #(map-kv (fn [pid player]
                        (new-pos player (make-p->ws human-walk-speeds world)))
                      %)))

(defstep [world]
  (update-in state [:lumberjacks]
             #(map-kv (fn [i lj]
                        (new-pos lj (make-p->ws human-walk-speeds world)))
                      %)))

(defn add-items [items x n]
  (if (some #(= (first %) x) items)
    (mapv #(if (= (first %) x)
             [x (+ (second %) n)]
             %)
          items)
    (conj items [x n])))

(defn give-player [player x n]
  (key-> player :inventar (add-items x n)))

(defstep [lumberjacks]
  (preduce (fn [{:keys [world] :as state} lj]
             (if-not (walking? lj)
               (let [p (round2 (:p lj))
                     tile (world p)]
                 (cond (and (= (:type tile) :tree)
                          (ready? tile))
                       (-> (assoc-in state [:world p :spawn] *tick*)
                           (update-in [:lumberjacks] conji (assoc lj
                                                             :path (route2 (:p lj) (:home lj) (make-p->ws human-walk-speeds world)))))
                       (= (:type tile) :lumberjack)
                       (-> (assoc-in state [:world p] (assoc tile
                                                         :spawn *tick*
                                                         :working false))
                           (update-in [:players (:owner tile)] give-player :wood 1)
                           (update-in [:lumberjacks] dissoc (:i lj)))))))
          state
          (vals lumberjacks)))

#_(defstep [bullets players bullet-speed spawn-point]
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

#_(defstep [bworld players]
   (assoc state
    :bworld (reduce (fn [bw pl]
                     (let [tilep (round (:p pl))
                           succ ({:tall-grass :grass
                                  :grass :dirt}
                                 (get-in-map bw tilep))]
                       (if (and succ
                                (= 0 (mod (apply bit-xor *tick* tilep)
                                          9)))
                         (assoc-in-map bw tilep succ)
                         bw)))
                   bworld
                   (vals players))))

(defn walk [{:keys [world players] :as state} pid p]
  (let [pp (get-in players [pid :p])
        path (route2 pp p (make-p->ws human-walk-speeds world))]
    (assoc-in state [:players pid :path] path)))

(defn sub-items [items x n]
  ;(if-let [i (index-by #(= x (first %)) items)]
  (vec (keep (fn [y]
               (if (not= x (first y))
                 y
                 (if (>= n (second y))
                   nil
                   [x (- (second y) n)])))
             items)))

(defn steal-player [{:keys [inventar inventar-p] :as player} x n]
  (let [new-inventar (sub-items inventar x n)]
    (assoc player
      :inventar new-inventar
      :inventar-p (min inventar-p (dec (count new-inventar))))))

(defn shot [state pid goalp]
  (let [{:keys [p inventar inventar-p]} (get-in state [:players pid])
        selected (first (nth inventar inventar-p))
        tilep (round2 goalp)
        tile ((:world state) tilep)]
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
          #_(and (= :hands selected) (= :chest (get-in-map (:mworld state) tilep)))
          #_(assoc-in state [:players pid :open-chest] tilep)
          #_(and (interactions [selected (get-in-map (:mworld state) tilep)])
               (not (some #(= tilep (:p %)) (:c-sites state))))
          #_(assoc-in (walk state pid tilep) [:players pid :do-at] tilep)
          (placable selected)
          (-> (update-in state [:world tilep] assoc
                         :type selected
                         :spawn *tick*)
              (update-in [:players pid] steal-player selected 1))
          (and (= :hands selected) (:type tile) (ready? tile) (object-fruits (:type tile)))
          (-> (assoc-in state [:world tilep :spawn] *tick*)
              (update-in [:players pid] give-player (object-fruits (:type tile)) 1))
          true
          state)))

(defstep [players world]
  (assoc state
    :players (reduce (fn [ps [pid player]]
                       (if-let [beneficary (and (ready? (:gold-spawn player) (seconds 10))
                                                (:owner (world (round2 (:p player)))))]
                         (-> (assoc-in ps [pid :gold-spawn] *tick*)
                             (update-in [beneficary :inventar] add-items :gold 1))
                         ps))
                     players
                     players)))

#_(defstep [players deadbunnies]
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

(defstep [players world]
  (let [tiles (set (mapcat (fn [player]
                             (get-map-part world (:p player) [4 4]))
                           (vals players)))
        [world ljs] (reduce (fn [[world ljs] tile]
                              (or (and (= :lumberjack (:type tile))
                                       (ready? tile)
                                       (if-let [tree (->> (get-map-part world (:p tile) [4 4])
                                                          (filter (fn [tile2]
                                                                    (and (= :tree (:type tile2))
                                                                         (ready? tile2))))
                                                          (sort-by :spawn)
                                                          first)]
                                         [(assoc-in world [(:p tile) :working] true)
                                          (conj ljs {:p (:p tile)
                                                     :home (:p tile)
                                                     :path (route2 (:p tile) (:p tree)
                                                                   (make-p->ws human-walk-speeds world))})]))
                                  [world ljs]))
                            [world []]
                            tiles)]
    (assoc state
      :world world
      :lumberjacks (reduce conji (:lumberjacks state) ljs))))

#_(defstep [players c-sites mworld]
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
                                              (update-in player [:energy] dec)
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

(defn scrollip
  [{:keys [inventar inventar-p inventar-category-p open-chest] :as player} x {:keys [chests]}]
  (let [items (get chests open-chest)]
    (cond (#{:dec :inc} x)
          (assoc player
            :inventar-p (mod (({:dec dec :inc inc} x) inventar-p)
                             (count (if (= inventar-category-p :inventar)
                                      inventar
                                      items))))
          (< 0 (count items))
          (assoc player
            :inventar-category-p ({:inventar :chest
                                   :chest :inventar} inventar-category-p)
            :inventar-p (mod inventar-p (count (get chests open-chest))))
          true
          player)))

(defn player-has? [{:keys [inventar]} x n]
  (some #(and (= (first %) x) (<= n (second %)))
        inventar))

(defn build [{:keys [players] :as state} pid xs]
  (let [player (players pid)
        r (recipes xs)]
    (if (and r
             (every? (fn [[y n]]
                       (player-has? player y n))
                     r))
      (assoc state
        :players (assoc players
                   pid (reduce (fn [pl x]
                                 (give-player pl x 1))
                               (reduce (fn [pl [y n]]
                                         (steal-player pl y n))
                                       player
                                       r)
                               xs)))
      state)))

(def rotations {:grass :dirt
                :dirt :water
                :water :tall-grass
                :tall-grass :grass})

(defn rotate-tile [{:keys [world players] :as state} pid]
  (let [player (players pid)
        p (round2 (:p player))
        old (or (world p)
                {:ground :grass
                 :owner pid
                 :p p})
        new (update-in old [:ground] rotations)]
    (assoc-in state [:world p] new)))

(defn move-item [{:keys [chests players] :as state} pid]
  (let [{:keys [inventar inventar-p inventar-category-p open-chest energy] :as player} (get players pid)]
    (if open-chest
      (if (= inventar-category-p :inventar)
        (let [selected (first (nth inventar inventar-p))]
          (assoc state
            :players (assoc players
                       pid (steal-player player selected 1))
            :chests (update-in chests [open-chest] add-items selected 1)))
        (let [selected (first (nth (chests open-chest) inventar-p))]
          (assoc state
            :players (assoc players
                       pid (give-player player selected 1))
            :chests (update-in chests [open-chest] sub-items selected 1))))
      (let [selected (first (nth inventar inventar-p))]
        (if (#{:pear :steak-fried} selected)
          (update-in state [:players pid] #(assoc (steal-player % selected 1)
                                             :energy (min 200 (+ energy 1))))
          state)))))

#_(defn step-tile [p bw mw]
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
        res (cond (and (#{:tree :wall :windowed-wall :door :campfire-on :chest} mtile)
                       (not= btile :dirt))
                  [(rand-int 999) :dirt mtile]
                  (and (#{:shrub :shrub-pear :tree} mtile)
                       (or (:tree mns)
                           (no-shrub)))
                  [(rand-int 999) btile nil]
                  (and (= :dirt btile) (:tree mns))
                  [nil btile mtile]
                  (and (= :dirt btile) (#{nil :rock :shrub} mtile)
                       (or (:grass bns) (:tall-grass bns)))
                  [(rand-int 99999) :grass mtile]
                  (and (#{:grass :tall-grass} btile) (:tree mns))
                  [(rand-int 999) :dirt mtile]
                  (= :grass btile)
                  [(rand-int 99999) :tall-grass mtile]
                  (and (#{:dirt :gras :tall-grass} btile) (not mtile)
                       (or (:shrub mns) (:shrub-pear mns))
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
               :close-chest (assoc-in state [:players pid :open-chest] nil)
               :scrollip (update-in state [:players pid] scrollip (second cmd) state)
               :move-item (move-item state pid)
               :build (build state pid (second cmd))
               :rotate-tile (rotate-tile state pid)))
    #_:tile #_(let [[_ btile mtile] (step-tile (second msg) (:bworld state) (:mworld state))]
            (assoc state
              :bworld (assoc-in-map (:bworld state) (second msg) btile)
              :mworld (assoc-in-map (:mworld state) (second msg) mtile)))
    #_:sapling #_(let [[_ sapp treep] msg
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

#_(defstep [mworld c-sites players]
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
