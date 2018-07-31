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
                        (if (ready? lj)
                          (new-pos lj (make-p->ws human-walk-speeds world))
                          lj))
                      %)))

(defstep [world]
  (update state :zombies
          (fn [zombies]
            (map-kv (fn [i zb]
                      (let [p->ws (make-p->ws zombie-walk-speeds world)
                            newzb (new-pos zb p->ws)]
                        (if (some #(let [d (distance (:p %) (:p zb))]
                                     (and (< d 0.5)
                                          (< (distance (:p %) (:p newzb)) d)))
                                  (vals zombies))
                          (new-pos zb (comp #(* 2 %) p->ws))
                          newzb)))
                    zombies))))

(defn add-items [items x n]
  (if (some #(= (first %) x) items)
    (mapv #(if (= (first %) x)
             [x (+ (second %) n)]
             %)
          items)
    (conj items [x n])))

(defn give-player [player x n]
  (key-> player :inventar (add-items x n)))

(defstep [arrows]
  (as-> state
        state
        (updates state (vals arrows)
                 (fn [state arrow]
                   (let [target ((:zombies state) (:target arrow))]
                     (if (< (distance (:p arrow) (:p target)) arrow-speed)
                       (-> (update-in state [:zombies (:target arrow) :hp] dec)
                           (update :arrows dissoc (:i arrow))
                           (assoc-in [:zombies (:target arrow) :last-hit] (:owner arrow)))
                       (update state :arrows conji (update arrow :p plus (mult (direction (:p arrow) (:p target)) arrow-speed)))))))
        (updates state (vals (:zombies state))
                 (fn [state zb]
                   (if (<= (:hp zb) 0)
                     (-> (update state :zombies dissoc (:i zb))
                         (update-in [:players (:last-hit zb)] give-player :gold 1)))))
        (updates state (vals (:arrows state))
                 (fn [state arrow]
                   (if-not ((:zombies state) (:target arrow))
                     (update state :arrows dissoc (:i arrow)))))))

(defn find-tree [world pa pb]
  (->> (get-map-part world pa [4 4])
       (filter (fn [tile]
                 (and (= :tree (:type tile))
                      (ready? tile))))
       (sort-by (fn [tree]
                  [(distance pb (:p tree))
                   (distance pa (:p tree))
                   (:spawn tree)]))
       first))

(defstep [lumberjacks]
  (reduce (fn [{:keys [world] :as state} lj]
            (let [p (round2 (:p lj))
                  tile (world p)]
              (cond (= (:home lj) p)
                    (cond-> (update state :lumberjacks dissoc (:i lj))
                            (:inventar lj)
                            (update-in [:players (:owner tile)] give-player :wood 1)
                            (= (:type tile) :lumberjack)
                            (assoc-in [:world p] (assoc tile
                                                   :spawn *tick*
                                                   :working false)))
                    (and (= (:type tile) :tree)
                         (ready? tile))
                    (-> (assoc-in state [:world p :spawn] *tick*)
                        (update :lumberjacks conji (assoc lj
                                                     :inventar true
                                                     :spawn *tick*
                                                     :path (route2 (:p lj) (:home lj) (make-p->ws human-walk-speeds world)))))
                    :else (let [dest (or (:p (find-tree world (:home lj) (:p lj)))
                                         (:home lj))]
                            (update state :lumberjacks conji (assoc lj
                                                               :spawn *tick*
                                                               :path (route2 (:p lj) dest (make-p->ws human-walk-speeds world))))))))
          state
          (filter (comp not walking?) (vals lumberjacks))))

(defn remove-object [state tilep]
  (update-in state [:world tilep] dissoc :type :spawn :working :merit :broken))

(defstep [zombies]
  (preduce (fn [{:keys [world] :as state} zb]
             (let [p (round2 (:p zb))
                   tile (world p)]
               (if (and (ready? zb)
                        (= (:type tile) :idol)
                        (not (:broken tile)))
                 (-> (update-in state [:world p] (fn [idol]
                                                   (let [newhp (dec (:hp idol))]
                                                     (if (<= newhp 0)
                                                       (assoc idol
                                                         :hp (+hp+ :idol)
                                                         :broken (*broken* :idol)
                                                         :merit 0)
                                                       (assoc idol
                                                         :hp newhp)))))
                     (assoc-in [:zombies (:i zb) :spawn] *tick*)))))
           state
           (filter (comp not walking?) (vals zombies))))

(defstep [players zombies]
  (preduce (fn [state [pid player]]
             (if-let [targets (and (ready? (:arrow-spawn player) 20)
                                   (seq (filter #(< (distance (:p player) (:p %)) 8)
                                                (vals zombies))))]
               (let [target (prng-nth targets *tick* (round2 (:p player)) pid)]
                 (-> (update state :arrows conji {:p (:p player)
                                                  :owner pid
                                                  :target (:i target)})
                     (assoc-in [:players pid :arrow-spawn] *tick*)))))
           state
           players))

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
          (and (grounds selected)
               (not (:owner tile)))
          (-> (update-in state [:world] conjp {:owner pid
                                               :ground selected
                                               :p tilep})
              (update-in [:players pid] steal-player selected 1))
          (and (= :pickaxe selected)
               (= (:owner tile) pid))
          (if-let [object (:type tile)]
            (-> (remove-object state tilep)
                (update-in [:players pid] give-player object 1))
            (-> (update-in state [:world] dissoc tilep)
                (update-in [:players pid] give-player (:ground tile) 1)))
          (placable selected)
          (-> (update-in state [:world tilep] assoc
                         :type selected
                         :spawn *tick*)
              (cond-> (= :idol selected)
                      (update-in [:world tilep] assoc :merit 0)
                      (+hp+ selected)
                      (update-in [:world tilep] assoc :hp (+hp+ selected))
                      (*broken* selected)
                      (update-in [:world tilep] assoc :broken (*broken* selected)))
              (update-in [:players pid] steal-player selected 1))
          (and (= :hands selected) (:type tile) (ready? tile) (object-fruits (:type tile)))
          (-> (assoc-in state [:world tilep :spawn] *tick*)
              (update-in [:players pid] give-player (object-fruits (:type tile)) 1))
          (and (= :gold selected) (= :idol (:type tile)) (not (:broken tile)))
          (-> (update-in state [:world tilep :merit] inc)
              (update-in [:players pid :merit] inc)
              (update-in [:players pid] steal-player :gold 1))
          (and (= :wood selected) (:broken tile))
          (-> (update-in state [:world tilep :broken] (comp nil0 dec))
              (assoc-in [:world tilep :spawn] *tick*)
              (update-in [:players pid] steal-player :wood 1))
          (and (= :wood selected) (:hp tile) (< (health-fraction tile) 1))
          (-> (update-in state [:world tilep :hp] inc)
              (update-in [:players pid] steal-player :wood 1))
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
                           (vals players)))]
    (preduce (fn [{:keys [world] :as state} tile]
               (and (= :lumberjack (:type tile))
                    (ready? tile)
                    (if-let [tree (find-tree world (:p tile) (:p tile))]
                      (-> (assoc-in state [:world (:p tile) :working] true)
                          (update :lumberjacks conji
                                  {:p (:p tile)
                                   :home (:p tile)
                                   :type :lumberjack-being
                                   :spawn *tick*
                                   :path (route2 (:p tile) (:p tree) (make-p->ws human-walk-speeds world))})))))
             state
             tiles)))

(defstep [players world]
  (let [tiles (set (mapcat (fn [player]
                             (get-map-part world (:p player) [4 4]))
                           (vals players)))]
    (preduce (fn [{:keys [world] :as state} tile]
               (and (= :idol (:type tile))
                    (ready? tile)
                    (< 0 (:merit tile))
                    (let [arc (mod (prng *tick* (:p tile) (:merit tile)) tau)
                          dira (mult (dir<-arc arc) 2)
                          dirb (mult (dir<-arc (+ arc (/ tau 4))) 0.3)
                          spawn-points (map #(loop [p (plus (mult dirb %) (:p tile))]
                                               (if (:ground (world (round2 p)))
                                                 (recur (plus p dira))
                                                 p))
                                            (range (:merit tile)))]
                      (-> (reduce (fn [state p]
                                    (update state :zombies conji
                                            {:p p
                                             :goal (:p tile)
                                             :type :zombie
                                             :spawn *tick*
                                             :hp (+hp+ :zombie)
                                             :path (route2 p (:p tile) (make-p->ws zombie-walk-speeds world))}))
                                  state
                                  spawn-points)
                          (assoc-in [:world (:p tile) :spawn] *tick*)))))
             state
             tiles)))

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
               :build (build state pid (second cmd))))
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
