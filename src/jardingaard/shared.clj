(ns jardingaard.shared
  (:use [jardingaard util reducers rules helpers field]))

(def tick-duration 16)

(defstep [world]
  (update-in state [:players]
             #(map-field (fn [player]
                           (new-pos player (make-p->ws human-walk-speeds world)))
                         %)))

(defstep [world]
  (update-in state [:lumberjacks]
             #(map-field (fn [lj]
                           (if (ready? lj)
                             (new-pos lj (make-p->ws human-walk-speeds world))
                             lj))
                         %)))

(defstep [world]
  (update-in state [:carpenters]
             #(map-field (fn [cp]
                           (if (ready? cp)
                             (new-pos cp (make-p->ws human-walk-speeds world))
                             cp))
                         %)))

(defstep [world]
  (update state :zombies
          (fn [zombies]
            (map-field (fn [zb]
                         (if (ready? zb)
                           (let [p->ws (make-p->ws zombie-walk-speeds world)
                                 newzb (new-pos zb p->ws)]
                             (if (some #(let [d (distance (:p %) (:p zb))]
                                          (and (< d 0.7)
                                               (< (distance (:p %) (:p newzb)) d)))
                                       zombies)
                               (new-pos zb (comp #(* 2 %) p->ws))
                               newzb))
                           zb))
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
        (updates state arrows
                 (fn [state arrow]
                   (let [target ((:zombies state) (:target arrow))]
                     (if (< (distance (:p arrow) (:p target)) arrow-speed)
                       (-> (update-in state [:zombies (:target arrow) :hp] dec)
                           (update :arrows dissoc (:i arrow))
                           (assoc-in [:zombies (:target arrow) :last-hit] (:owner arrow)))
                       (update state :arrows conj (update arrow :p plus (mult (direction (:p arrow) (:p target)) arrow-speed)))))))
        (updates state (:zombies state)
                 (fn [state zb]
                   (if (<= (:hp zb) 0)
                     (-> (update state :zombies dissoc (:i zb))
                         (update-in [:players (:last-hit zb)] give-player :gold 1)))))
        (updates state (:arrows state)
                 (fn [state arrow]
                   (if-not ((:zombies state) (:target arrow))
                     (update state :arrows dissoc (:i arrow)))))))

(defn find-tree [buildings pa pb]
  (->> (get-map-part buildings pa [4 4])
       (filter (fn [building]
                 (and (= :tree (:type building))
                      (ready? building))))
       (sort-by (fn [tree]
                  [(distance pb (:p tree))
                   (distance pa (:p tree))
                   (:spawn tree)]))
       first))

(defstep [lumberjacks]
  (reduce (fn [{:keys [buildings world] :as state} lj]
            (let [tile (world (->tilep (:p lj)))
                  p (->blp (:p lj))
                  bl (buildings p)]
              (cond (= (:home lj) p)
                    (cond-> (update state :lumberjacks dissoc (:i lj))
                            (:inventar lj)
                            (update-in [:players (:owner tile)] give-player :wood 1)
                            (= (:type bl) :lumberjack)
                            (assoc-in [:buildings p] (assoc bl
                                                       :spawn *tick*
                                                       :working false)))
                    (and (= (:type bl) :tree)
                         (ready? bl))
                    (-> (assoc-in state [:buildings p :spawn] *tick*)
                        (update :lumberjacks conj (assoc lj
                                                    :inventar true
                                                    :spawn *tick*
                                                    :path (route2 (:p lj) (:home lj) (make-p->ws human-walk-speeds world)))))
                    :else (let [dest (or (:p (find-tree buildings (:home lj) (:p lj)))
                                         (:home lj))]
                            (update state :lumberjacks conj (assoc lj
                                                              :spawn *tick*
                                                              :path (route2 (:p lj) dest (make-p->ws human-walk-speeds world))))))))
          state
          (filter (comp not walking?) lumberjacks)))

(defn repairable-building? [building]
  (or (:broken building)
      (and (:hp building) (< (health-fraction building) 1))))

(defn repair-building [building]
  (cond (:broken building)
        (-> (update building :broken (comp nil0 dec))
            (assoc :spawn *tick*))
        (and (:hp building) (< (health-fraction building) 1))
        (update building :hp inc)
        :else
        (throw (ex-info "building not repairable"))))

(defstep [carpenters]
  (reduce (fn [{:keys [buildings world] :as state} cp]
            (let [tile (world (->tilep (:p cp)))
                  p (->blp (:p cp))
                  bl (buildings p)]
              (if (= (:home cp) p)
                (cond-> (update state :carpenters dissoc (:i cp))
                        (= (:type bl) :carpenter)
                        (assoc-in [:buildings p] (assoc bl
                                                   :spawn *tick*
                                                   :working false)))
                (cond-> (update state :carpenters conj (assoc cp
                                                         :spawn *tick*
                                                         :path (route2 (:p cp) (:home cp) (make-p->ws human-walk-speeds world))))
                        (repairable-building? bl)
                        (update-in [:buildings p] repair-building)))))
          state
          (filter (comp not walking?) carpenters)))

(defn attackable-building? [{:keys [type broken]}]
  (and (+hp+ type) (not broken)))

(defn get-zombie-target [p buildings randarg]
  (if-let [targets (seq (filter #(and (attackable-building? %)
                                      (< (distance p (:p %)) 8))
                                (vals buildings)))]
    (prng-nth targets *tick* (->tilep p) randarg)))

(defstep [zombies world]
  (preduce (fn [{:keys [buildings] :as state} zb]
             (if (ready? zb)
               (let [p (->blp (:p zb))
                     {:keys [type hp] :as bl} (buildings p)]
                 (if (attackable-building? bl)
                   (-> (assoc-in state [:buildings p]
                                 (let [newhp (dec hp)]
                                   (if (<= newhp 0)
                                     (cond-> (assoc bl
                                               :hp (+hp+ type)
                                               :broken (*broken* type))
                                             (= type :idol) (assoc :merit 0))
                                     (assoc bl :hp newhp))))
                       (assoc-in [:zombies (:i zb) :spawn] *tick*))
                   (if-let [target (get-zombie-target p buildings (:i zb))]
                     (assoc-in state [:zombies (:i zb) :path] (route2 (:p zb) (:p target) (make-p->ws zombie-walk-speeds world))))))))
           state
           (filter (comp not walking?) zombies)))

(defstep [players zombies]
  (reduce (fn [state player]
             (reduce (fn [state zb]
                       (-> (update-in state [:players (:i player) :hp] dec)
                           (assoc-in [:zombies (:i zb) :spawn] *tick*)))
                     state
                     (filter ready? (in-radius zombies (:p player) 1))))
          state
          players))

(defn get-target [p zombies randarg]
  (some-> (seq (in-radius zombies p 8))
          (prng-nth *tick* (->tilep p) randarg)))

(defstep [players zombies]
  (preduce (fn [state player]
             (if-let [target (and (ready? (:arrow-spawn player) 20)
                                  (get-target (:p player) zombies (:i player)))]
               (-> (update state :arrows conj {:p (:p player)
                                               :owner (:i player)
                                               :target (:i target)})
                   (assoc-in [:players (:i player) :arrow-spawn] *tick*))))
           state
           players))

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
        tilep (->tilep goalp)
        tile ((:world state) tilep)
        blp (->blp goalp)
        bl ((:buildings state) blp)]
    (cond (and (grounds selected)
               (or (not (:owner tile))
                   (= pid (:owner tile))))
          (let [oldground (:ground tile)]
            (-> (update-in state [:world] conjp {:owner pid
                                                 :ground selected
                                                 :p tilep})
                (update-in [:players pid] steal-player selected 1)
                (cond-> oldground (update-in [:players pid] give-player oldground 1))))
          (and (= :pickaxe selected)
               (= (:owner tile) pid))
          (if-let [building (:type bl)]
            (-> (update state :buildings dissoc blp)
                (update-in [:players pid] give-player building 1))
            (-> (update state :world dissoc tilep)
                (update-in [:players pid] give-player (:ground tile) 1)))
          (and (placable selected)
               (not bl))
          (-> (update-in state [:buildings blp] assoc
                         :type selected
                         :p blp
                         :spawn *tick*)
              (cond-> (= :idol selected)
                      (update-in [:buildings blp] assoc :merit 0)
                      (+hp+ selected)
                      (update-in [:buildings blp] assoc :hp (+hp+ selected))
                      (*broken* selected)
                      (update-in [:buildings blp] assoc :broken (*broken* selected)))
              (update-in [:players pid] steal-player selected 1))
          (and (= :hands selected) (:type bl) (ready? bl) (object-fruits (:type bl)))
          (-> (assoc-in state [:buildings blp :spawn] *tick*)
              (update-in [:players pid] give-player (object-fruits (:type bl)) 1))
          (and (= :gold selected) (= :idol (:type bl)) (not (:broken bl)))
          (-> (update-in state [:buildings blp :merit] inc)
              (update-in [:players pid :merit] inc)
              (update-in [:players pid] steal-player :gold 1))
          (and (= :wood selected) (repairable-building? bl))
          (-> (update-in state [:buildings blp] repair-building)
              (assoc-in [:buildings blp :repair-spawn] *tick*)
              (update-in [:players pid] steal-player :wood 1))
          :else
          state)))

(defstep [players world]
  (assoc state
    :players (reduce (fn [ps player]
                       (if-let [beneficary (and (ready? (:gold-spawn player) (seconds 10))
                                                (:owner (world (->tilep (:p player)))))]
                         (-> (assoc-in ps [(:i player) :gold-spawn] *tick*)
                             (update-in [beneficary :inventar] add-items :gold 1))
                         ps))
                     players
                     players)))

(defn player-has? [{:keys [inventar]} x n]
  (some #(and (= (first %) x) (<= n (second %)))
        inventar))

(defmulti step-building (fn [state bl] (:type bl)))

(defmethod step-building :default [state bl] nil)

(defmethod step-building :lumberjack [{:keys [buildings world] :as state} {:keys [p] :as bl}]
  (if-let [tree (find-tree buildings p p)]
    (-> (assoc-in state [:buildings p :working] true)
        (update :lumberjacks conj
                {:p p
                 :home p
                 :type :lumberjack-being
                 :spawn *tick*
                 :path (route2 p (:p tree) (make-p->ws human-walk-speeds world))}))))

(defmethod step-building :carpenter [{:keys [buildings world players] :as state} {:keys [p] :as bl}]
  (let [owner (get-in world [(->tilep p) :owner])]
    (if-let [target (and (player-has? (players owner) :wood 1)
                         (->> (get-map-part buildings p [4 4])
                              (filter repairable-building?)
                              (sort-by #(or (:repair-spawn %) 0) >)
                              first))]
      (-> (assoc-in state [:buildings p :working] true)
          (update-in [:players owner] steal-player :wood 1)
          (update :carpenters conj
                  {:p p
                   :home p
                   :type :carpenter-being
                   :spawn *tick*
                   :path (route2 p (:p target) (make-p->ws human-walk-speeds world))})))))

(defmethod step-building :idol [{:keys [world] :as state} bl]
  (if (< 0 (:merit bl))
    (let [arc (mod (prng *tick* (:p bl) (:merit bl)) tau)
          dira (mult (dir<-arc arc) 2)
          dirb (mult (dir<-arc (+ arc (/ tau 4))) 0.3)
          spawn-points (map #(loop [p (plus (mult dirb %) (:p bl))]
                               (if (:ground (world (->tilep p)))
                                 (recur (plus p dira))
                                 p))
                            (range (:merit bl)))]
      (-> (reduce (fn [state p]
                    (update state :zombies conj
                            {:p p
                             :goal (:p bl)
                             :type :zombie
                             :spawn *tick*
                             :hp (+hp+ :zombie)
                             :path (route2 p (:p bl) (make-p->ws zombie-walk-speeds world))}))
                  state
                  spawn-points)
          (assoc-in [:buildings (:p bl) :spawn] *tick*)))))

(defmethod step-building :tower [{:keys [zombies world] :as state} {:keys [type p] :as bl}]
  (if-let [target (get-target p zombies 5)]
    (-> (update state :arrows conj {:p p
                                    :owner (:owner (world (->tilep p)))
                                    :target (:i target)})
        (assoc-in [:buildings p :spawn] *tick*))))

(defstep [players buildings]
  (->> (mapcat (fn [player]
                 (get-map-part buildings (:p player) [4 4]))
               players)
       set
       (filter ready?)
       (preduce step-building state)))

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
               :build (build state pid (second cmd))))))

(defn exec-messages [state msgs]
  (reduce exec-message state msgs))

(defn step [state msgs]
  (reduce (fn [st f]
            (f st))
          (exec-messages state msgs)
          @step-fns))
