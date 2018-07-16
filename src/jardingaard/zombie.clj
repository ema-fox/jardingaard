(ns jardingaard.zombie
  (:use [jardingaard util helpers rules]))

(def zombie-walk-speeds (into {} (map (fn [[k v]]
                                        [k (if (= k :door)
                                             (* 16 v)
                                             (* 4 v))])
                                      human-walk-speeds)))

(dosync
 (alter gen-fns conj (fn [state]
                       (assoc state
                         :zombies []))))

(defstep [spawn-point]
  (key-> state :zombies (as-> zs (cond-> zs
                                         (< (count zs) num-zombies)
                                         (conj {:p (rand-spawnpoint state)
                                                :cooldown 0
                                                :seed *tick*})))))

(defstep [players bworld mworld]
  (key->> state :zombies
          (map (fn [{:keys [p seed cooldown] :as zombie}]
                 (if (and (walking? zombie) (not= 0 (mod (prng *tick* seed
                                                               (round p)) 9)))
                   (new-pos zombie zombie-walk-speeds bworld)
                   (let [goal (and (= 0 (mod (prng *tick* seed (round p)) 9))
                                   (some (fn [[_ {pp :p}]]
                                           (if (< (distance p pp) 20)
                                             pp))
                                         players))]
                     (cond-> zombie
                             goal
                             (assoc :path (route (round p) (round goal)
                                                 zombie-walk-speeds mworld)))))))))

(defstep []
  (key->> state :zombies
          (map (fn [{:keys [cooldown] :as zombie}]
                 (assoc zombie :cooldown (max 0 (dec cooldown)))))))

(defstep [zombies players spawn-point]
  (let [[zmbs pls] (ttmap (fn [zombies [pid {:keys [p] :as player}]]
                            (let [newzombies (map (fn [{zp :p :keys [cooldown] :as zombie}]
                                                    (cond-> zombie
                                                            (and (= 0 cooldown)
                                                                 (< (distance p zp) 1))
                                                            (assoc :cooldown 10)))
                                                  zombies)
                                  damage (* 10 (count (filter not (map = zombies newzombies))))]
                              [newzombies
                               [pid (harm-player player damage spawn-point)]]))
                          zombies
                          players)]
    (assoc state
      :zombies zmbs
      :players (into {} pls))))


(defstep [bullets bullet-speed zombies]
  (let [[bus zmbs] (process-bullets state :zombies)]
    (assoc state
      :bullets bus
      :zombies (vec (filter #(not (:dead %)) zmbs)))))
