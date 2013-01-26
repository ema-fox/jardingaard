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

(defstep [zombies players spawn-point bworld mworld]
  (assoc state
    :zombies (mapv (fn [{:keys [p path seed cooldown] :as zombie}]
                     (assoc (if (and (seq path)
                                     (< 0.1 (distance (last path) p)))
                              (new-pos zombie zombie-walk-speeds bworld)
                              (if-let [goal (and (= 0 (mod (prng *seed* seed (round p)) 9))
                                                 (some (fn [[_ {pp :p}]]
                                                         (if (< (distance p pp) 20)
                                                           pp))
                                                       players))]
                                (assoc zombie
                                  :path (route (round p) (round goal) zombie-walk-speeds mworld))
                                zombie))
                       :cooldown (max 0 (dec cooldown))))
                   (if (< (count zombies) num-zombies)
                     (conj zombies {:p spawn-point
                                    :cooldown 0
                                    :seed *seed*})
                     zombies))))

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


(defstep [bullets bullet-speed zombies]
  (let [[bus zmbs] (process-bullets state :zombies)]
    (assoc state
      :bullets bus
      :zombies (vec (filter #(not (:dead %)) zmbs)))))
