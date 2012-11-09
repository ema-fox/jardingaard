(ns lom.server
  (:use [lom util shared])
  (:import [java.net ServerSocket]
           [java.util Date]
           [java.io OutputStreamWriter InputStreamReader]
           [clojure.lang LineNumberingPushbackReader]))

(set! *warn-on-reflection* true)

(def socket)

(def vowels "aioeu")
(def consonants "mnprlstgwft")

(def world-size 100)

(def bullet-speed 1)

(def fr-counter (ref 0))

(def todo (ref {}))

(def conns (ref {}))

(def world-state (new-world world-size bullet-speed))

(defn send-client [conn msg]
  (try
    (binding [*out* (OutputStreamWriter. (.getOutputStream conn))]
      (prn msg))
    (catch java.net.SocketException e
      (dosync
       (alter conns dissoc conn)))))

(defn inform-cls [msg]
  (doseq [[conn _] @conns]
    (send-client conn msg)))

(defn msg-pos []
  (inform-cls {:players (into {} (map (fn [[pid player]]
                                        [pid (dissoc player :do-at)])
                                      @players))
               :c-sites @c-sites
               :bullets (vec @bullets)}))

(defn manhatten [[a0 a1] [b0 b1]]
  (+ (Math/abs (int (- a0 b0)))
     (Math/abs (int (- a1 b1)))))

(defn ngbrs [p w]
  (filter #(get-in w %) (map #(plus p %)
                             [[0 1]
                              [0 -1]
                              [1 0]
                              [-1 0]])))

;add maximum search distance
(defn route [start goal w]
  (loop [open {start {:ps [start]
                      :d 0
                      :h (manhatten start goal)}}
         closed #{}]
    (if (first open)
      (let [[endp {:keys [ps d h]}] (reduce (fn a [[_ {da :d ha :h} :as a] [_ {db :d hb :h} :as b]]
                                              (if (< (+ da ha) (+ db hb))
                                                a
                                                b))
                                            open)]
        (if (= endp goal)
          ps
          (recur (merge-with (fn b [a b]
                               (if (< (+ (:d a) (:h a))
                                      (+ (:d b) (:h b)))
                                 a
                                 b))
                             (dissoc open endp)
                             (into {} (for [p (filter #(and (not (closed %))
                                                            (#{:dirt :gras :door} (get-in w %))
                                                            (< (manhatten % goal) 40))
                                                      (ngbrs endp w))]
                                        [p {:ps (conj ps p)
                                            :d (+ 1 d (let [n (get-in w p)]
                                                        (if (= n :dirt)
                                                          0
                                                          0.5)))
                                            :h (manhatten p goal)}])))
                 (conj closed endp))))
      nil)))

(defn give-player [{:keys [inventar] :as player} x n]
  (assoc player
    :inventar (if (some #(and (not (keyword? %)) (= (first %) x)) inventar)
                (mapv #(if (and (not (keyword? %)) (= (first %) x))
                         [x (+ (second %) n)]
                         %)
                      inventar)
                (conj inventar [x n]))))

(defn steal-player [{:keys [inventar inventar-p] :as player} x n]
  (let [new-inventar (vec (keep (fn [y]
                                  (if (or (keyword? y) (not= x (first y)))
                                    y
                                    (if (>= n (second y))
                                      nil
                                      [x (- (second y) n)])))
                                inventar))]
    (assoc player
      :inventar new-inventar
      :inventar-p (min inventar-p (dec (count new-inventar))))))

(defn player-has? [{:keys [inventar]} x n]
  (some #(and (not (keyword? %)) (= (first %) x) (<= n (second %)))
        inventar))

(defn set-world-tile! [p tile]
  (when-not (= tile (get-in @world p))
    (alter world assoc-in p tile)
    (inform-cls [:world-tile p tile])))

(defn step-c-sites []
  (doseq [{:keys [p t owner x]} @c-sites]
    (if (>= t 100)
      (set-world-tile! p (let [tile (get-in @world p)]
                           (cond
                            (= tile :dirt) x
                            (= tile :gras) x
                            (#{:wall :door :windowed-wall} tile)
                            (do (alter players update-in [owner] (fn [player]
                                                                   (give-player player tile 1)))
                                :dirt)
                            (= tile :shrub)
                            (do (alter players update-in [owner] (fn [player]
                                                                   (give-player player :twig 4)))
                                :dirt))))))
  (ref-set c-sites (keep (fn [cs]
                           (if (< (:t cs) 100)
                             (assoc cs :t (inc (:t cs)))))
                         @c-sites)))
(defn step-tile [p w]
  (let [tile (get-in w p)
        ns (frequencies (map #(get-in w %)
                             (ngbrs p w)))]
    (condp = tile
      :dirt (if (or (:gras ns) (:shrub ns))
              :gras
              tile)
      :gras (if (:shrub ns)
              :shrub
              tile)
      :shrub (if (and (:shrub ns)
                      (< 2 (:shrub ns)))
               :dirt
               tile)
      tile)))

(defn tile-delay [p w]
  (let [tile (get-in w p)
        ns (frequencies (map #(get-in w %)
                             (ngbrs p w)))]
    (condp = tile
      :dirt (if (or (:gras ns) (:shrub ns))
              (rand-int 9999))
      :gras (if (:shrub ns)
              (rand-int 99999))
      :shrub (if (and (:shrub ns)
                     (< 2 (:shrub ns)))
               (rand-int 99))
      nil)))

(defn step-world []
  (doseq [p (@todo @fr-counter)]
    (set-world-tile! p (step-tile p @world)))
  (doseq [p (set (mapcat #(ngbrs % @world) (@todo @fr-counter)))
          :let [delay (tile-delay p @world)]
          :when delay]
    (alter todo update-in [(+ 1 @fr-counter delay)] conj p))
  (alter todo dissoc @fr-counter)
  (alter fr-counter inc)
  (doseq [[_ {:keys [p]}] @players]
    (let [tilep (round p)]
      (when (and (= :gras (get-in @world tilep)) (= 0 (rand-int 3)))
        (set-world-tile! tilep :dirt)
        (alter todo update-in [(+ 1 @fr-counter (rand-int 9999))] conj tilep)))))

(defn step []
  (dosync
   (step-c-sites)
   (step-world)
   (alter world-state step-bullets&players)))
  
(defn steps []
  (loop []
    (let [start-t (.getTime (Date.))]
      (step)
      (inform-cls [:frame-count  @fr-counter])
      (msg-pos)
      ;(prn (- (.getTime (Date.)) start-t))
      (Thread/sleep (max 0 (- 33 (- (.getTime (Date.)) start-t)))))
    (recur)))

(defn walk [{:keys [world :players] :as state} pid p]
  (let [goal (if (= #{:dirt :gras} (get-in world (round p)))
               p
               (->> (line-affects (get-in players [pid :p]) p)
                    (sort-by #(distance % p))
                    (drop-while #(#{:wall :shrub :windowed-wall} (get-in world %)))
                    first))
        path (route (round (get-in players [pid :p]))
                    (round goal)
                    world)]
    (assoc-in state [:players pid :path] path)))

(defn walk! [pid p]
  (dosync
   (alter world-state walk pid p)))

(defn call-near [pid goalp f]
  (let [{:keys [p]} (@players pid)]
    (if (< (distance p goalp) 5)
      (f)
      (let [workp (round (plus goalp (mult (direction goalp p) 4)))]
        (walk! pid workp)
        (dosync
         (alter players assoc-in [pid :do-at] [workp f]))))))

(defn shot! [pid goalp]
  (let [{:keys [inventar inventar-p p] :as player} (@players pid)
        selected (nth inventar inventar-p)]
    (cond (= :gun selected)
          (dosync
           (alter bullets conj (let [m (direction p goalp)]
                                 {:p (plus p (mult m 0.75))
                                  :ttl 500
                                  :m m})))
          (= :pickaxe selected)
          (let [tilep (round goalp)]
            (if (and (#{:wall :shrub :windowed-wall :door} (get-in @world tilep))
                     (not (some #(= tilep (:p %)) @c-sites)))
              (call-near pid tilep #(dosync (alter c-sites conj {:p tilep :t 0 :owner pid})))))
          (#{:wall :windowed-wall :door} (first selected))
          (let [tilep (round goalp)]
            (when (and (#{:gras :dirt} (get-in @world tilep))
                       (not (some #(= tilep (:p %)) @c-sites)))
              (call-near pid tilep (fn []
                                     (dosync
                                      (alter players update-in [pid] #(steal-player % (first selected) 1))
                                      (alter c-sites conj {:p tilep :t 0 :x (first selected)})))))))))

(defn enhance [player x]
  (condp = (or (keyword? x) (first x))
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
    player))

(defn update-player [pid f]
  (dosync (alter world-state update-in [:players pid] f)))

(defn listen [conn]
  (let [r (LineNumberingPushbackReader. (InputStreamReader. (.getInputStream conn)))
        eof (Object.)
        pid (@conns conn)]
    (loop []
      (if (let [m (read r false eof)]
            (if (= m eof)
              false
              (do 
                (condp = (first m)
                  :enhance
                  (update-player pid
                                 (fn [{:keys [inventar inventar-p] :as player}]
                                   (enhance player (nth inventar inventar-p))))
                  :decip
                  (update-player pid
                                 (fn [{:keys [inventar inventar-p] :as player}]
                                   (assoc player
                                     :inventar-p (if (= 0 inventar-p)
                                                   (dec (count inventar))
                                                   (dec inventar-p)))))
                  :incip
                  (update-player pid
                                 (fn [{:keys [inventar inventar-p] :as player}]
                                   (assoc player
                                     :inventar-p (if (= (dec (count inventar)) inventar-p)
                                                   0
                                                   (inc inventar-p)))))
                  :frame-count
                  (do
                    (update-player pid
                                   (fn [{:keys [pings] :as player}]
                                     (assoc player
                                       :pings (take 10 (conj pings (- @fr-counter (second m)))))))
                    (prn (nth (sort (get-in @world-state [:players pid :pings])) 5)))
                  :shot
                  (shot! pid (second m))
                  :walk
                  (walk! pid (second m)))
                true)))
        (recur)))))

(defn add-player! [pid]
  (alter world-state assoc-in [:players pid]
         {:p spawn-point
          :inventar [:gun :pickaxe]
          :inventar-p 0
          :path nil
          :name (str (rand-nth consonants) (rand-nth vowels) (rand-nth consonants) (rand-nth vowels))
          :died 0
          :pings (repeat 10 0)
          :hp 20}))

(defn accept []
  (loop []
    (let [conn (.accept socket)]
      (dosync
       (let [pid (inc (apply max 0 (keys @players)))]
         (alter conns assoc conn pid)
         (add-player! pid)
         (send-client conn (assoc @world-state
                             {:hello pid)))))
      (msg-pos)
      (.start (Thread. #(listen conn))))
    (recur)))

(defn -main []
  (dosync
   (doseq [p0 (range world-size)
           p1 (range world-size)
           :let [p [p0 p1]
                 delay (tile-delay p @world)]
           :when delay]
     (alter todo update-in [delay] conj p)))
  (def socket (ServerSocket. 8282))
  (.start (Thread. accept))
  (.start (Thread. steps)))

