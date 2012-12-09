(ns jardingaard.server
  (:use [jardingaard util shared]
        clojure.java.io)
  (:import [java.net ServerSocket Socket]
           [java.util Date]
           [java.io OutputStreamWriter InputStreamReader]
           [clojure.lang LineNumberingPushbackReader]))

(set! *warn-on-reflection* true)
(set! *assert* true)

(def socket)

(def world-size 100)

(def bullet-speed 1)

(def fr-counter (ref 0))

(def maxpid (ref 0))

(def conns (ref {}))

(def pingss (ref {}))

(def player-state (ref nil))

(def player-message-st (ref nil))

(def latest-player-message (ref {}))

(def world-state (ref nil))

(def save-path)

(def future-messages (ref {}))

(def messages (ref {}))

(defn log [m]
  (with-open [out (writer "log" :append true)]
    (.write out (str (Date.) " " m "\n"))))

(defn purge-conn [conn]
  (if (@conns conn)
    (log [:dissconect @conn]))
  (dosync
   (let [pid (@conns conn)]
     (alter pingss dissoc pid)
     (alter latest-player-message dissoc pid))
   (alter conns dissoc conn)))

(defn send-client [conn msg]
  (send conn (fn [^Socket sock]
               (try
                 (binding [*out* (OutputStreamWriter. (.getOutputStream sock))]
                   (prn msg))
                 (catch java.net.SocketException e
                   (purge-conn conn)))
               sock)))

(defn inform-cls [msg]
  (doseq [[conn _] @conns]
    (send-client conn msg)))

(defn half-median-ping [pid]
  (let [pings (@pingss pid)]
    (if (< 5 (count pings))
      (min 9 (int (/ (nth (sort pings) 5) 2)))
      0)))

(defn update-clients []
  (dosync
   (let [patch (gen-patch @player-state @world-state)
         m [:patch-state patch]]
     (ref-set player-state @world-state)
     (doseq [[conn pid] @conns]
       (send-client conn m)))))

(defn add-msg [delay msg]
   (alter (if (< delay 100)
            messages
            future-messages)
          update-in [(+ (first @world-state) delay)] conj msg))

(defn tiles-delay [ps]
  (doseq [p ps
          :let [delay (first (step-tile p (:world (second @world-state))))]
          :when delay]
    (add-msg delay [:tile p]))
  (doseq [p ps
          :when (= :tree (get-in @world-state (concat [1 :world] p)))]
    (doseq [delay [(rand-int 99999) (rand-int 99999)]]
      (add-msg delay
               [:sapling (plus p (minus [(rand-int 11) (rand-int 11)]
                                        [5 5]))]))))
  
(defn step! []
  (let [switch-t (+ (first @world-state) 100)]
    (alter messages update-in [switch-t] concat (@future-messages switch-t))
    (alter future-messages dissoc switch-t))
  (let [old-state @world-state]
    (alter world-state (fn [[c state]]
                         [(inc c)
                          (binding [*seed* c]
                            (step state (@messages c)))]))
    (tiles-delay (apply concat (for [[p0 p1s] (get-in (gen-patch old-state @world-state) [1 :world])
                                     [p1 _] p1s
                                     :let [p [p0 p1]]]
                                 (conj (ngbrs p (get-in @world-state [1 :world]))
                                       [p0 p1])))))
  (ref-set messages (into {} (filter #(<= (first @world-state)
                                          (first %))
                                     @messages)))
  (update-clients))

(defn steps []
  (loop []
    (let [start-t (.getTime (Date.))]
      (when (< 0 (count @conns))
        (dosync
         (alter fr-counter inc))
        (inform-cls [:frame-count @fr-counter]))
      (Thread/sleep (max 0 (- 33 (- (.getTime (Date.)) start-t)))))
    (recur)))

(defn update-messages! []
  (let [patch (gen-patch @player-message-st @messages)]
    (dosync
     (ref-set player-message-st @messages))
    (doseq [[conn pid] @conns]
      (send-client conn [:patch-messages patch (@latest-player-message pid)]))))

(defn add-player! [pid name]
  (if-let [oldpid (some (fn [[pid {n :name}]]
                          (if (= n name)
                            pid))
                        (get-in @world-state [1 :players]))]
    (alter world-state update-in [1 :players]
           #(dissoc (assoc % pid (% oldpid)) oldpid))
    (let [origin-t (first @world-state)]
      (alter messages update-in [origin-t] conj
             [:new-player pid
              {:inventar [[:gun 1] [:pickaxe 1]]
               :inventar-p 0
               :path nil
               :name name
               :died 0
               :hp 20}]))))

(defn listen [conn]
  (let [r (LineNumberingPushbackReader. (InputStreamReader. (.getInputStream ^Socket @conn)))
        eof (Object.)
        pid (@conns conn)]
    (loop []
      (if (let [m (try
                    (read r false eof)
                    (catch clojure.lang.LispReader$ReaderException e
                      (purge-conn conn)
                      eof))]
            (if (= m eof)
              false
              (dosync
               (condp = (first m)
                 :frame-count
                 (do (alter pingss update-in [pid]
                            (fn a [pings]
                              (take 10 (conj pings (- @fr-counter (second m))))))
                     (cond (< (+ (nth m 2) (half-median-ping pid)) @fr-counter)
                           (send-client conn [:skew :plus])
                           (> (+ (nth m 2) (half-median-ping pid)) @fr-counter)
                           (send-client conn [:skew :minus])))
                 :save (spit save-path (second @world-state))
                 :name (do (add-player! pid (second m))
                           (log [:name (second m) @conn]))
                 :cmds
                 (let [[_ origin-t cmds] m]
                   (alter latest-player-message assoc pid origin-t)
                   (alter messages update-in [origin-t] concat cmds)
                   (loop []
                     (when (< (first @world-state) (apply min (vals @latest-player-message)))
                       (step!)
                       (recur)))
                   (update-messages!)))
               true)))
        (recur)))))

(defn accept []
  (loop []
    (let [conn (loud-agent (doto (.accept socket)
                             (.setTcpNoDelay true)))]
      (log [:connect @conn])
      (dosync
       (let [pid (alter maxpid inc)]
         (alter conns assoc conn pid)
         (alter pingss assoc pid (repeat 10 0))
         (alter latest-player-message assoc pid @fr-counter)
         (send-client conn [:hello pid])
         (send-client conn [:patch-state (gen-patch nil @player-state)])
         (send-client conn [:patch-messages (gen-patch nil @player-message-st) @fr-counter])))
      (.start (Thread. #(listen conn))))
    (recur)))

(defn -main [& [sp]]
  (dosync
   (def save-path sp)
   (ref-set world-state [0 (or (if save-path
                                 (try
                                   (read-string (slurp save-path))
                                   (catch java.io.FileNotFoundException e)))
                               (new-world world-size bullet-speed))])
   (alter maxpid #(apply max % (keys (get-in @world-state [1 :players]))))
   (tiles-delay (for [p0 (range world-size)
                      p1 (range world-size)]
                  [p0 p1])))
  (def socket (ServerSocket. 8282))
  (forkIO accept)
  (forkIO steps))

