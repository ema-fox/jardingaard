(ns jardingaard.server
  (:use [jardingaard util shared reducers rules helpers backend]
        clojure.java.io)
  (:import [java.net ServerSocket Socket]
           [java.util Date]
           [java.io OutputStreamWriter InputStreamReader BufferedWriter BufferedReader]
           [clojure.lang LineNumberingPushbackReader]))

(set! *warn-on-reflection* true)
(set! *assert* true)

(def socket)

(def conns (ref {}))

(def pingss (ref {}))

(def player-states (ref {}))

(def player-message-st (ref nil))

(def latest-player-message (ref {}))

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
                 (binding [*out* (BufferedWriter. (OutputStreamWriter. (.getOutputStream sock)))]
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

(defn state-for [[c {:keys [bworld mworld players bunnies] :as state}] pid]
  (let [p (get-in players [pid :p])]
    [c (assoc-seq state
                  :bunnies
                  :bworld
                  :mworld
                  (if p [(vec (take 50 (filter (fn a [{pb :p}]
                                                 (< (manhatten pb p) 50))
                                               bunnies)))
                         (let [[p0 p1] (round p)]
                           (select-keys bworld (for [b0 [-32 0 32]
                                                     b1 [-32 0 32]]
                                                 (plus [b0 b1] [(bit-and high-mask p0)
                                                                (bit-and high-mask p1)]))))
                         (let [[p0 p1] (round p)]
                           (select-keys mworld (for [b0 [-32 0 32]
                                                     b1 [-32 0 32]]
                                                 (plus [b0 b1] [(bit-and high-mask p0)
                                                                (bit-and high-mask p1)]))))]
                      [nil nil nil]))]))

(defn update-clients []
  (doseq [[conn pid] @conns]
    (let [pstate (state-for @state pid)
          patch [:patch-state (gen-patch (@player-states pid) pstate)]]
      (alter player-states assoc pid pstate)
      (send-client conn patch))))

(defn steps []
  (loop []
    (let [start-t (.getTime (Date.))]
      (when (< 0 (count @conns))
        (dosync
         (alter fr-counter inc))
        (inform-cls [:frame-count @fr-counter]))
      (Thread/sleep (max 0 (- tick-duration (- (.getTime (Date.)) start-t)))))
    (recur)))

(defn update-messages! []
  (let [patch (gen-patch @player-message-st @messages)]
    (ref-set player-message-st @messages)
    (doseq [[conn pid] @conns]
      (send-client conn [:patch-messages patch (@latest-player-message pid)]))))

(defn process-msg [m pid conn]
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
     :save (spit save-path (second @state))
     :name (do (add-player! pid (second m))
               (log [:name (second m) @conn]))
     :cmds
     (let [[_ origin-t cmds] m]
       (alter latest-player-message assoc pid origin-t)
       (alter messages update-in [origin-t] concat cmds)
       (loop []
         (when (< (first @state) (apply min (vals @latest-player-message)))
           (step!)
           (recur)))
       (update-clients)
       (update-messages!)))))
  
(defn listen [conn]
  (let [r (LineNumberingPushbackReader. (BufferedReader. (InputStreamReader. (.getInputStream ^Socket @conn))))
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
              (do (process-msg m pid conn)
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
         ;(send-client conn [:patch-state (gen-patch nil @player-state)])
         (send-client conn [:patch-messages (gen-patch nil @player-message-st) @fr-counter])))
      (.start (Thread. #(listen conn))))
    (recur)))


(defn -main [& [sp]]
  (load-world! sp)
  (def socket (ServerSocket. 8282))
  (forkIO accept)
  (forkIO steps))
