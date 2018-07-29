(ns jardingaard.server
  (:refer-clojure :exclude [read read-string])
  (:use [jardingaard util shared reducers rules helpers backend]
        clojure.edn
        clojure.java.io)
  (:import [java.net ServerSocket Socket]
           [java.io OutputStreamWriter BufferedWriter]
           [java.util Date]))

(set! *warn-on-reflection* true)
(set! *assert* true)

(def socket)

(def conns (ref {}))

(def pingss (ref {}))

(def player-states (ref {}))

(def player-message-st (ref nil))

(def latest-player-message (ref {}))

(defn log [m]
  (prn m)
  (with-open [out (writer "log" :append true)]
    (.write out (str (Date.) " " m "\n"))))

(defn purge-conn [conn]
  (if (@conns conn)
    (log [:dissconect @conn]))
  (dosync
   (let [pid (@conns conn)]
     (alter pingss dissoc pid)
     (alter latest-player-message dissoc pid)
     (alter player-states dissoc pid)
     (alter player-message-st dissoc pid))
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
    [c state]))

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
     :save (spit save-path @state)
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

(defn listen [conn msgs]
  (let [pid (@conns conn)]
    (doseq [m msgs]
      (process-msg m pid conn)))
  (purge-conn conn))

(defn ensure-player! [player-name]
  (or (some (fn [[pid {n :name}]]
              (if (= n player-name)
                pid))
            (get-in @state [1 :players]))
      (let [pid (alter maxpid inc)]
        (add-player! pid player-name)
        pid)))

(defn listen-for-name [conn]
  (let [msgs (reads-socket @conn)
        m (first msgs)]
    (if (= (first m) :name)
      (do
        (dosync
         (let [pid (ensure-player! (second m))]
           (alter conns assoc conn pid)
           (alter pingss assoc pid (repeat 10 0))
           (alter latest-player-message assoc pid @fr-counter)
           (send-client conn [:hello pid])
           (send-client conn [:patch-messages (gen-patch nil @player-message-st) @fr-counter])))
        (listen conn (rest msgs)))
      (log [:wrong-opening-msg m @conn]))))

(defn accept []
  (loop []
    (let [conn (loud-agent (doto (.accept socket)
                             (.setTcpNoDelay true)))]
      (log [:connect @conn])
      (.start (Thread. #(listen-for-name conn))))
    (recur)))


(defn -main [& [sp]]
  (load-world! sp)
  (def socket (ServerSocket. 8282))
  (forkIO accept)
  (forkIO steps))
