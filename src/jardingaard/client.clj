(ns jardingaard.client
  (:gen-class)
  (:refer-clojure :exclude [read read-string])
  (:use clojure.edn
        [seesaw core]
        [jardingaard util shared reducers rules helpers gui])
  (:import [java.net Socket]
           [java.io OutputStreamWriter InputStreamReader BufferedWriter BufferedReader]
           [clojure.lang LineNumberingPushbackReader]))

(set! *warn-on-reflection* true)

(def ^Socket conn)

(def messenger (loud-agent nil))

(def state (ref nil))

(def srv-messages (ref nil))

(def cl-messages (ref {}))

(def skew (ref nil))

(def fr-counter (loud-agent nil))

(defn count-inc [c]
  (Thread/sleep (let [t (+ tick-duration
                           (condp = @skew
                             :plus
                             -20
                             :minus
                             20
                             0))]
                  (dosync (ref-set skew nil))
                  t))
  (send-off fr-counter count-inc)
  (inc c))

(defn step-to [[start st] end msgs]
  (reduce (fn [sta n]
            (binding [*seed* n]
              (step sta (msgs n))))
          st
          (range start end)))

(defn current-state2 []
  (if (and (get-in @state [1 :players @hello])
           (< (- @fr-counter (first @state)) 40))
    (step-to @state @fr-counter (merge-with concat @srv-messages @cl-messages))))

(defn set-dbg-info []
  (swap! dbg-info (constantly (str @fr-counter
                                   " "
                                   (if @state
                                     (- @fr-counter (first @state))
                                     "...")))))

(defn possible-recipes2 []
  (keep (fn [[x r]]
          (if (every? (fn [[y n]]
                        (player-has? (get-in @state [1 :players @hello]) y n))
                      r)
            x))
        recipes))

(defn connection-lost! []
  (show! (pack! (dialog :content "connection lost" :type :warning)))
  (System/exit 1))

(defn msg [m]
  (send-off messenger (fn [a]
                        (try 
                          (binding [*out* (BufferedWriter. (OutputStreamWriter. (.getOutputStream conn)))]
                            (prn m))
                          (catch java.net.SocketException e)))))

(add-watch fr-counter :send-msgs (fn [_ _ n nn]
                                   (msg [:cmds (or n nn) (@cl-messages n)])))

(defn add-msg [m]
  (dosync
   (alter cl-messages update-in [@fr-counter] conj [:plcmd @hello m])))

(config-gui current-state2 add-msg possible-recipes2 #(msg [:save]))

(defn handle-msg [d]
  (dosync
   (cond (vector? d)
         (condp = (first d)
           :patch-state (alter state apply-patch (second d))
           :patch-messages (do (alter srv-messages apply-patch (second d))
                               (alter cl-messages dissoc (nth d 2)))
           :hello (ref-set hello (second d))
           :skew (ref-set skew (second d))
           :frame-count (do (when-not @fr-counter
                              (send fr-counter (constantly (second d)))
                              (send-off fr-counter count-inc))
                            (msg (conj d (or @fr-counter (second d)))))))))

(defn handle-msgs []
  (dorun (map handle-msg (reads-socket conn)))
  (connection-lost!))
  
(defn -main [& [^String host]]
  (def conn (loop []
              (if-let [c (try
                           (doto (Socket. (or host "soupwhale.com") 8282)
                             (.setTcpNoDelay true))
                           (catch java.net.ConnectException e
                             (Thread/sleep 1000)
                             nil))]
                c
                (recur))))
  (forkIO #(msg [:name (show! (pack! (dialog :content
                                             (flow-panel :items ["Give yourself a name"
                                                                 (text :id :name :columns 10)])
                                             :type :question
                                             :option-type :ok-cancel
                                             :success-fn (fn [p]
                                                           (text (select (to-root p) [:#name]))))))]))
  (let [can (create-gui)
        drawer (loud-agent nil)]
    (add-watch fr-counter :paint (fn [_ _ _ _]
                                   (send-off drawer (fn [_]
                                                      (set-dbg-info)
                                                      (.display can))))))
  (handle-msgs))
