(ns jardingaard.standalone
  (:use [jardingaard gui backend shared rules])
  (:import [java.util Date]))

(defn current-state2 []
  (assoc (second @state)
    :tick (first @state)))

(defn add-plcmd2 [m]
  (dosync
   (add-msg 0 [:plcmd @hello m])))

(defn possible-recipes2 []
  (keep (fn [[x r]]
          (if (every? (fn [[y n]]
                        (player-has? (get-in @state [1 :players @hello]) y n))
                      r)
            x))
        recipes))

(config-gui current-state2 add-plcmd2 possible-recipes2
            (fn []
              (spit save-path @state)
              (prn "saved")))

(defn steps [can]
  (loop []
    (let [start-t (.getTime (Date.))]
      (dosync
       (alter fr-counter inc)
       (step!))
      (.display can)
      (Thread/sleep (max 0 (- tick-duration (- (.getTime (Date.)) start-t)))))
    (recur)))

(defn -main [& [sp]]
  (load-world! sp)
  (if (= 0 @maxpid)
    (let [pid (inc @maxpid)]
      (dosync
       (add-player! pid "foo")
       (ref-set hello pid)))
    (dosync
     (ref-set hello @maxpid)))
  (let [can (create-gui)]
    (steps can)))
