(ns jardingaard.backend
  (:refer-clojure :exclude [read read-string])
  (:use [clojure.java io]
        clojure.edn
        [jardingaard shared helpers util rules field]))

(def state (ref nil))

(def future-messages (ref {}))

(def messages (ref {}))

(def fr-counter (ref 0))

(defn add-msg [delay msg]
   (alter (if (< delay 100)
            messages
            future-messages)
          update-in [(+ (:tick @state) delay)] conj msg))

(defn step! []
  (let [switch-t (+ (:tick @state) 100)]
    (alter messages update-in [switch-t] concat (@future-messages switch-t))
    (alter future-messages dissoc switch-t))
  (alter state (fn [state]
                 (binding [*tick* (:tick state)]
                   (-> (step state (@messages *tick*))
                       (update :tick inc)))))
  (ref-set messages (into {} (filter #(<= (:tick @state)
                                          (first %))
                                     @messages))))

(defn player-id [player-name]
  (some (fn [{n :name i :i}]
          (if (= n player-name)
            i))
        (:players @state)))

(defn ensure-player! [player-name]
  (or (player-id player-name)
      (do (alter state update :players conj
                 (assoc +new-player+ :name player-name))
          (player-id player-name))))

(def +intitial-state+ {:tick 0
                       :players (field)
                       :lumberjacks (field)
                       :carpenters (field)
                       :zombies (field)
                       :arrows (field)
                       :buildings {}
                       :world {}})

(defn load-world! [sp]
  (def save-path sp)
  (dosync
   (print "generating world...")
   (flush)
   (ref-set state (or (if save-path
                        (try
                          (read-string {:readers {'jardingaard.field field}}
                                       (slurp save-path))
                          (catch java.io.FileNotFoundException e)))
                      +intitial-state+))
   (println " done.")))
