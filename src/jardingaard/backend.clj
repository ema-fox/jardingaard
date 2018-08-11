(ns jardingaard.backend
  (:refer-clojure :exclude [read read-string])
  (:use [clojure.java io]
        clojure.edn
        [jardingaard worldgen shared helpers util rules field]))

(def state (ref nil))

(def future-messages (ref {}))

(def messages (ref {}))

(def fr-counter (ref 0))

(defn add-msg [delay msg]
   (alter (if (< delay 100)
            messages
            future-messages)
          update-in [(+ (first @state) delay)] conj msg))

(defn step! []
  (let [switch-t (+ (first @state) 100)]
    (alter messages update-in [switch-t] concat (@future-messages switch-t))
    (alter future-messages dissoc switch-t))
  (alter state (fn [[c state]]
                 [(inc c)
                  (binding [*tick* c]
                    (step state (@messages c)))]))
  (ref-set messages (into {} (filter #(<= (first @state)
                                          (first %))
                                     @messages))))

(defn player-id [player-name]
  (some (fn [{n :name i :i}]
          (if (= n player-name)
            i))
        (get-in @state [1 :players])))

(defn ensure-player! [player-name]
  (or (player-id player-name)
      (do (alter state update-in [1 :players] conj
                 (assoc +new-player+ :name player-name))
          (player-id player-name))))

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
                      [0 (new-world world-size)]))
   (println " done.")))
