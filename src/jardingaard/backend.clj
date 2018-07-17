(ns jardingaard.backend
  (:refer-clojure :exclude [read read-string])
  (:use [clojure.java io]
        clojure.edn
        [jardingaard worldgen shared helpers util rules]))

(def maxpid (ref 0))

(def state (ref nil))

(def future-messages (ref {}))

(def messages (ref {}))

(def fr-counter (ref 0))

(defn add-msg [delay msg]
   (alter (if (< delay 100)
            messages
            future-messages)
          update-in [(+ (first @state) delay)] conj msg))

(defn tiles-delay [ps]
  #_(let [st (second @state)]
    (doseq [p ps
            :let [delay (first (step-tile p (:bworld st) (:mworld st)))]
            :when delay]
      (add-msg delay [:tile p])))
  #_(doseq [p ps
          :when (= :tree (get-in-map (get-in @state [1 :world]) p))]
    (doseq [delay [(rand-int 99999) (rand-int 99999)]]
      (add-msg delay
               [:sapling (plus p (minus [(rand-int 11) (rand-int 11)]
                                        [5 5]))
                p]))))

(def bunny-stats (writer (file "bunny-stats")))
(def tiles-stats (writer (file "tiles-stats")))

#_(defn ensure-bunnies [{:keys [bunnies] :as state}]
  (if (and (< (count bunnies) num-bunnies)
           (= 0 (mod (prng *tick* 565) 9)))
    (assoc state
      :bunnies (conj bunnies {:p (rand-spawnpoint state)
                              :energy 5000
                              :seed *tick*}))
    state))

(defn step! []
  (if (= 0 (mod (first @state) 300))
    (binding [*out* bunny-stats]
      (println (apply + (map :energy (get-in @state [1 :bunnies]))))))
  (let [switch-t (+ (first @state) 100)]
    (alter messages update-in [switch-t] concat (@future-messages switch-t))
    (alter future-messages dissoc switch-t))
  (let [old-state @state]
    (alter state (fn [[c state]]
                   [(inc c)
                    (binding [*tick* c]
                      (step state (@messages c)))]))
    (let [changed-tiles (apply concat (for [[chunkp offsets] (mapcat #(gen-patch (get-in old-state [1 %])
                                                                                 (get-in @state [1 %]))
                                                                     [:bworld :mworld])
                                            [o0 o1s] offsets
                                            [o1 _] o1s
                                            :let [p (plus chunkp [o0 o1])]]
                                        (conj (ngbrs p (get-in @state [1 :bworld])) p)))]
      (tiles-delay changed-tiles)
      (if (= 0 (mod (first @state) 300))
        (binding [*out* tiles-stats]
          (println (first (reduce (fn [[counter foo] [t p]]
                                    (if (= t :tile)
                                      (if (get foo p)
                                        [(inc counter) foo]
                                        [counter (conj foo p)])
                                      [counter foo]))
                                  [0 #{}]
                                  (mapcat val (concat @messages @future-messages)))))))))
  (ref-set messages (into {} (filter #(<= (first @state)
                                          (first %))
                                     @messages))))

(defn add-player! [pid name]
  (if-let [oldpid (some (fn [[pid {n :name}]]
                          (if (= n name)
                            pid))
                        (get-in @state [1 :players]))]
    (alter state update-in [1 :players]
           #(dissoc (assoc % pid (% oldpid)) oldpid))
    (let [origin-t (first @state)]
      (alter messages update-in [origin-t] conj
             [:new-player pid
              {:inventar [[:hands 1] [:pickaxe 1]]
               :inventar-p 0
               :inventar-category-p :inventar
               :path nil
               :name name
               :died 0
               :gold-spawn 0
               :energy 200
               :hp 200}]))))

(defn load-world! [sp]
  (def save-path sp)
  (dosync
   (print "generating world...")
   (flush)
   (ref-set state (or (if save-path
                        (try
                          (read-string (slurp save-path))
                          (catch java.io.FileNotFoundException e)))
                      [0 (new-world world-size bullet-speed)]))
   (println " done.")
   (alter maxpid #(apply max % (keys (get-in @state [1 :players]))))
   (print "predicting changes...")
   (flush)
   (tiles-delay (for [p0 (range world-size)
                      p1 (range world-size)]
                  [p0 p1]))
   (println " done.")))
