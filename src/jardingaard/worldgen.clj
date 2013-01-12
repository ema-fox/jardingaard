(ns jardingaard.worldgen
  (:use [jardingaard.shared]))

(defn tile-type [xs] ;[height vegetation]
  (let [ys (map #(int (* 5 %)) xs)]
    (get-in [[:water :water      :water      :water      :water]
             [:sand  :sand       :dirt       :grass      :grass]
             [:dirt  :grass      :grass      :tall-grass :tall-grass]
             [:wall  :tall-grass :tall-grass :shrub      :shrub]
             [:wall  :wall       :dirt       :tree       :tree]]
            ys)))

(defn gen-world [world-size]
  (let [mchunk (mapv (fn [_] (vec (range 32)))
                     (range 32))]
    (loop [acc (into {} (for [p0 (range 0 world-size 32)
                              p1 (range 0 world-size 32)]
                          [[p0 p1] mchunk]))
           p0 0
           prevline (repeat world-size [0.5 0.5])]
      (when (= 0 (mod p0 10))
        (print ".")
        (flush))
      (if (< p0 world-size)
        (let [[acc prevline] (loop [acc acc
                                    p1 0
                                    curline [[0.5 0.5]]]
                               (if (< p1 world-size)
                                 (let [foo (map (fn [a b]
                                                  (max 0 (min 0.999 (+ (/ (+ a b) 2)
                                                                   (rand 0.1)
                                                                   -0.05))))
                                                (peek curline)
                                                (nth prevline (min (inc p1)
                                                                   (dec (count prevline)))))]
                                   (recur (assoc-in-map acc [p0 p1] (tile-type foo))
                                          (inc p1)
                                          (conj curline foo)))
                                 [acc (rest curline)]))]
          (recur acc (inc p0) prevline))
        acc))))

(defn new-world [world-size bullet-speed]
  {:players {}
   :bullets []
   :c-sites []
   :zombies []
   :bunnies []
   :deadbunnies []
   :world (gen-world world-size)
   :bullet-speed bullet-speed
   :spawn-point [(/ world-size 2) (/ world-size 2)]})
