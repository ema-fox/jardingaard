(ns jardingaard.worldgen
  (:use [jardingaard helpers util]))

(defn tile-type [xs] ;[height vegetation]
  (let [ys (map #(int (* 5 %)) xs)]
    [(get-in [[:water :water :water      :water      :water]
              [:sand  :dirt  :grass      :tall-grass :dirt]
              [:dirt  :grass :tall-grass :tall-grass :dirt]
              [:granite-floor :dirt          :grass  :tall-grass :dirt]
              [:granite-floor :granite-floor :granite-floor :granite-floor :granite-floor]]
             ys)
     (get-in [[nil      :rock    nil      nil         nil]
              [nil      nil      nil      :shrub-pear :tree]
              [nil      nil      nil      :shrub      :tree]
              [:rock    :rock    nil      :rock       :rock]
              [:granite :granite :granite :granite    :granite]]
             ys)]))

(defn gen-world [world-size]
  (let [mchunk (mapv (fn [_] (vec (range 32)))
                     (range 32))]
    (loop [bacc (into {} (for [p0 (range 0 world-size 32)
                              p1 (range 0 world-size 32)]
                           [[p0 p1] mchunk]))
           macc bacc
           p0 0
           prevline (repeat world-size [0.5 0.5])]
      (when (= 0 (mod p0 10))
        (print ".")
        (flush))
      (if (< p0 world-size)
        (let [[bacc macc prevline] (loop [bacc bacc
                                          macc macc
                                          p1 0
                                          curline [[0.5 0.5]]]
                                     (if (< p1 world-size)
                                       (let [foo (map (fn [a b]
                                                        (max 0 (min 0.999 (+ (/ (+ a b) 2)
                                                                             (rand 0.1)
                                                                             -0.05))))
                                                      (peek curline)
                                                      (nth prevline (min (inc p1)
                                                                         (dec (count prevline)))))
                                             [b m] (tile-type foo)]
                                         (recur (assoc-in-map bacc [p0 p1] b)
                                                (assoc-in-map macc [p0 p1] m)
                                                (inc p1)
                                                (conj curline foo)))
                                       [bacc macc (rest curline)]))]
          (recur bacc macc (inc p0) prevline))
        [bacc macc]))))

(defn new-world [world-size bullet-speed]
  (reduce (fn [st f]
            (f st))
          (assoc-seq {:players {}
                      :bullets []
                      :c-sites []
                      :bunnies []
                      :deadbunnies []
                      :bullet-speed bullet-speed
                      :spawn-point [(/ world-size 2) (/ world-size 2)]}
                     :bworld :mworld
                     (gen-world world-size))
          @gen-fns))
