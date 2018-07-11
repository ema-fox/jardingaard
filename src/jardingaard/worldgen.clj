(ns jardingaard.worldgen
  (:use [jardingaard helpers reducers util]))

(defn tile-type [xs] ;[height vegetation]
  (let [ys (map #(int (* 4.99 %)) xs)]
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

(defn gen-world3 [world-size]
  (let [mchunk (mapv (fn [_] (mapv (fn [_] nil) (range 32)))
                     (range 32))
        empty-world (into {} (for [p0 (range 0 world-size 32)
                                   p1 (range 0 world-size 32)]
                               [[p0 p1] mchunk]))]
    [empty-world empty-world]))

(defn new-world [world-size bullet-speed]
  (reduce (fn [st f]
            (f st))
          (assoc-seq {:players {}
                      :bullets []
                      :c-sites []
                      :bunnies []
                      :deadbunnies []
                      :chests {}                      :bullet-speed bullet-speed
                      :spawn-point [(/ world-size 2) (/ world-size 2)]}
                     :bworld :mworld
                     (gen-world3 world-size))
          @gen-fns))
