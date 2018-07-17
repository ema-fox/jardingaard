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

(defn new-world [world-size bullet-speed]
  (reduce (fn [st f]
            (f st))
          {:players {}
           :bullets []
           :c-sites []
           :bunnies []
           :deadbunnies []
           :chests {}                      :bullet-speed bullet-speed
           :spawn-point [(/ world-size 2) (/ world-size 2)]
           :world {}}
          @gen-fns))
