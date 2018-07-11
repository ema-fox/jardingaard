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

(defn quux [x]
  (- (* 3 (Math/pow x 2)) (* 2 (Math/pow x 3))))

(defn gen-world3 [world-size]
  (let [mchunk (mapv (fn [_] (mapv (fn [_] nil) (range 32)))
                     (range 32))
        empty-world (into {} (for [p0 (range 0 world-size 32)
                                   p1 (range 0 world-size 32)]
                               [[p0 p1] mchunk]))]
    [empty-world empty-world]))

(defn gen-world2 [world-size]
  (let [feature-size 24
        mchunk (mapv (fn [_] (vec (range 32)))
                     (range 32))
        empty-world (into {} (for [p0 (range 0 world-size 32)
                                   p1 (range 0 world-size 32)]
                               [[p0 p1] mchunk]))
        foo (vec (for [i0 (range 0 (+ world-size feature-size) feature-size)]
                   (vec (for [i1 (range 0 (+ world-size feature-size) feature-size)]
                          [(quux (quux (rand))) (rand-int 2)]))))]
    (reduce (fn [[bworld mworld] p]
              (let [fp (map int (div p feature-size))
                    [bla0 bla1] (div (map #(rem % feature-size) p) feature-size)
                    bar (reduce plus (map (fn [[weight i]]
                                            (mult (get-in foo i) weight))
                                          [[(* bla0 bla1) (plus fp [1 1])]
                                           [(* (- 1 bla0) bla1) (plus fp [0 1])]
                                           [(* bla0 (- 1 bla1)) (plus fp [1 0])]
                                           [(* (- 1 bla0) (- 1 bla1)) (plus fp [0 0])]]))
                    [b m] (tile-type (map #(min 1 (max 0 (+ % (rand 0.1) -0.05))) bar))]
                [(assoc-in-map bworld p b)
                 (assoc-in-map mworld p m)]))
            [empty-world empty-world]
            (product (rrange world-size)
                     (rrange world-size)))))

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
                      :chests {}                      :bullet-speed bullet-speed
                      :spawn-point [(/ world-size 2) (/ world-size 2)]}
                     :bworld :mworld
                     (gen-world3 world-size))
          @gen-fns))
