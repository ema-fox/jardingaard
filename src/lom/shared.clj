(ns lom.shared
  (:use [lom util]))

(defn gen-chunk [[p0 p1 :as p] [s0 s1 :as s] bla]
  (if (= 0 (apply * (map dec s)))
    (into {} (for [b0 (range s0)
                     b1 (range s1)]
               [(plus p [b0 b1]) (rand-nth bla)]))
    (let [nbla (conj bla (rand-nth (concat (repeat 3 :wall)
                                           (repeat 30 :gras)
                                           (repeat 10 :dirt)
                                           (repeat 1 :shrub))))
          [d0 d1 :as d] (map #(int (/ % 2)) s)]
      (merge (gen-chunk p d nbla)
             (gen-chunk [(+ p0 d0) p1] [(- s0 d0) d1] nbla)
             (gen-chunk [p0 (+ p1 d1)] [s0 (- s1 d1)] nbla)
             (gen-chunk (plus p d) (minus s d) nbla)))))

(defn gen-world [world-size]
  (let [bla (gen-chunk [0 0] [world-size world-size] [])]
  (mapv (fn [p0]
          (mapv (fn [p1]
                  (bla [p0 p1]))
                (range world-size)))
        (range world-size))))

(defn new-world [world-size bullet-speed]
  {:players {}
   :bullets []
   :c-sites []
   :world (gen-world world-size)
   :bullet-speed bullet-speed
   :spawn-point [(/ world-size 2) (/ world-size 2)]})

(defn line-affects [start goal]
  (let [goal (if (== (first goal) (first start))
               (plus goal [0.0001 0])
               goal)
        [d0 d1] (minus goal start)
        slope (/ d1 d0)
        [a b] (sort [(first start) (first goal)])]
    (->> (concat [a]
                 (range (+ 0.5 (int (+ 0.5 a)))
                        (+ 0.5 (int (+ 0.5 b))))
                 [b])
         (partition 2 1)
         (map (fn [[a b]]
                [(int (/ (+ a b 1) 2))
                 (int (+ (second start) (* slope (- a (first start))) 0.5))
                 (int (+ (second start) (* slope (- b (first start))) 0.5))]))
         (mapcat (fn [[x ya yb]]
                   (map (fn [y]
                          [x y])
                        (range (min ya yb) (inc (max ya yb)))))))))

(defn test-line [start goal w]
  (every? #(#{:dirt :gras :door} (get-in w %)) (line-affects start goal)))

(defn smooth-path [start path w]
  (if path
    (loop [i (min 10 (dec (count path)))]
      (let [subgoal (nth path i)]
        (if (or (every? #(test-line (plus start %) (plus subgoal %) w)
                        [[0.4 0.4]
                         [-0.4 0.4]
                         [-0.4 -0.4]
                         [0.4 -0.4]])
                (= i 0))
          (drop i path)
          (recur (dec i)))))))

(defn step-bullets&players [{:keys [bullets players bullet-speed spawn-point world] :as state}]
  (let [[bus pls] (ttmap (fn [bullets [pid {:keys [p hp goal died path do-at] :as player}]]
                           (let [newbullets
                                 (filter (fn [{bp :p m :m}]
                                           (> (distance (p-on-line bp
                                                                   (plus bp (mult m bullet-speed))
                                                                   p)
                                                        p)
                                              0.5))
                                         bullets)
                                 newhp (- hp (- (count bullets) (count newbullets)))]
                             [newbullets
                              [pid
                               (if (< newhp 1)
                                 (assoc player
                                   :hp 20
                                   :p (plus spawn-point [(rand-int 5) (rand-int 5)])
                                   :path nil
                                   :died (inc died))
                                 (let [npath (smooth-path p path world)]
                                   (assoc (if (first npath)
                                            (assoc player
                                              :p (plus p (mult (direction p (first npath))
                                                               (min 0.25 (distance p (first npath))))))
                                            player)
                                     :do-at (if (and do-at
                                                     (< (distance (first do-at) p) 0.3))
                                              (do
                                                ((second do-at))
                                                nil)
                                              do-at)
                                     :hp newhp
                                     :path npath)))]]))
                         (->> bullets
                              (map (fn [{:keys [p ttl m] :as b}]
                                     (let [newp (plus p (mult m bullet-speed))]
                                       (assoc b
                                         :p newp
                                         :ttl (if (some #((if (> ttl 497)
                                                            #{:wall :door}
                                                            #{:wall :windowed-wall :door})
                                                          (get-in world %))
                                                        (line-affects p newp))
                                                0
                                                (dec ttl))))))
                              (filter #(< 0 (:ttl %))))
                         players)]
    (assoc state
      :bullets (vec bus)
      :players (into {} pls))))

