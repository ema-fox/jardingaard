(ns jardingaard.rules)

(def world-size (* 32 10))

(def num-zombies 0)
(def num-bunnies 2)

(def human-walk-speeds {:dirt 1
                        :door 1
                        :grass 1.1
                        :water 1.7
                        :sand 1.3
                        :tall-grass 1.3})

(def zombie-walk-speeds (into {} (map (fn [[k v]]
                                        [k (if (= k :door)
                                             (* 16 v)
                                             (* 4 v))])
                                      human-walk-speeds)))

(def placable [:wall :windowed-wall :door :rock])

(def places [:dirt :grass :tall-grass])

(def interactions (into {[:hands :shrub] {:give {:twig 3}
                                          :tile nil}
                         [:hands :rock] {:give {:stone 1}
                                         :tile :rock}
                         [:pickaxe :shrub] {:give {:twig 4}
                                            :tile nil}
                         [:pickaxe :tree] {:give {:twig 23
                                                  :trunk 1}
                                           :tile nil}}
                        (apply concat (for [pla placable]
                                        [[[pla nil] {:take {pla 1}
                                                     :tile pla}]
                                         [[:pickaxe pla] {:give {pla 1}
                                                          :tile nil}]]))))


(def recipes {[:windowed-wall] {:wall 1
                                :twig 5}
              [:door] {:twig 13}
              [:pickaxe] {:stone 2
                          :twig 3}
              [:spear] {:stone 1
                        :twig 2}
              [:wall] {:rock 1}
              [:fur :steak :thread] {:bunny 1}
              [:gun] {:twig 3
                      :thread 2}})
