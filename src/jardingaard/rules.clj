(ns jardingaard.rules
  (:use [jardingaard util]))

(defn seconds [x]
  (* x 60))

(defn minutes [x]
  (* (seconds x) 60))

(def world-size (* 32 1))

(def arrow-speed 0.1)

(def num-zombies 10)
(def num-bunnies 2)

(def human-walk-speeds {:dirt 1
                        :door 1
                        :grass 1.1
                        :water 1.7
                        :sand 1.3
                        :granite-floor 1
                        :tall-grass 1.3})

(def zombie-walk-speeds (map-kv (fn [_ v]
                                  (* v 2))
                                human-walk-speeds))

(def placable #{:wall :windowed-wall :door :rock :chest :tree :lumberjack :idol})

(def grounds #{:dirt :water :grass :tall-grass})

(def work-times {:tree (seconds 9)
                 :idol (minutes 0.5)
                 :lumberjack (seconds 2)
                 :lumberjack-being (seconds 0.5)})

(def object-fruits {:tree :wood})

(def *broken* {:lumberjack 5
               :idol 13})

;(def places [:dirt :grass :tall-grass])

(def interactions (into {[:hands :shrub] {:give {:twig 3}
                                          :tile nil}
                         [:hands :shrub-pear] {:give {:pear 13}
                                               :tile :shrub}
                         [:hands :rock] {:give {:stone 1}
                                         :tile :rock}
                         [:stone nil] {:take {:stone 9}
                                       :tile :campfire-empty}
                         [:twig :campfire-empty] {:take {:twig 7}
                                                   :tile :campfire-off}
                         [:stone :campfire-off] {:tile :campfire-on}
                         [:steak :campfire-on] {:take {:steak 1}
                                                :give {:steak-fried 1}
                                                :tile :campfire-on}
                         [:pickaxe :granite] {:give {:rock 2}
                                              :tile nil}
                         [:pickaxe :shrub] {:give {:twig 4}
                                            :tile nil}
                         [:pickaxe :shrub-pear] {:give {:twig 4}
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
              [:tree] {:gold 3}
              [:lumberjack] {:gold 5
                             :wood 3}
              [:idol] {:gold 12 :wood 38}
              [:water] {:gold 23}
              [:dirt] {:gold 1}
              [:grass] {:dirt 1 :gold 3}
              [:tall-grass] {:grass 1 :gold 4}
              [:door] {:twig 13}
              [:chest] {:trunk 1}
              [:pickaxe] {:stone 2
                          :twig 3}
              [:axe] {:stone 3
                      :twig 2}
              [:spear] {:stone 1
                        :twig 2}
              [:wall] {:rock 1}
              [:fur :steak :thread] {:bunny 1}
              [:gun] {:twig 3
                      :thread 2}})
