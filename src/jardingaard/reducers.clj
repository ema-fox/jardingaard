(ns jardingaard.reducers
  (:use [clojure.core.protocols]))

(defn rrange [start end]
  (reify
    CollReduce
    (coll-reduce [_ f val] (loop [i start
                                  res val]
                             (if (< i end)
                               (recur (inc i) (f res i))
                               res)))))

(defn product [a b]
  (reify
    CollReduce
    (coll-reduce [_ f val]
      (coll-reduce a (fn [res v]
                       (coll-reduce b (fn [res2 v2]
                                        (f res2 [v v2]))
                                    res))
                   val))))