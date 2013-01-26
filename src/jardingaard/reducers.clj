(ns jardingaard.reducers
  (:use [clojure.core.protocols]))

(defn rrange
  ([end] (rrange 0 end 1))
  ([start end] (rrange start end 1))
  ([start end step]
     (reify
       CollReduce
       (coll-reduce [_ f val] (loop [i start
                                     res val]
                                (if (< i end)
                                  (recur (+ i step) (f res i))
                                  res))))))

(defn product [a b]
  (reify
    CollReduce
    (coll-reduce [_ f val]
      (coll-reduce a (fn [res v]
                       (coll-reduce b (fn [res2 v2]
                                        (f res2 [v v2]))
                                    res))
                   val))))