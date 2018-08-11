(ns jardingaard.field
  (:import [clojure.lang IPersistentCollection IPersistentMap IPersistentSet ILookup IFn Seqable])
  (:use [clojure.core.protocols]
        [jardingaard util helpers]))

(defprotocol SubField
  (in-rect [field pa pb])
  (in-radius [field p r]))

(extend-protocol SubField
  Object
  (in-rect [field pa pb]
    (filter #(rect-contains? (:p %) pa pb)))
  (in-radius [field p r]
    (filter #(<= (distance p (:p %)) r))))

(deftype Field [maxi elements ps->is]
  IPersistentCollection
  (empty [this] (Field. 0 {} {}))
  (count [this] (count elements))
  (cons [this x]
    (if-let [p (:p x)]
      (let [[i x] (if-let [i (:i x)]
                    [i x]
                    (let [i (inc maxi)]
                      [i (assoc x :i i)]))]
        (Field. (max maxi i)
                (assoc elements i x)
                (update (cond-> ps->is
                                (elements i)
                                (update (->tilep (:p (elements i))) disj i))
                        (->tilep p) conj-set i)))
      (throw (ex-info "value must have :p member" {}))))

  IPersistentMap
  (assoc [this i x]
    (conj this (assoc x :i i)))
  (without [this i]
    (if-let [p (:p (elements i))]
      (Field. maxi
              (dissoc elements i)
              (update ps->is (->tilep p) disj i))
      this))

  IPersistentSet
  (disjoin [this x]
    (dissoc this (:i x)))
  (contains [this x]
    (contains? elements (:i x)))

  ILookup
  (valAt [this key]
    (get this key nil))
  (valAt [this key not-found]
    (if (integer? key)
      (elements key not-found)
      (elements (:i key) not-found)))

  IFn
  (invoke [this key]
    (get this key nil))
  (invoke [this key not-found]
    (get this key not-found))

  CollReduce
  (coll-reduce [this f]
    (coll-reduce (vals elements) f))
  (coll-reduce [this f val]
    (coll-reduce (vals elements) f val))

  Seqable
  (seq [this] (seq (vals elements)))
  Object
  (toString [this] (pr-str (tagged-literal 'jardingaard.field (apply vector this))))

  SubField
  (in-rect [this pa pb]
    (let [[[pc0 pc1] [pd0 pd1]] (normalize-rect (->tilep pa) (->tilep pb))]
      (for [p0 (range pc0 (+ pd0 4) 4)
            p1 (range pc1 (+ pd1 4) 4)
            i (ps->is [p0 p1])
            :let [x (elements i)]
            :when (rect-contains? pa pb (:p x))]
        x)))
  (in-radius [this p r]
    (filter #(<= (distance p (:p %)) r)
            (in-rect this (minus p [r r]) (plus p [r r])))))

(defmethod print-method Field [x ^java.io.Writer w]
  (.write w (str x)))

(defn field
  ([]
   (Field. 0 {} {}))
  ([xs]
   (into (field) xs)))

(defn map-field [f coll]
  (into (field) (map f coll)))
