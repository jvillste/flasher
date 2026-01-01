(ns flasher.random
  (:import
   (java.util Random)))

(def ^:dynamic random (Random.))

(defn random-double
  ([minimum maximum]
   (+ minimum
      (* (.nextDouble random)
         (- maximum minimum))))
  ([]
   (.nextDouble random)))

(defn pick-random [collection]
  (nth collection
       (* (random-double)
          (count collection))))

(defn shuffle-collection [^java.util.Collection coll]
  (let [array-list (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle array-list random)
    (clojure.lang.RT/vector (.toArray array-list))))

(defmacro with-fixed-random-seed [& body]
  `(binding [random (Random. 1)]
     ~@body))
