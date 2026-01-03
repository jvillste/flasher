(ns flasher.random
  (:require
   [clojure.test :refer [deftest is]])
  (:import
   (java.util Random)))

(def ^:dynamic random (Random.))

(defmacro with-fixed-random-seed [& body]
  `(binding [random (Random. 1)]
     ~@body))

(defn random-double
  ([minimum maximum]
   (+ minimum
      (* (.nextDouble random)
         (- maximum minimum))))
  ([]
   (.nextDouble random)))

(defn pick-random [collection]
  (if (empty? collection)
    nil
    (nth collection
         (* (random-double)
            (count collection)))))

(deftest test-pick-random
  (is (= 1
         (with-fixed-random-seed (pick-random [1]))))

  (is (= nil
         (with-fixed-random-seed (pick-random [])))))

(defn shuffle-collection [^java.util.Collection coll]
  (let [array-list (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle array-list random)
    (clojure.lang.RT/vector (.toArray array-list))))
