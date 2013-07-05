(ns mikera.orculje.util
  (:use mikera.cljutils.error)
  (:import [java.awt Color]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defmacro pd [exp]
  `(let [exp# ~exp]
     (println exp#)
     exp#))

(defn long*
  "Multiply and round to the nearest whole long value"
  (^long [] 1)
  (^long [a] (long (+ 0.5 a)))
  (^long [a b]
    (long (+ 0.5 (* a b))))
  (^long [a b c]
    (long (+ 0.5 (* a b c))))
  (^long [a b c d]
    (long (+ 0.5 (* a b c d))))
  ([a b c d & more]
    (long (+ 0.5 (reduce * (* a b c d) more)))))

(defn colour 
  ([^long argb]
    (Color. (bit-or (unchecked-int 0xFF000000) (unchecked-int argb)))))

(defn name-pred [^String name]
  (fn [thing]
    (.equals name (if (string? thing) thing (:name thing)))))

(defn map-equals-except [ignore-key-set a b]
  (= (reduce dissoc a ignore-key-set)
     (reduce dissoc b ignore-key-set)))
