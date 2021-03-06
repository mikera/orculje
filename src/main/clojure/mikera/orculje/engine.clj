(ns mikera.orculje.engine
  (:use [mikera.orculje util])
  (:require [mikera.cljutils.error :refer [error]])
  (:import [mikera.engine PersistentTreeGrid]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; =======================================================
;; Core data structures

;; =========================================================
;; Location
;;
;; type to represent an immutable (x,y,z) location
;; note that because of cache line sizes, a Location has 
;; about the same overhead as a single boxed long.
;; this is a big win!

(deftype Location [^int x ^int y ^int z]
  clojure.lang.Indexed
    (nth [loc i]
      (let [li (long i)]
        (case li
          0 (.x loc)
          1 (.y loc)
          2 (.z loc)
          (throw (IndexOutOfBoundsException. (str "index: " i))))))
    (nth [loc i not-found]
      (let [li (long i)]
        (case li
          0 (.x loc)
          1 (.y loc)
          2 (.z loc)
          not-found)))
  clojure.lang.IFn
    (invoke [loc i]
      (let [li (long i)]
        (case li
          0 (.x loc)
          1 (.y loc)
          2 (.z loc)
          (throw (IndexOutOfBoundsException. (str "index: " i))))))
  clojure.lang.ILookup
    (valAt [loc i]
      (let [li (long i)]
        (case li
          0 (.x loc)
          1 (.y loc)
          2 (.z loc)
          (throw (IndexOutOfBoundsException. (str "index: " i))))))
    (valAt [loc i not-found]
      (let [li (long i)]
        (case li
          0 (.x loc)
          1 (.y loc)
          2 (.z loc)
          not-found)))
  java.lang.Object
    (hashCode [a]
      (+ (.x a) (bit-shift-left (.y a) 5) (bit-shift-left (.z a) 10)))
    (equals [a b]
      (if (instance? Location b)
        (let [^Location b b]
          (and (== (.x a) (.x b)) (== (.y a) (.y b)) (== (.z a) (.z b))))
        false))
    (toString [this]
      (str [(.x this) (.y this) (.z this)]))) 

(defrecord Game [^PersistentTreeGrid world    ;; grid of terrain
                 ^PersistentTreeGrid things   ;; grid of [Vector of Things] for each location
                 thing-map                    ;; map of id -> Thing
                 ])

;; Thing is a record with a few special fields:
;; see core/SPECIAL-PROPERTIES for description of special fields that may be set
(defrecord Thing [])

;; ======================================================
;; data type functions
