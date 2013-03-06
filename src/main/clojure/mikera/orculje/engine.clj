(ns mikera.orculje.engine
  (:import [mikera.engine PersistentTreeGrid]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; =======================================================
;; Core data structures

(defrecord Game [^PersistentTreeGrid world 
                 ^PersistentTreeGrid things])

(defrecord Thing [])