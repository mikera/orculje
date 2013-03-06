(ns mikera.orculje.engine
  (:import [mikera.engine PersistentTreeGrid]))

;; =======================================================
;; Core data structures

(defrecord Game [^PersistentTreeGrid world 
                 ^PersistentTreeGrid things])

(defrecord Thing [])