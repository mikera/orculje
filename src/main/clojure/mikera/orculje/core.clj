(ns mikera.orculje.core
  (:import [mikera.engine PersistentTreeGrid])
  (:require [mikera.orculje.engine :as engine]))


;; =======================================================
;; Thing subsystem

(defn thing [props]
  (engine/map->Thing props))

(defn thing? [t]
  (instance? mikera.orculje.engine.Thing t))

(defmacro ? 
  ([thing key]
    `(let [k# ~key
           t# ~thing]
       (k# t#)))
  ([game thing key]
    `(let [t# ((:things g) (:id ~thing))]
       (? t# ~key))))

;; =======================================================
;; Game subsystem

(defn empty-game []
  (engine/->Game
    (PersistentTreeGrid/EMPTY) ;; no world terrain
    (PersistentTreeGrid/EMPTY) ;; no things
    ))