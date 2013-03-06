(ns mikera.orculje.core
  (:import [mikera.engine PersistentTreeGrid])
  (:require [mikera.orculje.engine :as engine]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; =======================================================
;; location handling

(defn loc? [loc]
  (instance? mikera.orculje.engine.Location loc))

(defn loc [^long x ^long y ^long z]
  (engine/->Location x y z))

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

(defn terrain
  ([^mikera.orculje.engine.Game game ^long x ^long y ^long z]
    (.get ^PersistentTreeGrid (.world game) (int x) (int y) (int z))))