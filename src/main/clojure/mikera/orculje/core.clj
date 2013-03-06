(ns mikera.orculje.core
  (:import [mikera.engine PersistentTreeGrid])
  (:require [mikera.orculje.engine :as engine]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; =======================================================
;; location handling

(defn loc? [loc]
  (instance? mikera.orculje.engine.Location loc))

(defn loc 
  ([xs]
    (engine/->Location (xs 0) (xs 1) (xs 2)))
  ([^long x ^long y ^long z]
    (engine/->Location x y z)))

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

(defn empty-game 
  "Creates a new, empty game object"
  ([]
    (engine/->Game
      (PersistentTreeGrid/EMPTY) ;; no world terrain
      (PersistentTreeGrid/EMPTY) ;; no things
    )))

(defn terrain
  "Returns the terrain in a given location"
  ([^mikera.orculje.engine.Game game ^mikera.orculje.engine.Location loc]
    (.get ^PersistentTreeGrid (.world game) (.x loc) (.y loc) (.z loc)))
  ([^mikera.orculje.engine.Game game ^long x ^long y ^long z]
    (.get ^PersistentTreeGrid (.world game) (int x) (int y) (int z))))

(defn things
  "Returns a vector of things in a given location, or nil if none found" 
  ([^mikera.orculje.engine.Game game ^mikera.orculje.engine.Location loc]
    (.get ^PersistentTreeGrid (.things game) (.x loc) (.y loc) (.z loc)))
  ([^mikera.orculje.engine.Game game ^long x ^long y ^long z]
    (.get ^PersistentTreeGrid (.things game) (int x) (int y) (int z))))