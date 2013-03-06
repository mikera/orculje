(ns mikera.orculje.core
  (:use mikera.cljutils.error)
  (:import [mikera.engine PersistentTreeGrid])
  (:import [mikera.util Rand])
  (:require [mikera.orculje.engine :as engine]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; =======================================================
;; location handling

(defn loc? 
  "Returns true if loc is a valid Location object" 
  ([loc]
    (instance? mikera.orculje.engine.Location loc)))

(defn loc 
  "Constructs a new Location"
  ([xs]
    (engine/->Location (xs 0) (xs 1) (xs 2)))
  ([^long x ^long y ^long z]
    (engine/->Location x y z)))

;; =======================================================
;; Thing subsystem

(defn thing 
  "Constructs a new thing with the given properties."
  ([props]
  (engine/map->Thing props)))

(defn thing? 
  "Returns tru if t is a Thing" 
  ([t]
    (instance? mikera.orculje.engine.Thing t)))

(defmacro ? 
  "Queries a property of a Thing"
  ([thing key]
    `(let [k# ~key
           t# ~thing]
       (k# t#)))
  ([game thing key]
    `(let [t# ((:thing-map ~game) (:id ~thing))]
       (? t# ~key))))

;; =======================================================
;; Game subsystem

(defn empty-game 
  "Creates a new, empty game object"
  ([]
    (engine/->Game
      (PersistentTreeGrid/EMPTY) ;; no world terrain
      (PersistentTreeGrid/EMPTY) ;; no things
      {}                         ;; no ids map to things
    )))

(defn new-id 
  "Creates a new, unique Long ID for a given game"
  ([game]
    (let [tm (:thing-map game)]
      (loop [max (int 1000)]
        (let [id (Long. (long (Rand/d max)))]
          (if (tm id)
            (recur (* 2 max))
            id))))))

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

(defn add-thing 
  [^mikera.orculje.engine.Game game 
   ^mikera.orculje.engine.Location loc 
   ^mikera.orculje.engine.Thing thing]
  (let [cur-things (or (things game loc) [])]
    ;; TODO: error if thing id already present
    (let [^PersistentTreeGrid cur-grid (:things game)
          id (new-id game)
          new-thing (-> thing
                      (assoc :id id)
                      (assoc :location loc))
          new-things (conj cur-things new-thing)]
      (-> game
        (assoc :things (.set cur-grid (.x loc) (.y loc) (.z loc) new-things))
        (assoc :thing-map (assoc (:thing-map game) id new-thing))))))

(defn remove-thing 
  [^mikera.orculje.engine.Game game 
   ^mikera.orculje.engine.Thing thing]
  (let [^mikera.orculje.engine.Location cur-loc (:location thing)]
    (when (not cur-loc (error "Thing is not on map!")))
    (TODO)))

(defn validate [game]
  "Validates a game"
  (engine/validate-game game))