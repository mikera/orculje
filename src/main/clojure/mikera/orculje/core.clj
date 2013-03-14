(ns mikera.orculje.core
  (:use mikera.cljutils.error)
  (:use mikera.orculje.util)
  (:import [mikera.engine PersistentTreeGrid])
  (:import [mikera.util Rand Maths])
  (:require [mikera.orculje.engine :as engine])
  (:require [mikera.cljutils.find :as find]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(declare get-thing)
(declare update-thing)
(declare merge-thing)

(defmacro valid 
  "Asserts that an expression is true, throws an error otherwise."
  ([body & msgs]
    `(or ~body
       (error ~@msgs))))

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

(defn loc-add 
  ([^mikera.orculje.engine.Location a ^mikera.orculje.engine.Location b]
    (engine/->Location (+ (.x a) (.x b)) (+ (.y a) (.y b)) (+ (.z a) (.z b))))
  ([^mikera.orculje.engine.Location a ^long x ^long y ^long z]
    (engine/->Location (+ (.x a) x) (+ (.y a) y) (+ (.z a) z))))

(defn direction 
  ^mikera.orculje.engine.Location [^mikera.orculje.engine.Location from-loc 
                                   ^mikera.orculje.engine.Location to-loc]
  (mikera.orculje.engine.Location. (int (Maths/sign (- (.x to-loc) (.x from-loc))))
                                   (int (Maths/sign (- (.y to-loc) (.y from-loc))))
                                   (int (Maths/sign (- (.z to-loc) (.z from-loc))))))

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

(defmacro ! 
  "Sets a property of a Thing"
  ([thing key value]
    `(let [k# ~key
           t# ~thing]
       (! ~'game t# k# ~value)))
  ([game thing key value]
    `(let [game# ~game
           k# ~key
           t# ((:thing-map game#) (:id ~thing)) ;; equivalent of get-thing
           t# (assoc t# k# ~value)]
       (update-thing game# t#))))

(defmacro !+
  "Adds to a property of a thing"
  ([thing key value]
    `(!+ ~'game thing key value))
  ([game thing key value]
    `(let [t# ~thing
           k# ~key
           v# ~value]
       (! ~'game t# k# (+ v# (or (k# t#) 0))))))

(defn location 
  "Gets the location of a thing."
  (^mikera.orculje.engine.Location [game thing]
    (loop [l (:location (get-thing game thing))]
      (if (instance? mikera.orculje.engine.Location l)
        l
        (recur (:location (get-thing game l)))))))

(defn contents 
  ([thing]
    (:things thing))
  ([game thing]
    (contents (get-thing game thing))))

;; =======================================================
;; Game subsystem

(defn game? [game]
  (instance? mikera.orculje.engine.Game game))

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

(defn get-tile
  "Returns the terrain in a given location"
  ([^mikera.orculje.engine.Game game ^mikera.orculje.engine.Location loc]
    (.get ^PersistentTreeGrid (.world game) (.x loc) (.y loc) (.z loc)))
  ([^mikera.orculje.engine.Game game ^long x ^long y ^long z]
    (.get ^PersistentTreeGrid (.world game) (int x) (int y) (int z))))

(defn set-tile
  "Sets the terrain in a given location"
  ([^mikera.orculje.engine.Game game ^mikera.orculje.engine.Location loc value]
    (assoc game :world (.set ^PersistentTreeGrid (.world game) (.x loc) (.y loc) (.z loc) value)))
  ([^mikera.orculje.engine.Game game x y z value]
    (assoc game :world (.set ^PersistentTreeGrid (.world game) (int x) (int y) (int z) value))))

(defn get-things
  "Returns a vector of things in a given location, or nil if none found" 
  ([^mikera.orculje.engine.Game game ^mikera.orculje.engine.Location loc]
    (.get ^PersistentTreeGrid (.things game) (.x loc) (.y loc) (.z loc)))
  ([^mikera.orculje.engine.Game game ^long x ^long y ^long z]
    (.get ^PersistentTreeGrid (.things game) (int x) (int y) (int z))))

(defn get-thing [game id-or-thing]
  (let [id (if (number? id-or-thing) id-or-thing (:id id-or-thing))]
    ((:thing-map game) id)))

(defn all-things 
  "Returns a sequence of all things in the game"
  [game]
  (vals (:thing-map game)))

(defn add-thing-to-map
  [^mikera.orculje.engine.Game game 
   ^mikera.orculje.engine.Location loc 
   ^mikera.orculje.engine.Thing thing]
  (let [cur-things (or (get-things game loc) [])]
    ;; TODO: error if thing id already present
    (let [^PersistentTreeGrid cur-grid (:things game)
          id (or (:id thing) (new-id game))
          thing-map (or (:thing-map game) (error "No thing-map found ?!?"))
          _ (when (thing-map id) (error "Thing already present!!"))
          new-thing (as-> thing thing
                      (assoc thing :id id)
                      (assoc thing :location loc))
          new-things (conj cur-things new-thing)]
      (as-> game game
        (assoc game :things (.set cur-grid (.x loc) (.y loc) (.z loc) new-things))
        (assoc game :thing-map (assoc thing-map id new-thing))
        (assoc game :last-added-id id)))))

(defn add-thing-to-thing ^mikera.orculje.engine.Game [^mikera.orculje.engine.Game game parent thing]
  (let [id (or (:id thing) (new-id game))
        thing-map (:thing-map game)
        _ (when (thing-map id) (error "Thing already present!!"))
        parent-id (if (number? parent) parent (or (:id parent) (error "no parent ID ?!?")))
        parent (or (thing-map parent-id) (error "Parent [" parent-id "] not found!!"))
        new-thing (as-> thing thing
                        (assoc thing :id id)
                        (assoc thing :location parent-id))
        ;; parent (pd (assoc parent :things (conj (or (:things parent) []) new-thing)))
        ]
      (as-> game game
        ;(do (println parent) game)
        ;(update-thing game parent) 
        (merge-thing game parent {:things (conj (or (:things parent) []) new-thing)})
        (assoc game :thing-map (assoc (:thing-map game) id new-thing))
        (assoc game :last-added-id id)
        (do
          (valid (:id parent))
          (valid (:things (get-thing game parent)))
          game))))

(defn add-thing ^mikera.orculje.engine.Game [game loc thing]
  (if (instance? mikera.orculje.engine.Location loc)
    (add-thing-to-map game loc thing)
    (add-thing-to-thing game loc thing)))

(defn- remove-thingmap-recursive 
  "Remove from the :thing-map, a thing and all its children"
  ([tm thing]
    (let [id (if (number? thing) thing (or (:id thing) (error "Thing has no ID!!")))
          thing (or (tm id) (error "Thing with id [" id "] not in thing-map"))
          tm (dissoc tm id)
          tm (reduce (fn [tm t] (remove-thingmap-recursive tm t)) tm (:things thing))]
      tm)))

(defn remove-thing-from-map 
  ^mikera.orculje.engine.Game
  [^mikera.orculje.engine.Game game 
   ^mikera.orculje.engine.Thing thing]
  (let [thing-map (:thing-map game)
        things ^PersistentTreeGrid (:things game)
        id (or (:id thing) (error "Thing has no ID!"))
        thing (or (thing-map id) (error "Can't find thing! " id))
        ^mikera.orculje.engine.Location loc (or (:location thing) (error "Thing is not on map!"))
        x (.x loc) y (.y loc) z (.z loc)
        thing-vec (.get things x y z)
        reduced-thing-vec (remove-from-vector thing thing-vec)]
    (as-> game game
      (assoc game :things (.set things x y z reduced-thing-vec))
      (assoc game :thing-map (remove-thingmap-recursive (:thing-map game) id)))))

(defn remove-thing-from-thing [game parent thing]
  (let [thing-map (:thing-map game)
        parent (get-thing game parent)
        thing (get-thing game thing)
        thing-id (or (:id thing) (error "thing has no ID!"))
        children (:things parent)
        ci (find/find-index #(= (:id %) thing-id) children)
        new-children (vector-without children ci)
        new-parent (assoc parent :things new-children)]
    (as-> game game
      (update-thing game new-parent)
      (assoc game :thing-map (remove-thingmap-recursive (:thing-map game) thing-id)))))

(defn remove-thing
  [game thing]
  (let [thing (get-thing game thing)
        loc (or (:location thing) (error "Thing is not present!"))]
    (if (instance? mikera.orculje.engine.Location loc)
      (remove-thing-from-map game thing)
      (remove-thing-from-thing game loc thing))))

(defn move-thing [^mikera.orculje.engine.Game game 
                  ^mikera.orculje.engine.Thing thing 
                  ^mikera.orculje.engine.Location loc]
  (let [thing-map (:thing-map game)
        things ^PersistentTreeGrid (:things game)
        id (or (:id thing) (error "Thing has no ID!"))
        thing (or (thing-map id) (error "Can't find thing! " id))
        ^mikera.orculje.engine.Location cloc (or (:location thing) (error "Thing is not on map!"))
        cx (.x cloc) cy (.y cloc) cz (.z cloc)
        nx (.x loc) ny (.y loc) nz (.z loc)
        thing-vec (.get things cx cy cz)
        reduced-thing-vec (remove-from-vector thing thing-vec)
        new-thing (-> (get-thing game thing)
                    (assoc :location loc))
        new-things (.set things cx cy cz reduced-thing-vec)
        target-thing-vec (or (.get things nx ny nz) [])
        increased-thing-vec (conj target-thing-vec new-thing)
        new-things (.set new-things nx ny nz increased-thing-vec)]
    (-> game
      (assoc :things new-things)
      (assoc :thing-map (assoc thing-map id new-thing))
      (assoc :last-added-id id))))

(defn update-thing
  "Updates a thing within the game. Thing must have valid ID and location
   Warning: must not break validation rules" 
  (^mikera.orculje.engine.Game [^mikera.orculje.engine.Game game 
                                ^mikera.orculje.engine.Thing changed-thing]
    (let [loc (or (:location (get-thing game changed-thing)) (error "thing has no locaation!"))]
      ; (println (str "Updating: " (into {} changed-thing)))
      (as-> game game 
          (remove-thing game changed-thing)
          ;(do (println game) game)
          (add-thing game loc changed-thing)
          ;(do (println game) game)          
          ))))

(defn merge-thing 
  "Update a thing, merging in some new properties"
  ([game thing props]
  (let [thing (merge (get-thing game thing) props)
        loc (or (:location thing) (error "thing has no locaation!"))]
    (as-> game game
      (remove-thing game thing)
      (add-thing game loc thing)))))

(defn get-pred 
  "Gets the first object satisfying a predicate in a square. Considers tile last."
  ([game loc pred]
    (let [ts (get-things game loc)
          tl (get-tile game loc)]
      (or 
        (find/find-first pred ts)
        (if (pred tl) tl nil)))))

(defn get-blocking 
  "Gets the object blocking a specific square"
  ([game loc]
    (let [ts (get-things game loc)
          tl (get-tile game loc)]
      (or 
        (find/find-first :is-blocking ts)
        (if (:is-blocking tl) tl nil))))) 


;; ======================================================
;; validation code


(defn validate-game-thing [game thing]
  (valid (:id thing))
  (valid (or (nil? (:things thing)) (vector? (:things thing))))
  (if-let [loc (:location thing)]
    (if (loc? loc)
      (let [^mikera.orculje.engine.Location loc loc]
        (valid 
          (<= 0 (find-identical-position thing (.get ^PersistentTreeGrid (:things game) (.x loc) (.y loc) (.z loc))))))
      (do 
        (valid (number? loc))
        (valid (loc? (location game thing)))))))

(defn validate-game [game]
  (valid (instance? mikera.orculje.engine.Game game))
    (let [world (:world game)
          things (:things game)
          thing-map (:thing-map game)]
      (valid world)
      (valid things) 
      (valid thing-map (str "No thing map!" game))
      (valid (every? (partial validate-game-thing game) (vals thing-map)))))

(defn validate [game]
  "Validates a game, throws an error for ay issue"
  (validate-game game))