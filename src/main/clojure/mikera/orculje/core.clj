(ns mikera.orculje.core
  (:use mikera.cljutils.error)
  (:use mikera.orculje.util)
  (:import [mikera.engine PersistentTreeGrid])
  (:import [mikera.util Rand Maths])
  (:import [mikera.orculje Finder])
  (:require [mikera.orculje.engine :as engine])
  (:require [mikera.cljutils.find :as find]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(declare get-thing)
(declare update-thing)
(declare merge-thing)

;; =======================================================
;; location handling

(defn loc? 
  "Returns true if loc is a valid Location object" 
  ([loc]
    (instance? mikera.orculje.engine.Location loc)))

(defn loc-within? 
  ([^mikera.orculje.engine.Location lmin ^mikera.orculje.engine.Location lmax 
    ^mikera.orculje.engine.Location a]
    (and (>= (.x a) (.x lmin)) (<= (.x a) (.x lmax))
         (>= (.y a) (.y lmin)) (<= (.y a) (.y lmax))
         (>= (.z a) (.z lmin)) (<= (.z a) (.z lmax)))))

(defn loc-bound 
  ([^mikera.orculje.engine.Location lmin ^mikera.orculje.engine.Location lmax 
    ^mikera.orculje.engine.Location a]
    (engine/->Location (max (.x lmin) (min (.x a) (.x lmax)))
                       (max (.y lmin) (min (.y a) (.y lmax)))
                       (max (.z lmin) (min (.z a) (.z lmax))))))

(defn rand-loc [^mikera.orculje.engine.Location lmin ^mikera.orculje.engine.Location lmax]
  (let [cloc (engine/->Location (Rand/range (lmin 0) (lmax 0))
                                (Rand/range (lmin 1) (lmax 1))
                                (Rand/range (lmin 2) (lmax 2)))]
    cloc))

(defn loc-dist-manhattan [^mikera.orculje.engine.Location a ^mikera.orculje.engine.Location b]
  (long (+ (Math/abs (- (.x a) (.x b)))
           (Math/abs (- (.y a) (.y b)))
           (Math/abs (- (.z a) (.z b))))))

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


(defn loc-inc 
  ([^mikera.orculje.engine.Location a]
    (engine/->Location (inc (.x a)) (inc (.y a)) (inc (.z a)))))


(defn loc-dec 
  ([^mikera.orculje.engine.Location a]
    (engine/->Location (dec (.x a)) (dec (.y a)) (dec (.z a)))))

(defn loc-min 
  ([^mikera.orculje.engine.Location a ^mikera.orculje.engine.Location b]
    (engine/->Location (min (.x a) (.x b)) (min (.y a) (.y b)) (min (.z a) (.z b)))))

(defn loc-max 
  ([^mikera.orculje.engine.Location a ^mikera.orculje.engine.Location b]
    (engine/->Location (max (.x a) (.x b)) (max (.y a) (.y b)) (max (.z a) (.z b)))))

(defn direction 
  ^mikera.orculje.engine.Location [^mikera.orculje.engine.Location from-loc 
                                   ^mikera.orculje.engine.Location to-loc]
  (mikera.orculje.engine.Location. (int (Maths/sign (- (.x to-loc) (.x from-loc))))
                                   (int (Maths/sign (- (.y to-loc) (.y from-loc))))
                                   (int (Maths/sign (- (.z to-loc) (.z from-loc))))))

(defn location-towards 
  "Gets a location one square closer to the target location"
  (^mikera.orculje.engine.Location [^mikera.orculje.engine.Location from-loc 
                                   ^mikera.orculje.engine.Location to-loc]
    (loc-add from-loc (direction from-loc to-loc))))

;; =======================================================
;; modifier definition
;;
;; usage: (modifier :SK (+ value (:ST thing) (:global-st-uplift (:globals game))))
(defmacro modifier [property expr]
  `(let [key# ~property]
     {:mod-fn (fn [~'mod ~'game ~'thing key# ~'value]
                ~expr)
      :priority 0
      :key key#}))

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

(defn get-modified-value [game thing modifiers k unmodified-value]
  (if-let [mods (seq (k modifiers))]
    (do 
      ;; (println (str "modifying: " k " on " thing))
      (reduce (fn [v mod]
                (let [mfn (or (:mod-fn mod) (error "Modifier has no :mod-fn ! " mod))]
                  (mfn mod game thing k v))) unmodified-value mods))
    unmodified-value))

(defmacro ? 
  "Queries a property of a Thing"
  ([thing key]
    `(let [k# ~key
           t# ~thing
           v# (k# t#)]
       (if-let [mods# (:modifiers t#)]
         (get-modified-value ~'game t# mods# k# v#)
         v#)))
  ([game thing key]
    `(let [~'game ~game
           t# ((:thing-map ~'game) (:id ~thing))]
       (? t# ~key))))

(defmacro ! 
  "Sets a property of a Thing. Sets the base value (before any modifiers)"
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
  "Adds to a property of a thing. Adds to the base value (before any modifiers)"
  ([thing key value]
    `(!+ ~'game ~thing ~key ~value))
  ([game thing key value]
    `(let [t# ~thing
           k# ~key
           v# ~value]
       (! ~'game t# k# (+ v# (or (k# t#) 0))))))

(defn location 
  "Gets the location of a thing."
  (^mikera.orculje.engine.Location [game thing]
    (cond 
      (number? thing)
        (location game (get-thing game thing))
      (thing? thing)
        (loop [l (or (:location (get-thing game thing)) "Thing has no location!")]
          (if (instance? mikera.orculje.engine.Location l)
            l
            (recur (:location (get-thing game l)))))
      (loc? thing)
        thing
      :else 
        (loc thing))))

(defn parent 
  "gets the parent of a thing"
  ([game thing]
    (let [thing (get-thing game thing)
          l (:location thing)] 
      (if (number? l) 
        (get-thing game l)
        nil))))

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
  ([game]
    (vals (:thing-map game)))
  ([game pred]
    (find/eager-filter pred (all-things game))))

;; =======================================================
;; Thing addition / removal / updates, heirarchy maintenance etc.

(defn- remove-thingmap-recursive 
  "Remove from the :thing-map, a thing and all its children"
  ([tm thing]
    (let [id (if (number? thing) thing (or (:id thing) (error "Thing has no ID!!")))
          thing (or (tm id) (error "Thing with id [" id "] not in thing-map"))
          tm (dissoc tm id)
          tm (reduce (fn [tm t] (remove-thingmap-recursive tm t)) tm (:things thing))]
      tm)))

(defn- add-thingmap-recursive 
  "Add to the :thing-map, a thing and all its children. thing must be fully ready to add."
  ([tm thing]
    (let [id (or (:id thing) (error "Thing has no ID!!"))
          tm (assoc tm id thing)
          tm (reduce (fn [tm t] (add-thingmap-recursive tm t)) tm (:things thing))]
      tm)))

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
        (assoc game :thing-map (add-thingmap-recursive (:thing-map game) new-thing))
        (assoc game :last-added-id id)))))

(defn add-child-modifiers [parent child pmods]
  (let [child-id (or (:id child) (error "child has no ID! : " child))]
    (reduce
      (fn [parent mod]
        (let [k (or (:key mod) (error "modifier has no :key " mod))
              all-mods (:modifiers parent)
              key-mods (k all-mods)
              new-mod (assoc mod :source child-id)]
          (assoc parent :modifiers (assoc all-mods k (cons new-mod key-mods)))))
        parent
        pmods)))

(defn- add-child [parent child]
  (as-> parent parent
    (assoc parent :things (conj (or (:things parent) []) child))
    (if-let [pmods (:parent-modifiers child)]
      (add-child-modifiers parent child pmods)
      parent)))

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
        (update-thing game (add-child parent new-thing))
        (assoc game :thing-map (add-thingmap-recursive (:thing-map game) new-thing))
        (assoc game :last-added-id id)
        (do
          (valid (:id parent))
          (valid (:things (get-thing game parent)))
          game))))

(defn add-thing ^mikera.orculje.engine.Game [game loc thing]
  (if (instance? mikera.orculje.engine.Location loc)
    (add-thing-to-map game loc thing)
    (add-thing-to-thing game loc thing)))

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

(defn remove-child-modifiers [parent child-id]
  (if-let [pmods (:modifiers parent)]
    (let [filt #(if (= child-id (:source %)) nil %)]
      (assoc parent :modifiers
             (reduce 
               (fn [pmods [k mods]]
                 (assoc pmods k
                        (doall (find/eager-filter filt mods))))
               pmods
               pmods)))
    parent))

(defn- remove-child [parent child]
  (let [thing-id (or (:id child) (error "child has no ID!"))
        children (or (:things parent) (error "No :things in parent ?!?"))
        ci (find/find-index #(= (:id %) thing-id) children)
        new-children (vector-without children ci)]
    (as-> parent parent 
       (remove-child-modifiers parent thing-id)
       (assoc parent :things new-children))))

(defn remove-thing-from-thing [game parent thing]
  (let [thing-map (:thing-map game)
        parent (or (get-thing game parent) (error "Can't find parent!"))
        thing (or (get-thing game thing) (error "Can't find child thing!"))
        new-parent (remove-child parent thing)]
    (as-> game game
      (update-thing game new-parent) ;; note this handles child removal from :thing-map
      )))

(defn remove-thing
  [game thing]
  (if-let [thing (get-thing game thing)]
    (let [loc (or (:location thing) (error "Thing is not present!"))]
      (if (instance? mikera.orculje.engine.Location loc)
        (remove-thing-from-map game thing)
        (remove-thing-from-thing game loc thing)))
    game))

;(defn move-thing [^mikera.orculje.engine.Game game 
;                  ^mikera.orculje.engine.Thing thing 
;                  ^mikera.orculje.engine.Location loc]
;  (let [thing-map (:thing-map game)
;        things ^PersistentTreeGrid (:things game)
;        id (or (:id thing) (error "Thing has no ID!"))
;        thing (or (thing-map id) (error "Can't find thing! " id))
;        ^mikera.orculje.engine.Location cloc (or (:location thing) (error "Thing is not on map!"))
;        cx (.x cloc) cy (.y cloc) cz (.z cloc)
;        nx (.x loc) ny (.y loc) nz (.z loc)
;        thing-vec (.get things cx cy cz)
;        reduced-thing-vec (remove-from-vector thing thing-vec)
;        new-thing (-> (get-thing game thing)
;                    (assoc :location loc))
;        new-things (.set things cx cy cz reduced-thing-vec)
;        target-thing-vec (or (.get things nx ny nz) [])
;        increased-thing-vec (conj target-thing-vec new-thing)
;        new-things (.set new-things nx ny nz increased-thing-vec)]
;    (-> game
;      (assoc :things new-things)
;      (assoc :thing-map (assoc thing-map id new-thing))
;      (assoc :last-added-id id))))

(defn move-thing [game thing loc]
  (let [thing (or (get-thing game thing) (error "thing to move not found!!"))]
    (as-> game game
          (remove-thing game thing)
          (add-thing game loc thing))))

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
        (if (and tl (:is-blocking tl)) tl nil))))) 

;; =====================================================
;; finder functions

(defn find-nearest-thing
  [^mikera.orculje.engine.Game game pred ^mikera.orculje.engine.Location loc-or-thing range]
  (let [^mikera.orculje.engine.Location cloc (location game loc-or-thing)
        ^mikera.orculje.engine.Location loc1 (loc-add cloc (loc (- range) (- range) 0))
        ^mikera.orculje.engine.Location loc2 (loc-add cloc (loc range range 0))
        x1 (.x loc1) y1 (.y loc1) z1 (.z loc1)
        x2 (.x loc2) y2 (.y loc2) z2 (.z loc2)
        ^PersistentTreeGrid thing-grid (:things game)
        best-distance-squared (atom Long/MAX_VALUE)
        best-thing (atom nil)
        find-fn (fn [x y z vs]
                  ;;(println (str "checking things at " (loc x y z)))
                  (dovec [v vs]
                    (if (pred v)
                      (let [dx (- (long x) (.x cloc))
                            dy (- (long y) (.y cloc))
                            dz (- (long z) (.z cloc))
                            dist2 (+ (* dx dx) (* dy dy) (* dz dz))]
                        ;;(println (str "found" v " at " (loc x y z)))
                        (when (< dist2 @best-distance-squared)
                          (reset! best-distance-squared dist2)
                          (reset! best-thing v))))))
        ^Finder finder (Finder. find-fn)]
    (.visitBlocks thing-grid finder x1 y1 z1 x2 y2 z2)
    @best-thing))

(defn find-things
  [^mikera.orculje.engine.Game game pred loc-or-thing loc2-or-range]
  (let [^mikera.orculje.engine.Location loc1 (location game loc-or-thing)
        use-range? (number? loc2-or-range)
        ^mikera.orculje.engine.Location loc2 (if use-range? 
                                               (loc-add loc1 (loc loc2-or-range loc2-or-range 0)) 
                                               loc2-or-range)
        ^mikera.orculje.engine.Location loc1 (if use-range? 
                                               (loc-add loc1 (loc (- loc2-or-range) (- loc2-or-range) 0))
                                               loc1)
        x1 (.x loc1) y1 (.y loc1) z1 (.z loc1)
        x2 (.x loc2) y2 (.y loc2) z2 (.z loc2)
        ^PersistentTreeGrid thing-grid (:things game)
        found-things (atom nil)
        find-fn (fn [x y z vs]
                  ;; (println (str x "," y "," z vs))
                  (dovec [v vs]
                    ;;(println (str x "," y "," z " = " (str v)))
                    (when (pred v)
                      ;;(println "found!!")
                      (reset! found-things (cons v @found-things)))))
        ^Finder finder (Finder. find-fn)]
    (.visitBlocks thing-grid finder x1 y1 z1 x2 y2 z2)
    @found-things))



;; ======================================================
;; validation code

(defn validate-modifiers
  [game thing]
  (when-let [mods (:modifiers thing)]
    (valid (associative? mods))
    (valid (not (vector? mods)))
    (doseq [[k mod-list] mods]
      (valid (or (nil? mod-list) (sequential? mod-list))))))

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
        (valid (loc? (location game thing))))))
  (validate-modifiers game thing))

(defn validate-game [game]
  (valid (instance? mikera.orculje.engine.Game game))
  (let [world (:world game)
        things (:things game)
        thing-map (:thing-map game)]
    (valid world)
    (valid things) 
    (valid thing-map (str "No thing map!" game))
    (doseq [t (vals thing-map)]
      (validate-game-thing game t))))

(defn validate [game]
  "Validates a game, throws an error for any issue"
  (validate-game game)
  true)