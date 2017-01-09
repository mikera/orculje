(ns mikera.orculje.core
  (:use mikera.cljutils.error)
  (:use mikera.cljutils.vectors)
  (:use mikera.orculje.util)
  (:require [mikera.orculje.engine :as engine])
  (:import [mikera.engine PersistentTreeGrid])
  (:import [mikera.util Rand Maths])
  (:import [mikera.orculje Finder])
  (:import [mikera.orculje.engine Game Location Thing])
  (:require [mikera.cljutils.find :as find])
  (:require [mikera.cljutils.loops :refer [dovec]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(declare get-thing)
(declare update-thing)
(declare merge-thing)
(declare stack-thing)

;; =======================================================
;; special properties for Things
(def SPECIAL-PROPERTIES 
  {:id {:desc "Long ID of a thing. Must exist whenever a thing is present in a Game"}
   :location {:desc "Defines the location of a thing. Can be a Long ID or Location"}
   :name {:desc "The name of a thing."}
   :things {:desc "A vector of child Things. Can be nil."}
   :modifiers {:desc "Defines the current modifiers active on a thing."}
   :number {:desc "Defined the number of things in a stack"}
   :can-stack? {:desc "A (fn [a b]...) that returns true if a can stack with b"}
   :stack-fn {:desc "A (fn [a b]...) that returns a combined stack of a and b"}})

(def MODIFIER-SPECIAL-PROPERTIES
  {:key "Keyword indicating the property that the modifier affects"
   :when-effective "(fn [mod parent child] ...) that returns true when the modifier should be effective"})

;; =======================================================
;; location handling

(defn loc? 
  "Returns true if loc is a valid Location object" 
  ([loc]
    (instance? Location loc)))

(defn loc-within? 
  "Returns true if the location a is within the given bounds lmin and lmax"
  ([^Location lmin 
    ^Location lmax 
    ^Location a]
    (and (>= (.x a) (.x lmin)) (<= (.x a) (.x lmax))
         (>= (.y a) (.y lmin)) (<= (.y a) (.y lmax))
         (>= (.z a) (.z lmin)) (<= (.z a) (.z lmax)))))

(defn loc-bound 
  "Returns a location clamped to within the specified bounds"
  (^Location [^Location lmin 
              ^Location lmax 
              ^Location a]
    (engine/->Location (int (max (.x lmin) (min (.x a) (.x lmax))))
                       (int (max (.y lmin) (min (.y a) (.y lmax))))
                       (int (max (.z lmin) (min (.z a) (.z lmax)))))))

(defn rand-loc 
  "Returns a random location within the given bounds"
  (^Location [^Location lmin 
              ^Location lmax]
    (let [cloc (engine/->Location (Rand/range (lmin 0) (lmax 0))
                                  (Rand/range (lmin 1) (lmax 1))
                                  (Rand/range (lmin 2) (lmax 2)))]
      cloc)))

(defn loc-dist-manhattan 
  "Rturns the manhattan distance between two locations."
  (^long [^Location a 
          ^Location b]
    (long (+ (Math/abs (- (.x a) (.x b)))
             (Math/abs (- (.y a) (.y b)))
             (Math/abs (- (.z a) (.z b)))))))

(defn loc 
  "Constructs a new Location"
  ([xs]
    (engine/->Location (xs 0) (xs 1) (xs 2)))
  ([^long x ^long y ^long z]
    (engine/->Location x y z)))

(defn loc-add 
  "Creates a new location by adding an offset to an existing location"
  ([^Location a ^Location b]
    (engine/->Location (+ (.x a) (.x b)) (+ (.y a) (.y b)) (+ (.z a) (.z b))))
  ([^Location a ^long dx ^long dy ^long dz]
    (engine/->Location (+ (.x a) dx) (+ (.y a) dy) (+ (.z a) dz))))


(defn loc-inc 
  ^Location ([^Location a]
    (engine/->Location (inc (.x a)) (inc (.y a)) (inc (.z a)))))


(defn loc-dec 
  ^Location ([^Location a]
    (engine/->Location (dec (.x a)) (dec (.y a)) (dec (.z a)))))

(defn loc-min 
  ^Location ([^Location a 
                                    ^Location b]
    (engine/->Location (min (.x a) (.x b)) (min (.y a) (.y b)) (min (.z a) (.z b)))))

(defn loc-max 
  ^Location ([^Location a ^Location b]
    (engine/->Location (max (.x a) (.x b)) (max (.y a) (.y b)) (max (.z a) (.z b)))))

(defn direction 
  "Gets a direction vector in the direction of from-loc to to-loc"
  (^Location [^Location from-loc 
              ^Location to-loc]
    (Location. (int (Maths/sign (- (.x to-loc) (.x from-loc))))
               (int (Maths/sign (- (.y to-loc) (.y from-loc))))
               (int (Maths/sign (- (.z to-loc) (.z from-loc)))))))

(defn location-towards 
  "Gets a location one square closer to the target location"
  (^Location [^Location from-loc 
                                   ^Location to-loc]
    (loc-add from-loc (direction from-loc to-loc))))

;; =======================================================
;; modifier definition
;;
;; usage: (modifier :SK (+ value (:ST thing) (:global-st-uplift (:globals game))))

(defn effective-when-wielded [mod parent child]
  (:wielded child))

(defmacro modifier 
  ([property expr]
    `(modifier ~property ~expr nil))
  ([property expr extra-props]
    `(let [key# ~property
           eprops# ~extra-props]
       (merge 
         {:mod-fn (fn [~'mod ~'game ~'thing key# ~'value]
                    ~expr)
          :priority 0
          :key key#}
         eprops#))))

(defmacro wielded-modifier
  ([property expr]
    `(modifier ~property ~expr {:when-effective effective-when-wielded}))
  ([property expr extra-props]
    `(modifier ~property ~expr (merge ~extra-props {:when-effective effective-when-wielded})))) 

;; =======================================================
;; Thing subsystem

(defn thing 
  "Constructs a new thing with the given properties."
  ([props]
    (let [props (or props (error "Can't construct a thing with no properties!"))] 
      (engine/map->Thing props))))

(defn thing? 
  "Returns true if t is a Thing" 
  ([t]
    (instance? Thing t)))

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
           t# (or (and ~'game ((:thing-map ~'game) (:id ~thing))) thing)]
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
  (^Location [game thing]
    (cond 
      (number? thing)
        (let [^Thing thing (or (get-thing game thing) (error "Thing has no location!"))]
          (location game thing))
      (thing? thing)
        (loop [l (or (:location (get-thing game thing)) (error "Thing has no location!"))]
          (if (instance? Location l)
            l
            (recur (:location (get-thing game l)))))
      (loc? thing)
        thing
      (not thing)
        (error "nil thing passed to location!") 
      :else 
        (error "Can't work out location for: " thing))))

(defn parent 
  "Gets the parent of a thing. Returns nil if the Thing has no parent."
  ([game thing]
    (let [thing (get-thing game thing)
          l (:location thing)] 
      (if (number? l) 
        (get-thing game l)
        nil))))

(defn contents 
  "Gets the contents (children) of a Thing. Used for inventory, effects etc."
  ([thing]
    (:things thing))
  ([game thing]
    (contents (get-thing game thing))))

(defn get-number 
  "Gets the number associated with a Thing. Usually 1, but may be more if the Thing represents
   a stack of items."
  (^long [thing]
    (long (or (:number thing) 1))))

;; =======================================================
;; Game subsystem

(defn game? [game]
  (instance? Game game))

(defn empty-game 
  "Creates a new, empty game object"
  ([]
    (engine/->Game
      (PersistentTreeGrid/EMPTY) ;; no world terrain
      (PersistentTreeGrid/EMPTY) ;; no things
      {}                         ;; no ids map to things
    ))
  ([params]
    (engine/->Game
      (PersistentTreeGrid/EMPTY) ;; no world terrain
     (PersistentTreeGrid/EMPTY)  ;; no things
     {}                          ;; no ids map to things
     nil                         ;; no metadata
     params                      ;; additional data
     )))

(defn new-id 
  "Creates a new, unique Long ID for a given game"
  ([game]
    (let [tm (:thing-map game)]
      (loop [max (int 1000)]
        (let [id (Long/valueOf (Rand/d max))]
          (if (tm id)
            (recur (* 3 max))
            id))))))

(defn get-tile
  "Returns the terrain in a given location"
  ([^Game game ^Location loc]
    (.get ^PersistentTreeGrid (.world game) (.x loc) (.y loc) (.z loc)))
  ([^Game game ^long x ^long y ^long z]
    (.get ^PersistentTreeGrid (.world game) (int x) (int y) (int z))))

(defn set-tile
  "Sets the terrain in a given location"
  ([^Game game ^Location loc value]
    (assoc game :world (.set ^PersistentTreeGrid (.world game) (.x loc) (.y loc) (.z loc) value)))
  ([^Game game x y z value]
    (assoc game :world (.set ^PersistentTreeGrid (.world game) (int x) (int y) (int z) value))))

(defn get-things
  "Returns a vector of things in a given location, or nil if none found" 
  ([^Game game ^Location loc]
    (.get ^PersistentTreeGrid (.things game) (.x loc) (.y loc) (.z loc)))
  ([^Game game ^long x ^long y ^long z]
    (.get ^PersistentTreeGrid (.things game) (int x) (int y) (int z))))

(defn get-thing [game id-or-thing]
  (let [id (if (number? id-or-thing) id-or-thing (:id id-or-thing))]
    ((:thing-map game) id)))

(defn all-things 
  "Returns a sequence of all things in the game"
  ([game]
    (vals (:thing-map game)))
  ([game pred]
    (filter pred (all-things game))))

;; =======================================================
;; query features

(defn is-identified? 
  "Returns true if an object has been identified."
  ([game thing]
    (boolean 
      (or (:is-identified thing) 
          (if-let [id-fn (:is-identified? (:functions game))]
            (id-fn game thing))))))

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

(defn try-stack 
  "Attempts to stack an object in a target vector of things. 
   Either a) completes the stacking operation and returns updated game
          b) returns nil if stacking is not possible"
  ([game ob potential-stack-vector]
    (if-let [can-stack? (:can-stack? ob)]
      (let [sc #(can-stack? ob %)
            stack-target (when potential-stack-vector (find/find-first sc potential-stack-vector))]
        (if stack-target
          (as-> game game
                (stack-thing game ob stack-target)
                (assoc game :last-added-id (:id stack-target))))))))

(defn add-thing-to-map
  "Adds a thing to a map location.
   Updates :things and :thing-map "
  ([^Game game 
   ^Location loc 
   ^Thing thing]
    (let [cur-things (or (get-things game loc) [])]
      ;; TODO: error if thing id already present
      (or
        (try-stack game thing cur-things)
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
                (assoc game :last-added-id id)))))))

(defn add-child-modifiers 
  ([parent child]
    (if-let [pmods (:parent-modifiers child)]
      (add-child-modifiers parent child pmods)
      parent))
  ([parent child pmods]
    (let [child-id (or (:id child) (error "child has no ID! : " child))]
      (reduce
        (fn [parent mod]
          (let [when-effective (:when-effective mod)]
            (if (or (not when-effective) (when-effective mod parent child))
              (let [k (or (:key mod) (error "modifier has no :key " mod))
                    all-mods (:modifiers parent)
                    key-mods (k all-mods)
                    new-mod (assoc mod :source child-id)]
                (assoc parent :modifiers (assoc all-mods k (cons new-mod key-mods))))
              parent)))
        parent
        pmods))))

(defn- add-child [parent child]
  (as-> parent parent
    (assoc parent :things (conj (or (:things parent) []) child))
    (if-let [pmods (:parent-modifiers child)]
      (add-child-modifiers parent child pmods)
      parent)))

(defn add-thing-to-thing ^Game [^Game game parent thing]
  (let [id (or (:id thing) (new-id game))
        thing-map (:thing-map game)
        _ (when (thing-map id) (error "Thing already present!!"))
        parent-id (if (number? parent) parent (or (:id parent) (error "no parent ID ?!?")))
        parent (or (thing-map parent-id) (error "Parent [" parent-id "] not found!!"))
        pcontents (:things parent)
        ;; parent (pd (assoc parent :things (conj (or (:things parent) []) new-thing)))
        ]
      (or
        (try-stack game thing pcontents)
        (let [new-thing (as-> thing thing
                        (assoc thing :id id)
                        (assoc thing :location parent-id))]
          (as-> game game
              ;(do (println parent) game)
              ;(update-thing game parent) 
              (update-thing game (add-child parent new-thing))
              (assoc game :thing-map (add-thingmap-recursive (:thing-map game) new-thing))
              (assoc game :last-added-id id)
              (do
                (valid (:id parent))
                (valid (:things (get-thing game parent)))
                game))))))

(defn add-thing 
  "Adds a Thing to a place in a game. Place may be either a Location or another Thing"
  (^Game [game place thing]
  (or thing (error "Can't add a nil thing!!"))
  (if (instance? Location place)
    (add-thing-to-map game place thing)
    (add-thing-to-thing game place thing))))

(defn remove-thing-from-map 
  "Removes a Thing from the map. Throws an error of not on the map."
  (^Game [^Game game ^Thing thing]
    (let [thing-map (:thing-map game)
          things ^PersistentTreeGrid (:things game)
          id (or (:id thing) (error "Thing has no ID!"))
          thing (or (thing-map id) (error "Can't find thing! " id))
          ^Location loc (or (:location thing) (error "Thing is not on map!"))
          x (.x loc) y (.y loc) z (.z loc)
          thing-vec (.get things x y z)
          reduced-thing-vec (remove-from-vector thing thing-vec)]
      (as-> game game
        (assoc game :things (.set things x y z reduced-thing-vec))
        (assoc game :thing-map (remove-thingmap-recursive (:thing-map game) id))))))

(defn- remove-child-modifiers 
  "Removes modifiers from a parent associated with a specific child object."
  ([parent child-id]
    (if-let [pmods (:modifiers parent)]
      (let [filt #(if (= child-id (:source %)) nil %)]
        (assoc parent :modifiers
               (reduce 
                 (fn [pmods [k mods]]
                   (assoc pmods k
                          (filterv filt mods)))
                 pmods
                 pmods)))
      parent)))

(defn- remove-child 
  "Remove a child from a parent thing. Returns the updated parent Thing.
   Removes modifiers if necessary."
  ([^Thing parent ^Thing child]
    (let [thing-id (or (:id child) (error "child has no ID!"))
          children (or (:things parent) (error "No :things in parent ?!?"))
          ci (find/find-index #(= (:id %) thing-id) children)
          new-children (vector-without children ci)]
      (as-> parent parent 
         (remove-child-modifiers parent thing-id)
         (assoc parent :things new-children)))))

(defn- remove-thing-from-thing [game parent thing]
  (let [thing-map (:thing-map game)
        parent (or (get-thing game parent) (error "Can't find parent!"))
        thing (or (get-thing game thing) (error "Can't find child thing!"))
        new-parent (remove-child parent thing)]
    (as-> game game
      (assoc game :thing-map (dissoc thing-map (:id thing)))
      (update-thing game new-parent) 
      )))

(defn remove-thing
  "Removes a Thing from the game. Thing may be on the map or a child of another Thing.
   An optional number may be provided, in which case the specified number of Things
   is removed from a stack (up to and including the whole stack).
   Returns the updated game."
  ([game thing]
    (if-let [thing (get-thing game thing)]
      (let [loc (or (:location thing) (error "Thing is not present!"))]
        (if (instance? Location loc)
          (remove-thing-from-map game thing)
          (remove-thing-from-thing game loc thing)))
      game))
  ([game thing number]
    (let [number (long number)
          thing (get-thing game thing)
          num (get-number thing)]
      (cond 
        (== num number)
          (remove-thing game thing)
        (> num number)
          (update-thing game (assoc thing :number (- num number)))
        :else 
          (error "Trying to remove more things [" number "] than exist [" num "]")))))

;(defn move-thing [^Game game 
;                  ^mikera.orculje.engine.Thing thing 
;                  ^Location loc]
;  (let [thing-map (:thing-map game)
;        things ^PersistentTreeGrid (:things game)
;        id (or (:id thing) (error "Thing has no ID!"))
;        thing (or (thing-map id) (error "Can't find thing! " id))
;        ^Location cloc (or (:location thing) (error "Thing is not on map!"))
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

(defn move-thing 
  "Moves a Thing to a new place. 
   Initial thing must be present on map.
   Target place may either be a map Location or a parent Thing.
   Performs movement only: other thing data will be as before."
  ([game thing place]
    (let [thing (or (get-thing game thing) (error "thing to move not found!!"))]
      (as-> game game
            (remove-thing game thing)
            (add-thing game place thing)))))

(defn- update-thing-within-parent [game parent-id changed-id changed-thing]
  (let [tm (:thing-map game) 
        parent (or (tm parent-id) (error "Parent not found!!"))
        pconts (or (:things parent) (error "Parent has no things?!?"))
        i (find/find-index #(= (:id %) changed-id) pconts)
        new-conts (assoc pconts i changed-thing)
        parent (remove-child-modifiers parent (:id changed-thing))
        parent (assoc parent :things new-conts)
        new-parent (add-child-modifiers parent changed-thing)]
    (as-> game game
      (update-thing game new-parent))))

(defn- update-thing-within-map [game ^Location tloc changed-id changed-thing]
  (let [x (.x tloc) y (.y tloc) z (.z tloc)
        ^PersistentTreeGrid grid (:things game) 
        lconts (or (.get grid x y z) (error "Map location has no things?!?"))
        i (find/find-index #(= (:id %) changed-id) lconts)
        nconts (assoc lconts i changed-thing)
        ngrid (.set grid x y z nconts)]
    (assoc game :things ngrid)))


(defn update-thing
  "Updates a thing within the game. Thing must have valid ID and location
   Warning: must not break validation rules, children must be correct and complete etc." 
  (^Game [^Game game 
          ^Thing changed-thing]
    (let [id (or (:id changed-thing) (error "No valid ID on updated thing"))
          old-thing (get-thing game changed-thing)
          l (or (:location old-thing) (error "Thing to be updated has no :location!"))]
      ; (println (str "Updating: " (into {} changed-thing)))
      (as-> game game 
          (if (number? l)
            (update-thing-within-parent game l id changed-thing)
            (update-thing-within-map game l id changed-thing))
          (assoc game :thing-map (assoc (:thing-map game) id changed-thing))
          (assoc game :last-added-id id)))))

(defn default-can-stack? 
  "Default stacking test. Should work for standard item stacks."
  ([a b]
    (and (= (:name a) (:name b))
         (= (:contents a) (:contents b))
         (map-equals-except #{:id :location :number} a b))))

(defn stack-thing 
  "Stacks the object source into the object dest, according to the :stack-fn function.
   Source is assumed to have been removed from the map."
  ([game source dest]
    (let [stack-fn (or (:stack-fn source) 
                       (fn [a b] (assoc b :number (+ (get-number a) (get-number b)))))]
      (as-> game game
            (update-thing game (stack-fn source dest))))))

(defn merge-thing 
  "Update a thing, merging in some new properties"
  ([game thing props]
  (let [thing (merge (get-thing game thing) props)]
    (update-thing game thing))))

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
  [^Game game pred ^Location loc-or-thing ^long range]
  (let [^Location cloc (location game loc-or-thing)
        ^Location loc1 (loc-add cloc (loc (- range) (- range) 0))
        ^Location loc2 (loc-add cloc (loc range range 0))
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
                        (when (< dist2 (long @best-distance-squared))
                          (reset! best-distance-squared dist2)
                          (reset! best-thing v))))))
        ^Finder finder (Finder. find-fn)]
    (.visitBlocks thing-grid finder x1 y1 z1 x2 y2 z2)
    @best-thing))

(defn find-things
  "Finds things on the map that satisfy a given predicate.
   Base location may be either a location or Thing ID
   Either a long range or a range vector may be provided"
  ([^Game game pred loc-or-thing loc2-or-range]
    (let [^Location loc1 (location game loc-or-thing)
          use-range? (number? loc2-or-range)
          ^Location loc2 (if use-range? 
                           (loc-add loc1 (loc (long loc2-or-range) (long loc2-or-range) 0)) 
                           loc2-or-range)
          ^Location loc1 (if use-range? 
                           (loc-add loc1 (loc (- (long loc2-or-range)) (- (long loc2-or-range)) 0))
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
      @found-things)))



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
  (valid (loc? (location game thing)))
  (if-let [loc (:location thing)]
    (if (loc? loc) 
      (let [^Location loc loc] ;; if the thing is on the map
        (valid 
          (find/find-position thing (.get ^PersistentTreeGrid (:things game) (.x loc) (.y loc) (.z loc)))
          (str "Cannot find thing within map contents vector for location.")))
      (do  ;; if the thing is a child, should resolve to a parent on the map
        (valid (number? loc))
        (valid (loc? (location game thing))))))
  (doseq [child (contents thing)]
    (validate-game-thing game child)
    (valid (identical? child (get-thing game child)))
    (valid (= (:id thing) (:location child))))
  (validate-modifiers game thing))

(defn validate-game [game]
  (valid (instance? Game game))
  (let [world (:world game)
        things (:things game)
        thing-map (:thing-map game)]
    (valid world)
    (valid things) 
    (valid thing-map (str "No thing map!" game))
    (doseq [[id thing] thing-map]
      (valid (= id (:id thing)))
      (validate-game-thing game thing))))

(defn validate [game]
  "Validates a game, throws an error for any issue"
  (validate-game game)
  true)