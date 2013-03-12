(ns mikera.orculje.engine
  (:use [mikera.orculje util])
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
    (equals [a b]
      (if (instance? Location b)
        (let [^Location b b]
          (and (== (.x a) (.x b)) (== (.y a) (.y b)) (== (.z a) (.z b))))
        false))
    (toString [this]
      (str [(.x this) (.y this) (.z this)]))) 

(defrecord Game [^PersistentTreeGrid world    ;; grid of terrain
                 ^PersistentTreeGrid things   ;; grid of things (contains vectors)
                 thing-map                    ;; map of id -> thing
                 ])

(defrecord Thing [])

;; ======================================================
;; data type functions



;; ======================================================
;; validation code

(defn validate-game-thing [game thing]
  (and
    (:id thing)
    (if-let [^Location loc (:location thing)]
      (<= 0 (find-identical-position thing (.get ^PersistentTreeGrid (:things game) (.x loc) (.y loc) (.z loc)))))))

(defn validate-game [game]
  (and 
    (instance? Game game)
    (let [world (:world game)
          things (:things game)
          thing-map (:thing-map game)]
      (and 
        world
        things 
        thing-map
        (every? (partial validate-game-thing game) (vals thing-map))))))