(ns mikera.orculje.engine
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
          not-found)))) 

(defrecord Game [^PersistentTreeGrid world    ;; grid of terrain
                 ^PersistentTreeGrid things   ;; grid of things (contains vectors)
                 thing-map                    ;; map of id -> thing
                 ])

(defrecord Thing [])