(ns mikera.orculje.engine
  (:import [mikera.engine PersistentTreeGrid]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; =======================================================
;; Core data structures

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
          not-found)))) 

(defrecord Game [^PersistentTreeGrid world 
                 ^PersistentTreeGrid things])

(defrecord Thing [])