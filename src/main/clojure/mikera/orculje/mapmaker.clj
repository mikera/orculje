(ns mikera.orculje.mapmaker
  (:use mikera.orculje.core)
  (:use mikera.cljutils.loops)
  (:import [mikera.util Rand])
  (:require [mikera.orculje.engine :as engine]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn fill-block 
  ([game 
    ^mikera.orculje.engine.Location lower 
    ^mikera.orculje.engine.Location upper 
    value]
    (let [^mikera.engine.PersistentTreeGrid grid (:world game)
          x1 (min (.x lower) (.x upper))
          x2 (max (.x lower) (.x upper))
          y1 (min (.y lower) (.y upper))
          y2 (max (.y lower) (.y upper))
          z1 (min (.z lower) (.z upper))
          z2 (max (.z lower) (.z upper))]
      (assoc game :world (.setBlock grid x1 y1 z1 x2 y2 z2 value)))))

(defn reduce-block 
  "Reduces over a block of squares from lmin to lmain"
  ([f init loc1 loc2]
    (let [lmin (loc-min loc1 loc2)
          lmax (loc-max loc1 loc2)
          [x1 y1 z1] lmin
          [x2 y2 z2] lmax]
      ;;(println (str "reduce-block range: " lmin lmax))
      (loop [m init z (long z1)]
        (if (<= z z2)
          (recur 
            (loop [m m y (long y1)]
              (if (<= y y2)
                (recur 
                  (loop [m m x (long x1)]
                    (if (<= x x2)
                      (let [l (loc x y z)]
                        ;;(println (str "reducing at: " l))
                        (recur (f m l) (inc x)))
                      m)) (inc y))
                m)) (inc z))
          m))))) 

(def DEFAULT_PLACE_RETRIES 20)

(defn find-loc 
  "Attempts to find a location that satisfies a given predicate."
  ([lmin lmax loc-pred]
    (find-loc lmin lmax loc-pred DEFAULT_PLACE_RETRIES))
  ([lmin lmax loc-pred max-retries]
    (loop [i max-retries]
      (let [l (rand-loc lmin lmax)]
        (when (> i 0)
          (if (loc-pred l)
            l
            (recur (dec i))))))))

(defn place-thing
  "Places a thing randomly within the given area. 
   Returns nil if thing cannot be placed (i.e. if no unblocked square can be found)"
  [game 
   ^mikera.orculje.engine.Location la 
   ^mikera.orculje.engine.Location lb 
   t]
  (let [^mikera.orculje.engine.Location lower (loc-min la lb)
        ^mikera.orculje.engine.Location upper (loc-max la lb)
        ^mikera.engine.PersistentTreeGrid grid (:world game)
          x1 (min (.x lower) (.x upper))
          x2 (max (.x lower) (.x upper))
          y1 (min (.y lower) (.y upper))
          y2 (max (.y lower) (.y upper))
          z1 (min (.z lower) (.z upper))
          z2 (max (.z lower) (.z upper))]
    (loop [i DEFAULT_PLACE_RETRIES]
      (when (> i 0)
        (let [x (Rand/range x1 x2)
              y (Rand/range y1 y2)
              z (Rand/range z1 z2)
              tloc (loc x y z)]
          (if (get-blocking game tloc)
            (recur (dec i))
            (add-thing game tloc t)))))))

(defn maybe-place-thing 
  "Attempts to place a thing, returns unchanged game if placing fails" 
  ([game l1 l2 t]
    (or (and t (place-thing game l1 l2 t))
        game)))


(defn scatter-things
  "Scatters a number of things in a given area, using the specified generator function" 
  ([game l1 l2 num thing-or-func]
    (reduce 
        (fn [game _]
          (maybe-place-thing game l1 l2 (if (fn? thing-or-func)
                                          (thing-or-func)
                                          thing-or-func)))
        game
        (range num))))


