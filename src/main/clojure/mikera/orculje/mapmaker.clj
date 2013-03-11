(ns mikera.orculje.mapmaker
  (:use mikera.orculje.core)
  (:use mikera.cljutils.loops)
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

