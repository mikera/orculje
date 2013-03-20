(ns mikera.orculje.test-types
  (:use clojure.test)
  (:require [mikera.orculje.core :as core])
  (:import [clojure.lang Seqable]) 
  (:import [java.util Map]) 
  (:require [clojure.main])
  (:require [clojure.core.typed :refer [ann inst cf fn> pfn> check-ns ann-form ann-datatype def-alias tc-ignore override-method]]))

(def-alias Location mikera.orculje.engine.Location)
(def-alias Location? (U nil Location))
(def-alias Thing mikera.orculje.engine.Thing)
(def-alias Thing? (U nil Thing))
(def-alias Game mikera.orculje.engine.Game)
(def-alias Game? (U nil Game))
(def-alias Properties Any)

(ann clojure.core/*unchecked-math* Boolean)

(ann-datatype mikera.orculje.engine.Location [x :- Integer
                                              y :- Integer
                                              z :- Integer])

(ann mikera.orculje.core/SPECIAL-PROPERTIES Map)

(ann mikera.orculje.core/loc? [Location -> Boolean])

(ann mikera.orculje.core/loc-within? [Location Location Location -> Boolean])

(ann mikera.orculje.core/loc-bound [Location Location Location -> Location])

(ann mikera.orculje.core/rand-loc [Location Location -> Location])

(ann mikera.orculje.core/loc (Fn [(I (Seqable Long) (ExactCount 3)) -> Location]
                                 [Long Long Long -> Location]))

(ann mikera.orculje.core/loc-add (Fn [Location Location -> Location]
                                     [Location Long Long Long -> Location]))

(ann mikera.orculje.core/loc-inc [Location -> Location])
(ann mikera.orculje.core/loc-dec [Location -> Location])

(ann mikera.orculje.core/loc-min [Location Location -> Location])
(ann mikera.orculje.core/loc-min [Location Location -> Location])
(ann mikera.orculje.core/direction [Location Location -> Location])
(ann mikera.orculje.core/location-towards [Location Location -> Location])

(ann mikera.orculje.core/thing [Any -> Thing])
(ann mikera.orculje.core/thing? [Any -> Boolean])

(ann mikera.orculje.core/get-modified-value (All [x] [Game Thing Any clojure.lang.Keyword x -> x]))

(ann mikera.orculje.core/location [Game (U Thing Location Long) -> Location?])

(ann mikera.orculje.core/parent [Game (U Thing Long) -> Thing?])

(ann mikera.orculje.core/contents (Fn [Thing -> (Seqable Thing)]
                                      [Game Thing -> (Seqable Thing)]))

(ann mikera.orculje.core/get-number [Thing -> Long])

(ann mikera.orculje.core/game? [Any -> Boolean])

(ann mikera.orculje.core/empty-game (Fn [ -> Game]))

(ann mikera.orculje.core/new-id [Game -> Long])

(ann mikera.orculje.core/get-tile (Fn [Game Location -> Thing?]
                                      [Game Long Long Long -> Thing?]))

(ann mikera.orculje.core/set-tile (Fn [Game Location Thing? -> Game]
                                      [Game Long Long Long Thing? -> Game]))

(ann mikera.orculje.core/all-things (Fn [Game -> (Seqable Thing)]
                                        [Game (Fn [Thing -> Boolean]) -> (Seqable Thing)]))

(ann mikera.orculje.core/is-identified? [Game Thing -> Boolean])

;;
;; this fails
;;(ann foo [Any -> Number])
;; (defn foo [x] "Hello")


;;(deftest test-check-ns
;;  (is ))

;; (check-ns 'mikera.orculje.core)