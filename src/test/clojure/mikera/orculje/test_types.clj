(ns mikera.orculje.test-types
  (:use [mikera.orculje core engine])
  (:use clojure.test)
  (:require [clojure.main])
  (:require [clojure.core.typed :refer [ann inst cf fn> pfn> check-ns ann-form]]))

(ann loc? [mikera.orculje.engine.Location -> Object])

;;(deftest test-check-ns
;;  (is ))

(check-ns)