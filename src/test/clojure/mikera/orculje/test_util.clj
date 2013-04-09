(ns mikera.orculje.test-util
  (:use mikera.orculje.util)
  (:use clojure.test))

(deftest test-colour 
  (is (instance? java.awt.Color (colour 0xFF0000))))
