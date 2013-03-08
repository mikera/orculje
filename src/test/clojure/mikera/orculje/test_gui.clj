(ns mikera.orculje.test-gui
  (:use mikera.orculje.gui)
  (:use clojure.test))

(deftest test-action
  (is (action (+ 1 2))))

