(ns mikera.orculje.test-gui
  (:use mikera.orculje.gui)
  (:use clojure.test)
  (:import [javax.swing JButton KeyStroke Action]))

(deftest test-action
  (is (action (+ 1 2))))


(deftest test-input-binding
  (add-input-binding (JButton.) (KeyStroke/getKeyStroke "a") (fn [] "foo!!")))

