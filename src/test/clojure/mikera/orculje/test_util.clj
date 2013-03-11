(ns mikera.orculje.test-util
  (:use mikera.orculje.util)
  (:use clojure.test))

(deftest test-vector-without
  (let [a 1 b 2 c 3]
    (= [a b] (vector-without [a b c] 2))
    (= [a c] (vector-without [a b c] 1))
    (= [b c] (vector-without [a b c] 0))
    (= [] (vector-without [a] 0))))

(deftest test-colour 
  (is (instance? java.awt.Color (colour 0xFF0000))))
