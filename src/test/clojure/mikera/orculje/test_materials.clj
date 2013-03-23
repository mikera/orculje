(ns mikera.orculje.test-materials
  (:use mikera.orculje.materials)
  (:use clojure.test))

(deftest test-mats
  (doseq [[k mat] MATERIALS]
    (is (mat (:material-type mat)))
    (is (= (:key mat) k))))

