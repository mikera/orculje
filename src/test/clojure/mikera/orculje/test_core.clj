(ns mikera.orculje.test-core
  (:use [mikera.orculje core])
  (:use [clojure test]))

(deftest test-thing
  (testing "Thing construction"
    (let [t (thing {:id 1 :name 'bob})]
      (is (= 'bob (? t :name)))
      (is (= 1 (? t :id))))))

(deftest test-game
  (testing "Game construction"
    (let [g (empty-game)]
      (is (nil? (terrain g 0 0 0))))))