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

(deftest test-location
  (testing "Location building"
    (let [l (loc 1 2 3)]
      (== 1 (.x l)))
    (let [l (loc [1 2 3])]
      (== 1 (.x l)))))