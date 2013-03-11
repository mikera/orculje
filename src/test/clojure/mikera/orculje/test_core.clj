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
      (== 1 (.x l))
      (== 2 (get l 1)))
    (let [l (loc [1 2 3])]
      (== 1 (.x l))
      (== 3 (get l 2))))
  (testing "Location compare"
    (is (= (loc 1 2 3) (loc [1 2 3])))
    (is (not (= (loc 1 2 3) (loc 1 2 4))))))

(deftest test-thing-locations
  (let [game (empty-game)
          l (loc 1 2 3)
          t (thing {:foo :bar})
          game (add-thing game l t)
          ts (get-things game l)
          nt (first ts)
          new-id (:id nt)]
      (testing "adding to map" 
        (is (= 1 (count ts)))
	      (is (vector? ts))
	      (is (= l (:location nt)))
	      (is new-id)
	      (is (= nt ((:thing-map game) new-id)))
	      (is (= :bar (? nt :foo)))
	      (is (= :bar (? game nt :foo)))
        (is (validate game)))
      (testing "removal from map"
        (let [game (remove-thing game nt)
              ts (get-things game l)]
          (is (not (seq ts)))
          (is (validate game))))))