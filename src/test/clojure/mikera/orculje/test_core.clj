(ns mikera.orculje.test-core
  (:use [mikera.orculje core])
  (:use [clojure test]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(deftest test-thing
  (testing "Thing construction"
    (let [t (thing {:id 1 :name 'bob})]
      (is (= 'bob (? t :name)))
      (is (= 1 (? t :id))))))

(deftest test-game
  (testing "Game construction"
    (let [g (empty-game)]
      (is (nil? (get-tile g 0 0 0))))))

(deftest test-location
  (testing "Location building"
    (let [^mikera.orculje.engine.Location l (loc 1 2 3)]
      (== 1 (.x l))
      (== 2 (get l 1)))
    (let [^mikera.orculje.engine.Location l (loc [1 2 3])]
      (== 1 (.x l))
      (== 3 (get l 2))))
  (testing "Location compare"
    (is (= (loc 1 2 3) (loc [1 2 3])))
    (is (not (= (loc 1 2 3) (loc 1 2 4)))))
  (testing "Location direction"
    (is (= (loc 1 0 -1) (direction (loc 10 5 10) (loc 15 5 5))))))

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
        (is (= l (location game nt)))
	      (is new-id)
	      (is (= nt ((:thing-map game) new-id)))
	      (is (= :bar (? nt :foo)))
	      (is (= :bar (? game nt :foo)))
        (is (validate game)))
      (testing "update"
        (let [game (! game nt :changed true)] 
          (is (not (:changed nt)))
          (is (:changed (get-thing game nt)))
          (is (validate game))))
      (testing "add"
        (let [game (!+ game nt :something 100)] 
          (is (== 100 (? game nt :something)))
          (is (validate game))))
      (testing "move with map"
        (let [nloc (loc 11 12 13)
              game (move-thing game nt nloc)
              ots (get-things game l)]
          (is (not (seq ots)))
          (is (seq (get-things game nloc)))
          (is (validate game))))
      (testing "removal from map"
        (let [game (remove-thing game nt)
              ts (get-things game l)]
          (is (not (seq ts)))
          (is (validate game))))))