(ns mikera.orculje.test-core
  (:use [mikera.orculje core])
  (:use [clojure test]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(deftest test-thing
  (testing "Thing construction"
    (let [game (empty-game)
          t (thing {:id 1 :name 'bob})]
      (is (= 'bob (? t :name)))
      (is (= 1 (? t :id))))))

(deftest test-game
  (testing "Game construction"
    (let [g (empty-game)]
      (is (nil? (get-tile g 0 0 0)))
      (is (nil? (get-thing g 1000)))
      (is (identical? g (remove-thing g 2000))))))

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
    (is (= (loc 1 0 -1) (direction (loc 10 5 10) (loc 15 5 5)))))
  (let [lmin (loc 2 2 2) lmax (loc 5 5 5)]
    (is (= (loc 2 4 5) (loc-bound lmin lmax (loc 1 4 7))))
    (is (loc-within? lmin lmax (rand-loc lmin lmax))))
  (testing "distance"
    (is (= 3 (loc-dist-manhattan (loc 1 1 1) (loc 1 2 3))))       
    ))

(deftest test-modifier
  (let [game (empty-game)
        t (thing {:SK 10 
                  :modifiers {:SK [(modifier :SK (+ value 15))]}})]
    (is (== 25 (? t :SK)))))

(deftest test-thing-contents
  (let [game (empty-game)
        l (loc 1 2 3)
        t1 (thing {:foo :bar})
        t2 (thing {:foo :baz
                   :parent-modifiers [(modifier :modified-by-child true)]})
        game (add-thing game l t1)
        t1 (get-thing game (:last-added-id game)) 
        game (add-thing game t1 t2)
        ;; _ (println game) 
        _ (validate game)
        t1 (get-thing game t1) ;; refresh t1 with correct children
        t2 (get-thing game (:last-added-id game)) ]
    ;(println game)
    ;(println (str (into {} t1)))
    ;(println (str (into {} t2)))
    (is (= l (:location t1)))
    (is (= :bar (:foo t1)))
    (is (= :baz (:foo t2)))
    (is (= (:id t1) (:location t2)))
    (is (= t1 (parent game t2)))
    ;; (println t1)
    (is (? t1 :modified-by-child))
    (is (validate game))
    (testing "contents"
      (let [cts (contents t1)]
        (is (vector? cts))
        (is (== 1 (count cts)))
        (is (= t2 (cts 0)))))
    (testing "remove it all!"
      (let [game (remove-thing game t1)]
        ;;(println game)
        (is (not (seq (all-things game))))
        (is (validate game))))
    (testing "remove the child!"
      (let [game (remove-thing game t2)
            t1 (get-thing game t1)]
        ;;(println game)
        (is (== 1 (count (all-things game))))
        (is (not (? t1 :modified-by-child))) 
        (is (validate game))))
    ))

(deftest test-thing-update
  (let [game (empty-game)
        l (loc 1 2 3)
        t1 (thing {:foo :bar})
        game (add-thing game l t1)
        t1 (get-thing game (:last-added-id game))
        t1 (assoc t1 :updated true)
        game (update-thing game t1) 
        t1 (get-thing game t1)]
    (is (:updated t1)) 
    (is (validate game))
    ))

(deftest test-thing-merge
  (let [game (empty-game)
        l (loc 1 2 3)
        t1 (thing {:foo :bar})
        game (add-thing game l t1)
        t1 (get-thing game (:last-added-id game))
        game (merge-thing game t1 {:merged true}) 
        t1 (get-thing game t1)]
    (is (:merged t1)) 
    (is (= l (:location t1))) 
    (is (validate game))
    ))

(deftest test-thing-map-stacking
  (let [game (empty-game)
        l (loc 1 2 3)
        t (thing {:number 2 :foo :bar :can-stack? default-can-stack?})
        game (add-thing game l t)
        game (add-thing game l t) 
        ts (get-things game l)
        t1 (first ts)]
    (is (= 4 (:number t1))) 
    (is (= 1 (count ts)))
    (is (validate game))))

(deftest test-thing-locations
  (let [game (empty-game)
          l (loc 1 2 3)
          t (thing {:foo :bar})
          game (add-thing game l t)
          ts (get-things game l)
          nt (first ts)
          new-id (:id nt)]
      (testing "finder"
        (is (not (find-things game :id (loc 0 0 0) (loc 1 1 1))))
        (is (= nt (first (find-things game :id (loc 0 0 0) (loc 3 3 3)))))
        (is (= nt (find-nearest-thing game :id (loc 0 0 3) 10)))
        (is (not (find-nearest-thing game :id (loc 0 0 0) 0)))
        (is (not (find-nearest-thing game :id (loc 0 0 0) 10))) ;; not on same z-plane
        )
      
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