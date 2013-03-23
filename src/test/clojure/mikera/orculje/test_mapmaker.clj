(ns mikera.orculje.test-mapmaker
  (:use mikera.orculje.core)
  (:use clojure.test)
  (:use mikera.orculje.mapmaker))

(deftest test-block
  (let [game (empty-game)
        game (fill-block game (loc 0 0 0) (loc 2 2 2) {:foo true})]
    (is (:foo (get-tile game 2 2 2)))))

(deftest test-reduce-block
  (let [game (empty-game)
        game (reduce-block 
               (fn [game loc] (set-tile game loc :foo))
               game (loc 0 0 0) (loc 2 2 2))]
    (is game)
    (is (= :foo (get-tile game (loc 0 0 0))))
    (is (= :foo (get-tile game (loc 1 1 1))))
    (is (= :foo (get-tile game (loc 2 2 2))))))

