(ns mikera.orculje.test-rules
  (:use [mikera.orculje core rules])
  (:use [clojure test]))

(def game (empty-game))

(deftest test-damage
  (testing "stats provide resistance at 0.5 factor"
    (let [t (thing {:TG 4})]  
      (== 0.0 (calc-armour nil t :normal ))
      (== 2.0 (calc-resistance nil t :normal ))
      (== 2.0 (calc-damage nil t 4 :normal )))
    (let [t (thing {})]  
      (== 0.0 (calc-armour nil t :normal ))
      (== 0.0 (calc-resistance nil t :normal ))
      (== 4.0 (calc-damage nil t 4 :normal )))
    (let [t (thing {:WP 4})]  
      (== 0.0 (calc-armour nil t :normal ))
      (== 0.0 (calc-resistance nil t :normal ))
      (== 2.0 (calc-resistance nil t :lightning ))
      (== 2.0 (calc-damage nil t 4 :lightning )))
    (let [t (thing {:TG 4 :is-living false})]  
      (== 0.0 (calc-armour nil t :poison ))
      (== 2.0 (calc-resistance nil t :poison ))
      (== 0.0 (calc-damage nil t 4 :poison )))))

(deftest validate-damage-types
  (doseq [dt (vals DAMAGE-TYPE-INFO)]
    ;; (println (str dt))
    (is (keyword? (:resist-stat dt)))
    (is (keyword? (:resist dt)))
    (is (keyword? (:factor dt)))
    (is (keyword? (:armour dt)))
    (is (number? (:default-factor dt)))
    (is (contains? MAIN-STAT-INFO (:resist-stat dt)))))

(deftest validate-checks
  (is (number? (multiple-check 1 1 10)))
  (is (check 1 0))
  (is (not (check 0 1))))

(deftest test-wielding
  (let [game (empty-game)
        h (thing {:is-hero true})
        l (loc 0 0 0)
        game (add-thing game l h)
        h (get-thing game (:last-added-id game))
        w (thing (merge ATT_SWORD {:parent-modifiers [(modifier :fearsome true
                                                                {:when-effective (fn [mod parent child] (:wielded child))})]}))
        game (add-thing game h w)
        w (get-thing game (:last-added-id game))
        h (get-thing game h)]
    (is (:parent-modifiers w))
    (is (= l (location game w)))
    (is (= (:id h) (:location w)))
    (is (not (? game h :fearsome)))
    
    (is (== 1 (count (contents h))))
    
    (let [game (wield game h w :right-hand)]
      (is (? game h :fearsome))
      (is (== 1 (count (filter :wielded (contents game h))))))
    
    (let [game (wield game h w :right-hand)
          game (unwield game h (get-thing game w))]
      (is (not (? game h :fearsome))))
    
    (is ((:replaces (WIELD-TYPES :right-hand)) :right-hand))
    (let [game (wield game h w :right-hand)
          w2 (thing {:foo :bar})
          game (add-thing game h w2)
          w2 (get-thing game (:last-added-id game))
          game (wield game h w2 :right-hand)
          w2 (get-thing game w2)
          w (get-thing game w)]
      ;; (println (seq (contents game h))) 
      (is (:wielded w2))
      (is (not (:wielded w)))
      (is (not (? game h :fearsome))))))