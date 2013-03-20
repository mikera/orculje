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