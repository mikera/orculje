(ns mikera.orculje.demo.world
  (:use mikera.orculje.core))

;; WORLD SIMULATION
;; everything internal to the game goes in here

;; key external functions (called by main namespace)

(defn new-game []
  (let [game (empty-game)]
    game))