(ns mikera.orculje.demo.world
  (:use mikera.orculje.core))

;; WORLD SIMULATION
;; everything internal to the game goes in here

;; key external functions (called by main namespace)

(defn new-game []
  (let [game (empty-game)]
    (merge game
           {:turn 0})))

(defn handle-command
  "Handles a command, expressed as a complete command String"
  [game k]
  (let [turn (inc (:turn game))]
    (println (str "Handling turn " turn " with keypress " k))
    (-> game
      (assoc :turn turn))))