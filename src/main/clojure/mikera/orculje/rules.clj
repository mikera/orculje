(ns mikera.orculje.rules
  (:use mikera.orculje.core)
  (:use mikera.cljutils.error)
  (:use mikera.orculje.util))

;; ============================================================
;; General game rules for the orculje engine
;;
;; These are optional for user of the orculje library
;; however they present a pretty good "ready-built" RPG system
;; designed for use with the orculje library


;; =============================================================
;; Damage types and calculations
;;
;; Damage is based on several different types indicated by keywords (:normal :fire :acid etc.)
;; different damage types behave differnetly and can have different effectiveness / resistences 
;;
;; Each damage type has:
;; - a vulnerability factor stat (with a default that is typcically 1.0 for most creatures)
;; - a resistance stat (typically TG = tougness, but could also be WP = willpower for magic damage)
;; - an armour stat (e.g. :ARM for physical damage, :ARM-fire for special fire-resisting armour)
;; - a resistance bonus stat (e.g. :resist-fire)

(def DEFAULT-RESIST-STAT :TG)   ;; stat used to reduce damage types
(def DEFAULT-DAMAGE-FACTOR 1.0) ;; multiplier for damage types, can be overrided by vulnerable / invulnerable creatures

(let [dtypes-base {:normal {:factor :damage-factor-normal
                            :armour :ARM
                            :resist :resist-normal}
                   :impact {:factor :damage-factor-impact
                            :armour :ARM}
                   :poison {:factor :damage-factor-poison
                            :affect-pred :is-living}
                   :fire {:factor :damage-factor-impact
                          :armour :ARM-ice}
                   :ice {:factor :damage-factor-ice
                         :armour :ARM-fire}
                   :water {:factor :damage-factor-water
                           :default-factor 0.0} ;; most things not damaged by water
                   :lightning {:factor :damage-factor-lightning
                               :armour :ARM-lightning
                               :resist-stat :WP}
                   :acid {:factor :damage-factor-acid
                          :armour :ARM}}]
  (def DAMAGE_TYPES (vec (keys dtypes-base)))
  (def DAMAGE-TYPE-INFO 
    (reduce 
      (fn [m [k v]]
        (assoc m k
          (as-> v v
                (assoc v :resist-stat (or (:resist-stat v) DEFAULT-RESIST-STAT))
                (assoc v :default-factor (or (:default-factor v) DEFAULT-DAMAGE-FACTOR)))))
      dtypes-base
      dtypes-base)))


(defn calc-armour
  "Calculates the armmour of a thing vs. a specific damage type"
  [game target damage-type]
  (let [dt (or (DAMAGE-TYPE-INFO damage-type) (error "Damage type not known [" damage-type "]"))
        arm-stat (:armour dt)
        arm-val (or (? game target arm-stat) 0)]
    arm-val))

(defn calc-damage
  "Calculates damage on a target, after including immunity and resistances"
  ([game target base-damage damage-type]
    (let [dt (or (DAMAGE-TYPE-INFO damage-type) (error "Damage type not known [" damage-type "]"))
          resist-stat (:resist-stat dt)
          resist-val (* 0.5 (or (? game target resist-stat) 0))
          affect-pred (:affect-pred dt)
          affected? (or (not affect-pred) (affect-pred target))
          factor-stat (or (:factor dt) (error "Damage type [" damage-type  "]  has no :factor"))
          factor (or (? game target factor-stat) (:default-factor dt))]
      (if affected?
        (long* factor base-damage (/ base-damage (+ resist-val base-damage)))
        0))))