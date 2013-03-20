(ns mikera.orculje.rules
  (:use mikera.orculje.core)
  (:use mikera.cljutils.error)
  (:use mikera.orculje.util)
  (:import [mikera.util Rand]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; ============================================================
;; General game rules for the orculje engine
;;
;; These are all optional for users of the orculje library
;; however they present a pretty good "ready-built" RPG system
;; designed for use with the orculje library. 

;; =============================================================
;; main player stats
;;
;; these are the primary statistics for characters (players and NPCs)
;; used to determine capabilities across various dimensions
;;
;; Typical ranges for humans
;; 0-4    : subhuman
;; 5-10   : regular human
;; 10-20  : skilled human
;; 20-40  : masterful human
;; 40-80  : legendary human / hero
;; 80-160 : superhuman
;; 160+   : demigod

(let [main-stats-base {:SK {:name "skill"
                            :desc "determines physical skill and dexterity"}
                       :ST {:name "strength"
                            :desc "determines pure physical strength"}
                       :AG {:name "agility"
                            :desc "determines physical agility and reaction speed"}
                       :TG {:name "toughness"
                            :desc "determines ability to resist phyical hardships and damage"}
                       :IN {:name "intelligence"
                            :desc "determines mental aptitude and ability to solve problems"}
                       :WP {:name "willpower"
                            :desc "determines strength of will and ability to resist mental attacks" }
                       :CH {:name "charisma"
                            :desc "determines social skill and ability to interact with others"}
                       :CR {:name "craft"
                            :desc "determines creativity and ability to create new items"}}]
  (def MAIN-STATS (vec (keys main-stats-base)))
  (def MAIN-STAT-INFO main-stats-base))

;; ==============================================================
;; other stats generally useful for roguelike games

(let [roguelike-stats-base 
      {:speed    {:name "speed"
                  :desc "determines movement speed relative to other actors"
                  :default 100}
       :aps      {:name "APs"
                  :desc "Determines the number of action points an entity has. An entity may not move until it has aps>0."}
       :hps      {:name "HPs"
                  :desc "determines how much damage an entity can sustain"}
       :hps-max  {:name "HPs max"
                  :desc "determines maximum HPs for a being / entity"}}]
  (def ROGUELIKE-STATS (vec (keys roguelike-stats-base)))
  (def ROGUELIKE-STAT-INFO roguelike-stats-base))


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
(def STAT-RESIST-FACTOR 0.5)    ;; fixed at 0.5
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
                (assoc v :default-factor (or (:default-factor v) DEFAULT-DAMAGE-FACTOR))
                (assoc v :resist (or (:resist v) (keyword (str "resist-" (name k)))))
                (assoc v :armour (or (:armour v) (keyword (str "armour-" (name k))))))))
      dtypes-base
      dtypes-base)))


(defn calc-armour
  "Calculates the armour of a thing vs. a specific damage type"
  ^double [game target damage-type]
  (let [dt (or (DAMAGE-TYPE-INFO damage-type) (error "Damage type not known [" damage-type "]"))
        arm-stat (or (:armour dt) (error "No armour stat available for damage type " damage-type))
        arm-val (or (? game target arm-stat) 0.0)]
    arm-val))

(defn calc-resistance 
  "Calculates the resistance of a thing vs. a specific damage type"
  ^double [game target damage-type]
  (let [dt (or (DAMAGE-TYPE-INFO damage-type) (error "Damage type not known [" damage-type "]"))
        resist-stat (:resist-stat dt)
        resist-val (* STAT-RESIST-FACTOR (double (or (? game target resist-stat) 0.0)))
        resist-bonus-stat (:resist dt) 
        resist-bonus (double (or (? game target resist-bonus-stat) 0.0))]
    (+ resist-val resist-bonus))) 

(defn calc-damage
  "Calculates damage on a target, after including immunity and resistances"
  ([game target base-damage damage-type]
    (let [dt (or (DAMAGE-TYPE-INFO damage-type) (error "Damage type not known [" damage-type "]"))
          resist-stat (:resist-stat dt)
          resist-val (calc-resistance game target damage-type)
          affect-pred (:affect-pred dt)
          affected? (or (not affect-pred) (affect-pred target))
          factor-stat (or (:factor dt) (error "Damage type [" damage-type  "]  has no :factor"))
          factor (or (? game target factor-stat) (:default-factor dt))]
      (if affected?
        (long* factor base-damage (/ base-damage (+ resist-val base-damage)))
        0))))

;; ========================================================
;; skill checks and random tests

(defn check 
  "Random skill check. Tests a skill level a against a difficulty level b.
   Difficulties should be proportional to main player stats."
  ([^double a ^double b]
    (> a (* (Rand/nextDouble) (+ a b)))))

(defn multiple-check 
  "Performs a skill check muliple times. Returns the number of sucecsses"
  ([^double a ^double b ^long n]
    (let [chance (/ a (+ a b))]
      (loop [i 0 succ 0]
        (if (< i n)
          (recur (inc i) (if (Rand/chance chance) (inc succ) succ))
          succ))))) 
