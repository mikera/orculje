(ns mikera.orculje.materials)
;; ========================================================
;; material lists and attributes

(def wood-names ["alder" "applewood" "ash" "balsawood" "beech" "birch"  "bloodwood"
                 "cedar" "cherrywood" "corkwood" "cypress" "elm" "eucalyptus" "gumtree"
                 "hickory" "hornbeam" "ironwood" "larch" "laurel" "mahogany" "maple" "oak"  
                 "pine" "poplar" "redwood" "rosewood" "sandalwood" "snakewood" 
                 "spruce" "sycamore" "teak"  "walnut" "willow"])

(def primary-metal-names ["copper" "iron" "steel" "silver" "titanium"])

(def secondary-metal-names ["gold" "bronze" "platinum"
                            "nickel" "cobalt" "manganese" "molybdenum" "magnesium"
                            "tungsten" "zinc" "billion" "brass" "electrum" "rose gold"
                            "palladium" "tin" "aluminium" "lead"])

(def special-metal-names ["black steel" "red steel" "elvish steel" "dwarven steel"
                          "krythium" "eternium" "adamantium" "parrilite"])

(def all-metal-names)


(def precious-stone-names ["emerald" "ruby" "sapphire" "diamond" "red diamond"
                           "starstone" "moonstone" "sunstone"])



(def METALS nil)

(def WOODS
  (reduce 
    (fn [mats name]
      (let [mk (keyword name)]
        (assoc mats mk {:key mk
                        :name name
                        :is-wood true
                        :material-type :is-wood})))
    {} wood-names))

(let [mats (merge WOODS
                  METALS)]
  (def MATERIALS (reduce
                    (fn [mats [k mat]]
                      (assoc mats k mat))
                    mats mats)))