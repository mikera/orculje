(ns mikera.orculje.plan
  "Namespace for orculja planning engine."
  (:require [clojure.core.logic :as lg :refer [run* run membero unify fail succeed conde conso
                                               project]])
  (:require [mikera.orculje.core :as orc]))

(def actions
  '[{:desc "Navigate"
     :pre ()
     :post [[:location t l]]}
    {:desc "Pickup"
     :pre [[:location t l]
           [:location item l]] 
     :post [[:possess t item]]}])

;(defn check-precondition 
;  "Non-relational - check if a precondition holds in a given game."
;  ([game precond]
;  (conde
;    [(lg/== [:location ] precond) ()])))

(defn plan [game goal]
  (run 1 [plan]
       ))