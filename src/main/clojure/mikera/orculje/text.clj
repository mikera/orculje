(ns mikera.orculje.text
  (:use mikera.cljutils.error)
  (:use mikera.orculje.core))

(def name-hints
  {:number "Number of items, used to show plutrality"
   :is-quantity "Used to falg a quanitifiable substance, e.g. 'water'"
   :grammatical-person "Used to specify :first or :second (default is :third)"
   :name-fn "Function to calculate a name n the context of a game, called with (name-fn game thing)"})

(def verb-lookup 
  {})

(defn third-person-verb [vb]
  (if-let [irregular (verb-lookup vb)]
    (error "irregular verb not yet implemented")
    (str vb "s"))) 

(defn get-person [t]
  (or (:grammatical-person t) :third))

(defn base-name [game thing]
  (let [identified? (if-let [id-fn (:is-identified (:functions game))]
                      (id-fn game thing)
                      (:is-identified thing))] 
    (or (and (not identified?) (:unidentified-name thing))
        (:proper-name thing)
        (if-let [name-fn (:name-fn thing)]
          (name-fn game thing))
        (:name thing)
        (error "object has no name!" thing)))) 

(defn plural? [thing]
  (if-let [num (:number thing)]
    (not (== num 1))))

(defn singular? [thing]
  (not
    (or (:is-quantity thing)
        (plural? thing)
        false)))

(defn starts-with-vowel? [^String s]
  (boolean (#{\a \e \i \o \u} (.charAt s 0))))

(defn the-name [game thing]
  (or (:proper-name thing)
      (if-let [person (:grammatical-person thing)]
        (base-name game thing)) 
      (str "the " (base-name game thing)))) 

(defn a-name [game thing]
  (let [bname (base-name game thing)]
    (or (:proper-name thing)
        (if-let [person (:grammatical-person thing)]
          bname) 
        (str (if (singular? thing)
               (if (starts-with-vowel? bname) "an " "a ")
               "some ") 
             bname)))) 

(defn str-add [s a]
  (str 
    (if s (str s " "))
    a)) 

(defn capitalise ^String [^String s]
  (if (> (count s) 0)
    (str (Character/toUpperCase (.charAt s 0)) (.substring s 1))
    s))

(defn verb-phrase [game & terms]
  (loop 
    [context {:person :third
              :definite :false}
     s nil
     ts (seq terms)]
    (if ts
      (let [t (first ts)]
        (cond 
          (keyword? t)
            (recur 
              (case t
                :the (assoc context :definite true)
                :a (assoc context :definite false))
              s
              (next ts)) 
          (thing? t)
            (recur 
              (assoc context :person (get-person t))
              (str-add s 
                       (if (:definite context)
                         (the-name game t)
                         (a-name game t)))
              (next ts))
          (string? t)
            (recur
              context
              (str-add s
                       (if (= :third (:person context))
                         (third-person-verb t)
                         t))
              (next ts))
          :else
            (error "unregognised term in phrase: " t)))
      s)))