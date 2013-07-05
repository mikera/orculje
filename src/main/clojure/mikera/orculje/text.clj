(ns mikera.orculje.text
  (:use [mikera.cljutils error])
  (:require [mikera.cljutils.find :as find])
  (:use mikera.orculje.core)
  (:import [org.atteo.evo.inflector English]))

(def name-hints
  {:number "Number of items, used to show plutrality"
   :is-quantity "Used to falg a quanitifiable substance, e.g. 'water'"
   :grammatical-person "Used to specify :first or :second (default is :third)"
   :name-fn "Function to calculate a name n the context of a game, called with (name-fn game thing)"})

(def verb-lookup 
  {"is" {:second-person "are"
         :third-person "is"}
   "cry" {:third-person "cries"}})

(def irregular-plural-lookup 
  {"mouse" "mice"})

(defn pronoun [thing]
  (cond
    (= :second (:grammatical-person thing)) "you"
    (:gender thing) (if (= :male (:gender thing))  "he" "she") 
    :else "it"))

(defn third-person-verb [vb]
  (or 
    (if-let [irregular (verb-lookup vb)] (:third-person irregular))
    (str vb (if (= \s (last vb)) "es" "s")))) 

(defn pluralise [s]
  (when s
    (or 
      (irregular-plural-lookup s)
      (English/plural s))))

(defn person-verb [vb person]
  (cond 
    (= :third person) (third-person-verb vb)
    :else vb))

(defn get-person [t]
  (or (:grammatical-person t) :third))

(defn base-name 
  "Returns the singular base name of a thing."
  ([game thing]
    (let [identified? (is-identified? game thing)] 
      (or (and (not identified?) (:unidentified-name thing))
          (:proper-name thing)
          (if-let [name-fn (:name-fn thing)] (name-fn game thing))
          (:name thing)
          (error "object has no name!" thing))))) 

(defn plural-name 
  "Returns the pluralised base name of a thing"
  ([game thing]
    (let [identified? (is-identified? game thing)] 
      (or (and (not identified?) 
               (or (:unidentified-name-plural thing) (pluralise (:unidentified-name thing))))
          (or (:proper-name-plural thing) (pluralise (:proper-name thing)))
          (if-let [name-fn (:name-fn thing)] (name-fn game thing))
          (or (:name-plural thing) (pluralise (:name thing)))
          (error "object has no plural name!" thing)))))

(defn num-name 
  "Returns the base name of a thing plus a number if > 1"
  ([game thing]
    (let [num (or (:number thing) 1)
          plural? (> num 1)
          bname (if plural? (plural-name game thing) (base-name game thing))] 
      (if plural?
        (str num " " bname)
        bname))))

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

(defn the-name 
  "Returns the name of a thing with a definite article (the)"
  ([game thing]
    (cond 
      (:proper-name thing)
        (:proper-name thing)
      (if-let [person (:grammatical-person thing)] (= person :second))
        (base-name game thing)
      (:is-quantity thing) 
        (str "the " (base-name game thing))
      :else 
        (str "the " (num-name game thing))))) 

(defn a-name 
  "Returns the name of a thing with an indefinite article (a, some)"
  ([game thing]
    (cond
      (:proper-name thing) 
        (:proper-name thing)
      (:is-quantity thing) 
        (str "some " (base-name game thing)) 
      (plural? thing) 
        (str (:number thing) " " (plural-name game thing))
      :else 
        (let [bname (base-name game thing)]
          (str (if (starts-with-vowel? bname) "an " "a ") bname))))) 

(defn and-string [ss]
  (let [ss (find/eager-filter identity ss)
        c (count ss)]
    (cond 
      (== c 0) nil
      (== c 1) (first ss)
      (== c 2) (str (first ss) " and " (second ss))
      :else (str
              (apply str (map #(str % ", ") (take (- c 2) ss)))
              (and-string (drop (- c 2) ss))))))

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
    (valid (game? game) "First parameter must be a game!")
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
          (associative? t) 
            (do ;; (println (str "merge:" t)) 
              (recur (merge context t) s (next ts))) 
          (string? t)
            (recur
              context
              (str-add s
                       (do ;;(println context)
                         (person-verb t (:person context))))
              (next ts))
          :else
            (error "unregognised term in phrase: " t)))
      s)))