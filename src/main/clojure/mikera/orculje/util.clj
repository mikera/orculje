(ns mikera.orculje.util
  (:use mikera.cljutils.error)
  (:import [java.awt Color]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defmacro pd [exp]
  `(let [exp# ~exp]
     (println exp#)
     exp#))

(defn long*
  "Multiply and round to the nearest whole long value"
  (^long [] 1)
  (^long [a] (long (+ 0.5 a)))
  (^long [a b]
    (long (+ 0.5 (* a b))))
  (^long [a b c]
    (long (+ 0.5 (* a b c))))
  (^long [a b c d]
    (long (+ 0.5 (* a b c d))))
  ([a b c d & more]
    (long (+ 0.5 (reduce * (* a b c d) more)))))

(defn colour 
  ([^long argb]
    (Color. (bit-or (unchecked-int 0xFF000000) (unchecked-int argb)))))

(defn find-identical-position
  "Searches a vector for an identical item and returns the index, or -1 if not found.
   Mainly used to pick out the position of a thing within a specific location"
  ^long [item ^clojure.lang.APersistentVector vector]
  (let [c (count vector)]
    (loop [i (int 0)]
      (if (>= i c)
        -1
        (if (identical? item (.nth vector i)) i (recur (inc i)))))))

(defn vector-without
  "Cuts a specific position of a vector"
  [^clojure.lang.PersistentVector vector ^long i]
  (let [c (count vector)
        ni (inc i)]
    (cond 
      (== c 1) (if (== i 0) [] (error "Idex of out range: " i))
      (== ni c) (subvec vector 0 i)
      :else (vec (concat (subvec vector 0 i) (subvec vector ni c))))))

(defn remove-from-vector
  "Removes a specific object from a vector. Throws an error if the object is not found."
  [item ^clojure.lang.APersistentVector vector]
  (let [i (find-identical-position item vector)]
    (when (< i 0) (error "item not found!"))
    (vector-without vector i)))


(defmacro dovec 
  "Performs an operation for each element of an indexed vector. Binds i to the index at each element."
  ([[sym v :as bindings] & body]
    (when-not (vector? bindings) (error "dovec requires a binding vector"))
    (when-not (symbol? sym) (error "dovec binding requires a symbol"))
    `(let [v# ~v
           c# (count v#)]
       (loop [~'i 0]
         (if (< ~'i c#)
           (let [~sym (v# ~'i)] ~@body)
           nil)))))

(defmacro or-loop 
  "Evaluates body repeatedly up to a given number of times, until it returns a truthy value. 
   Returns nil if a truthy value is not found."
  ([[times :as bindings] & body]
    (when-not (vector? bindings) (error "or-loop requires a binding vector"))
    `(loop [tries# ~times]
       (if (<= tries# 0) nil
         (if-let [res# (do ~@body)]
           res#
           (recur (dec tries#)))))))