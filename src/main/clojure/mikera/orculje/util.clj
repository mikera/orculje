(ns mikera.orculje.util)

(defn find-identical-position
  "Searches a vector for an identical item and returns the index, or -1 if not found.
   Mainly used to pick out the position of a thing within a specific location"
  ^long [item ^clojure.lang.PersistentVector vector]
  (let [c (count vector)]
    (loop [i (int 0)]
      (if (>= i c)
        -1
        (if (identical? item (.nth vector i)) i (recur (inc i)))))))