(ns mikera.orculje.test-text
  (:use clojure.test)
  (:use mikera.orculje.text)
  (:use mikera.orculje.core))

(deftest test-vowel
  (is (starts-with-vowel? "apple"))
  (is (not (starts-with-vowel? "pumpkin"))))

(deftest test-you
  (let [t (thing {:grammatical-person :second
           :name "you"})
        t2 (thing {:name "wall"})]
    (is (= "you hit a wall" (verb-phrase nil :the t "hit" :a t2))))
  )

(deftest test-cap
  (is (= "You" (capitalise "you"))))