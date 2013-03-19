(ns mikera.orculje.test-text
  (:use clojure.test)
  (:use mikera.orculje.text)
  (:use mikera.orculje.core))

(deftest test-vowel
  (is (starts-with-vowel? "apple"))
  (is (not (starts-with-vowel? "pumpkin"))))

(deftest test-you
  (let [game (empty-game)
        t (thing {:grammatical-person :second
           :name "you"})
        t2 (thing {:name "wall"})]
    (is (= "you hit a wall" (verb-phrase game :the t "hit" :a t2))))
  )

(deftest test-and-string
  (is (= "a, b and c" (and-string ["a" "b" "c"]))))

(deftest test-identified
  (let [game (empty-game)
        t1 (thing {:name "bob" :unidentified-name "??" :is-identified false})
        t2 (thing {:name "bob" :unidentified-name "??" :is-identified true})]
    (is (= "??" (base-name game t1)))
    (is (= "bob" (base-name game t2)))))

(deftest test-cap
  (is (= "You" (capitalise "you"))))

(deftest test-plural 
  (let [game (empty-game)]
    (is (= "2 mice" (a-name game {:name "mouse" :number 2})))
    (is (= "the 2 mice" (the-name game {:name "mouse" :number 2})))
    (is (= "a mouse" (a-name game {:name "mouse" :number 1})))
    (is (= "some water" (a-name game {:name "water" :number 2 :is-quantity true})))
    (is (= "the water" (the-name game {:name "water" :number 2 :is-quantity true})))))