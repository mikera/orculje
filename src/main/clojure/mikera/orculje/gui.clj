(ns mikera.orculje.gui
  (:import [java.awt.event ActionListener])
  (:import [javax.swing JComponent KeyStroke Action]))

(defmacro action 
  "creates a java.awt.ActionListner that executes the given code"
  ([body]
    `(reify Action
       (actionPerformed [action event]
         ~body))))


(defn add-input-binding [^JComponent comp ^KeyStroke keystroke f]
  (let [input-map (.getInputMap comp JComponent/WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
        action-map (.getActionMap comp)]
    (.put input-map keystroke (str keystroke))
    (.put action-map (str keystroke) (action (f)))))