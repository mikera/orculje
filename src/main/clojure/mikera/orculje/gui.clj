(ns mikera.orculje.gui
    (:import [java.awt.event ActionListener]))

(defmacro action 
  "creates a java.awt.ActionListner that executes the given code"
  ([body]
    `(reify javax.swing.Action
       (actionPerformed [action event]
         ~body))))

