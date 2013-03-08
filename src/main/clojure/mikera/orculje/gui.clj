(ns mikera.orculje.gui
  (:import [java.awt.event ActionListener KeyEvent])
  (:import [javax.swing JComponent KeyStroke Action AbstractAction]))

(defmacro action 
  "creates a java.awt.ActionListner that executes the given code"
  ([body]
    `(proxy [AbstractAction] []
       (actionPerformed [~'event]
         ~body))))


(defn add-input-binding [^JComponent comp ^KeyStroke keystroke f]
  (let [input-map (.getInputMap comp JComponent/WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
        action-map (.getActionMap comp)]
    ;; (println (str "Setting up input binding: " keystroke))
    (.put input-map keystroke (str keystroke))
    (.put action-map (str keystroke) (action 
                                       (do 
                                         ;; (println "handled!")
                                         (f))))))

(defn keystroke 
  "Gets a keystroke object for the given string"
  (^KeyStroke [k]
    (keystroke k 0))
  (^KeyStroke [k mods]
    (KeyStroke/getKeyStroke (.charAt (str k) 0) mods)))

(defn keystroke-from-keyevent 
  "Gets a keystroke object for the given string"
  (^KeyStroke [^KeyEvent k]
    (keystroke-from-keyevent k 0))
  (^KeyStroke [^KeyEvent k mods]
    (KeyStroke/getKeyStroke k mods)))