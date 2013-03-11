(ns mikera.orculje.gui
  (:use mikera.orculje.util)
  (:import [java.awt.event ActionListener KeyEvent])
  (:import [mikera.gui JConsole ConsoleData])
  (:import [javax.swing JComponent KeyStroke Action AbstractAction]))

;; screen filling and manipluation

(defprotocol PDisplay
  (display-char [m])
  (display-font [m])
  (display-foreground [m])
  (display-background [m]))

(defn draw 
  "Draws an object at a specified location on a console"
  [^JConsole console ^long x ^long y v]
  (.setCursorPos console (int x) (int y))
  (.write console (str v)))

(defn draw-char 
  "Draws an object at a specified location on a console"
  [^JConsole console ^long x ^long y ^long ch]
  (.setCursorPos console (int x) (int y))
  (.write console (char ch)))

;; key action handling

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