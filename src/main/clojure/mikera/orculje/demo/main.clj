(ns mikera.orculje.demo.main
  (:use mikera.cljutils.error)
  (:require [mikera.orculje.gui :as gui])
  (:require [mikera.orculje.demo.world :as world])
  (:import [javax.swing JFrame JComponent KeyStroke])
  (:import [java.awt Font Color])
  (:import [java.awt.event KeyEvent])
  (:import [mikera.gui JConsole]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^Font font (Font. "Courier New" Font/PLAIN 16))

(defn new-frame 
  (^JFrame []
    (let [frame (JFrame.)]
      frame)))

(defn new-console
  (^JConsole []
    (let [jc (JConsole. 100 40)]
      (.setMainFont jc font)
      (.setFocusable jc true)
      (.setCursorVisible jc false)
      (.setCursorBlink jc false)
      jc)))

(defn make-input-action 
  "Builds an input action handler for the specified state object"
  ([state k]
    (fn []
      (let [hand @(:event-handler state)]
        (or
          (hand k)
          (println (str "Key pressed but no event handler ready: " k)))))))

(defn redraw-screen [state]
  (let [^JConsole jc (:console state)
        w (.getColumns jc)
        h (.getRows jc)
        gw (- w 20)
        gh (- h 5)]
    (.setBackground jc (Color. 0x203040))
    (dotimes [y gh]
      (dotimes [x gw]
        (gui/draw jc x y (str x))))
    (.repaint jc)))

(defn make-main-handler
  [state]
  (fn [k]
    (let [k ({"5" "."} k k) ;; handle synonyms
          ]
      (swap! (:game state) world/handle-command k)
      (redraw-screen state)
      :handled)))

(defn setup-input 
  ([^JComponent comp state]
    (doseq [k "abcdefghijklmnopqrstuvwxyz 01234567890!\"\\/£$%^&*()'~<>?@#_-+=[]{},."]
      (gui/add-input-binding comp (gui/keystroke k) (make-input-action state (str k))))
    (doseq [k "ABCDEFGHIJKLMNOPQRSTUVWXYZ"]
      (gui/add-input-binding comp (gui/keystroke k) (make-input-action state (str k))))
    (doseq [[^KeyEvent ke k] {KeyEvent/VK_LEFT "4"
                              KeyEvent/VK_RIGHT "6"
                              KeyEvent/VK_UP "2"
                              KeyEvent/VK_DOWN "8"
                              KeyEvent/VK_ESCAPE "Q"}]
      (gui/add-input-binding comp (gui/keystroke-from-keyevent ke) (make-input-action state (str k))))))

(defn launch [state]
  (let [^JFrame frame (:frame state)
        ^JConsole jc (:console state)]
      (setup-input jc state) 
      (.add (.getContentPane frame) jc)
      (.pack frame)
      (.setVisible frame true)
      (redraw-screen state)
      frame))

(defn new-state []
  (let [state {:game (atom (world/new-game))
               :console (new-console)
               :frame (new-frame)
               :event-handler (atom nil)}]
    (reset! (:event-handler state) (make-main-handler state))
    state))

(def s (new-state))

(defn main 
  "Main entry point to the demo, called directly from Java main() method in DemoApp"
  ([]
    (let [^JFrame frame (launch s)]
      (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE))))