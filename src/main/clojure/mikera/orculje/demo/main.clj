(ns mikera.orculje.demo.main
  (:use mikera.cljutils.error)
  (:use mikera.orculje.gui)
  (:import [javax.swing JFrame JComponent KeyStroke])
  (:import [java.awt Font])
  (:import [java.awt.event KeyEvent])
  (:import [mikera.gui JConsole]))

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
      jc)))

(defn make-input-handler 
  "Builds an input handler for the specified state object"
  ([state k]
    (fn []
      (println (str "Key handled: " k)))))


(defn setup-input 
  ([^JComponent comp state]
    (doseq [k "abcdefghijklmnopqrstuvwxyz 01234567890!\"\\/£$%^&*()'~<>?@#_-+=[]{},."]
      (add-input-binding comp (keystroke k) (make-input-handler state (str k))))
    (doseq [k "ABCDEFGHIJKLMNOPQRSTUVWXYZ"]
      (add-input-binding comp (keystroke k) (make-input-handler state (str k))))
    (doseq [[^KeyEvent ke k] {KeyEvent/VK_LEFT "4"
                              KeyEvent/VK_RIGHT "6"
                              KeyEvent/VK_UP "2"
                              KeyEvent/VK_DOWN "8"
                              KeyEvent/VK_ESCAPE "Q"}]
      (add-input-binding comp (keystroke-from-keyevent ke) (make-input-handler state (str k))))))

(defn launch [state]
  (let [^JFrame frame (new-frame)
          ^JConsole jc (new-console)]
      (setup-input jc state) 
      (.add (.getContentPane frame) jc)
      (.pack frame)
      (.setVisible frame true)
      frame))

(def s {:game (atom nil)})

(defn main 
  "Main entry point to the demo, called directly from Java main() method in DemoApp"
  ([]
    (let [^JFrame frame (launch s)]
      (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE))))