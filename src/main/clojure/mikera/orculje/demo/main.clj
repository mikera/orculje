(ns mikera.orculje.demo.main
  (:use mikera.cljutils.error)
  (:use mikera.orculje.gui)
  (:import [javax.swing JFrame  JComponent])
  (:import [java.awt Font])
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
      jc)))


(defn main 
  "Main entry point to the demo, called directly from Java main() method in DemoApp"
  ([]
    (let [^JFrame frame (new-frame)
          ^JConsole jc (new-console)]
      (.add (.getContentPane frame) jc)
      (.pack frame)
      (.setVisible frame true))))