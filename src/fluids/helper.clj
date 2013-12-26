(ns fluids.helper
  (:require [kachel.core :as grid]
            [clojure.pprint :refer [pprint
                                    *print-right-margin*
                                    *print-miser-width*]])
  (:import [kachel.core SquareGrid]
           [javax.swing JPanel ToolTipManager SpringLayout JButton]
           [java.awt.event ActionListener]
           [java.io StringWriter]
           [java.awt Color]))

(defn load-world [file-name]
  (let [data (read-string (slurp file-name))]
    (SquareGrid. (:width data)
                 (:height data)
                 (mapv atom (:fields data))
                 false false)))

(defn store-world [world file-name]
  (spit file-name (pr-str {:fields (map deref (.fields world))
                           :width (.width world)
                           :height (.height world)})))

(defn paint-background [g width height]
  (.setColor g Color/BLACK)
  (.fillRect g 0 0 width height))

(defn paint-grid [g size width height]
  (.setColor g Color/GRAY)
  (doseq [x (map (partial * size) (range (inc width)))]
    (.drawLine g x 0 x (* height size)))
  (doseq [y (map (partial * size) (range (inc height)))]
    (.drawLine g 0 y (* width size) y)))

(defn render [g sim width height cell-renderer]
  (let [grid-width (.width (sim :world))
        grid-height (.height (sim :world))
        cell-size (sim :cell-size)]
    (paint-background g width height)
    (doseq [y (range grid-height)
            x (range grid-width)]
      (cell-renderer g x y cell-size @(grid/coordinate->field (sim :world) [x y])))
    (paint-grid g cell-size grid-width grid-height)))

(defn pprint-str [s]
  (binding [*print-right-margin* 20]
           (let [w (StringWriter.)]
             (pprint s w)
             (.toString w))))

(defn create-grid-panel [simulation cell-renderer]
  (let [cs (:cell-size simulation)
        panel (proxy [JPanel] []
                (paintComponent [g]
                  (proxy-super paintComponent g)
                  (render g simulation (.getWidth this) (.getHeight this) cell-renderer))
                (getToolTipText [event]
                  (let [x (int (/ (.getX event) cs))
                        y (int (/ (.getY event) cs))]
                    (format "<html>%s</html>"
                            (clojure.string/replace
                             (clojure.string/replace (pprint-str {:coords [x y]
                                                                  :content @(grid/coordinate->field (:world simulation)
                                                                                                    [x y])})
                                                     #"\n" "<br>")
                             #" " "&nbsp;")))))]
    (.setDismissDelay (ToolTipManager/sharedInstance) Integer/MAX_VALUE)
    (.registerComponent (ToolTipManager/sharedInstance) panel)
    panel))

(defn create-control-panel [step-fn]
  (let [panel (JPanel.)
        layout (SpringLayout.)
        step-button (JButton. "Step")
        play-button (JButton. "Run")
        stop-button (JButton. "Stop")]
    (doto step-button
      (.addActionListener (proxy [ActionListener] []
                            (actionPerformed [event]
                              (step-fn)))))
    (doto panel
      (.setLayout layout)
      (.add step-button)
      (.add play-button)
      (.add stop-button))
    (doto layout
      (.putConstraint SpringLayout/WEST step-button 5 SpringLayout/WEST panel)
      (.putConstraint SpringLayout/NORTH step-button 5 SpringLayout/NORTH panel)
      (.putConstraint SpringLayout/NORTH play-button 5 SpringLayout/NORTH panel)
      (.putConstraint SpringLayout/NORTH stop-button 5 SpringLayout/NORTH panel)
      (.putConstraint SpringLayout/SOUTH panel 5 SpringLayout/SOUTH step-button)
      (.putConstraint SpringLayout/WEST play-button 5 SpringLayout/EAST step-button)
      (.putConstraint SpringLayout/WEST stop-button 5 SpringLayout/EAST play-button))
    panel))