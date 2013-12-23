(ns fluids.demo1
  (:require [kachel.core :as grid]
            [clojure.edn :as edn])
  (:import [javax.swing JFrame JPanel SpringLayout JButton ToolTipManager]
           [java.awt Color Dimension]
           [java.awt.event ActionListener]
           [kachel.core SquareGrid]))

(defn paint-background [g width height]
  (.setColor g Color/BLACK)
  (.fillRect g 0 0 width height))

(defn paint-grid [g size width height]
  (.setColor g Color/GRAY)
  (doseq [x (map (partial * size) (range (inc width)))]
    (.drawLine g x 0 x (* height size)))
  (doseq [y (map (partial * size) (range (inc height)))]
    (.drawLine g 0 y (* width size) y)))

(defmulti render-cell (fn [_ _ _ _ cell] (:type (first cell))))

(defmethod render-cell nil [g x y size cell])

(defmethod render-cell :terrain [g x y size cell]
  (doto g
    (.setColor (Color. 0 128 0))
    (.fillRect (* size x)
               (* size y)
               size
               size)))

(defmethod render-cell :water [g x y size cell]
  (doto g
    (.setColor (Color. 192 192 255))
    (.fillRect (* size x)
               (* size y)
               size
               size)))

(defn render [g sim width height]
  (let [grid-width (.width @(sim :world))
        grid-height (.height @(sim :world))
        cell-size (sim :cell-size)]
    (paint-background g width height)
    (doseq [y (range grid-height)
            x (range grid-width)]
      (render-cell g x y cell-size @(grid/coordinate->field @(sim :world) [x y])))
    (paint-grid g cell-size grid-width grid-height)))

(defn create-grid-panel [simulation]
  (let [cs (:cell-size simulation)
        panel (proxy [JPanel] []
                (paintComponent [g]
                  (proxy-super paintComponent g)
                  (render g simulation (.getWidth this) (.getHeight this)))
                (getToolTipText [event]
                  (str @(grid/coordinate->field @(:world simulation)
                                                [(int (/ (.getX event) cs))
                                                 (int (/ (.getY event) cs))]))))]
    (.registerComponent (ToolTipManager/sharedInstance) panel)
    panel))

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

(defn step [world]
  (prn "Step"))

(defn run [& args]
  (let [world (load-world "demo.edn")
        _ (doseq [c [[20 5]
                     [15 20] [15 21] [15 22] [15 23] [15 24] [15 25]
                     [14 25] [13 25] [12 25] [11 25]
                     [10 25] [10 24] [10 23]]]
            (reset! (grid/coordinate->field world c) [{:type :water
                                                       :volume 1}]))
        simulation {:world (ref world)
                    :cell-size 16}
        grid-panel (create-grid-panel simulation)
        control-panel (create-control-panel #(step world))
        frame (JFrame. "Liquid test")
        layout (SpringLayout.)
        content-pane (.getContentPane frame)]
    (doto grid-panel
      (.setPreferredSize (Dimension. (* (:cell-size simulation)
                                        (.width world))
                                     (* (:cell-size simulation)
                                        (.height world)))))
    (doto content-pane
      (.setLayout layout)
      (.add grid-panel)
      (.add control-panel))
    (doto layout
      (.putConstraint SpringLayout/WEST grid-panel 5 SpringLayout/WEST content-pane)
      (.putConstraint SpringLayout/WEST control-panel 5 SpringLayout/WEST content-pane)
                                        ;      (.putConstraint SpringLayout/NORTH grid-panel 5 SpringLayout/NORTH content-pane)
      (.putConstraint SpringLayout/NORTH control-panel 5 SpringLayout/NORTH content-pane)
      (.putConstraint SpringLayout/NORTH grid-panel 5 SpringLayout/SOUTH control-panel)
      (.putConstraint SpringLayout/EAST content-pane 5 SpringLayout/EAST grid-panel)
      (.putConstraint SpringLayout/EAST control-panel 5 SpringLayout/EAST content-pane)
      (.putConstraint SpringLayout/SOUTH content-pane 5 SpringLayout/SOUTH grid-panel))
    (doto frame
      .pack
      (.setVisible true))))
