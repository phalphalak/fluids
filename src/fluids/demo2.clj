(ns fluids.demo2
  (:require [kachel.core :as grid]
            [clojure.pprint :refer [pprint]]
            [fluids.helper :as helper])
  (:import [javax.swing JFrame SpringLayout JButton]
           [java.awt Color Dimension]
           [java.awt.event ActionListener]
           [java.util LinkedList]))

(defmulti render-cell (fn [_ _ _ _ cell] (first (keys cell))))

(defmethod render-cell nil [g x y size cell])

(defmethod render-cell :terrain [g x y size cell]
  (doto g
    (.setColor (Color. 164 160 148))
    (.fillRect (* size x)
               (* size y)
               size
               size)))

(defmethod render-cell :water [g x y size cell]
  (let [pressure (get-in cell [:water :temp :pressure] 0)]
    (doto g
      (.setColor (Color. (int (- 192 (* 18 pressure)))
                         (int (- 192 (* 18 pressure)))
                         255))
      (.fillRect (* size x)
                 (* size y)
                 size
                 size))))

(defn step [simulation]
  (let [world (:world simulation)]
    (doseq [y (range (.height world))
            x (range (.width world))]
      (let [field-ref (grid/coordinate->field world [x y])]
        (when-let [water (:water @field-ref)] (prn "!!!!"
                                                   (grid/coordinate->field world [x y])
                                                   water [x y]))))))

(defn run [& args]
  (let [world (helper/load-world "demo.edn")
        simulation {:world world
                    :cell-size 16}
        grid-panel (helper/create-grid-panel simulation render-cell)
        control-panel (helper/create-control-panel #(do (step simulation)
                                                 (.repaint grid-panel)))
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
