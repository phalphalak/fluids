(ns fluids.demo1
  (:require [kachel.core :as grid]
            [clojure.pprint :refer [pprint]])
  (:import [javax.swing JFrame JPanel SpringLayout JButton ToolTipManager]
           [java.awt Color Dimension]
           [java.awt.event ActionListener]
           [java.util LinkedList]
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

(defmulti render-cell (fn [_ _ _ _ cell] (first (keys cell))))

(defmethod render-cell nil [g x y size cell])

(defmethod render-cell :terrain [g x y size cell]
  (doto g
    (.setColor (Color. 0 128 0))
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

(defn render [g sim width height]
  (let [grid-width (.width (sim :world))
        grid-height (.height (sim :world))
        cell-size (sim :cell-size)]
    (paint-background g width height)
    (doseq [y (range grid-height)
            x (range grid-width)]
      (render-cell g x y cell-size @(grid/coordinate->field (sim :world) [x y])))
    (paint-grid g cell-size grid-width grid-height)))

(defn create-grid-panel [simulation]
  (let [cs (:cell-size simulation)
        panel (proxy [JPanel] []
                (paintComponent [g]
                  (proxy-super paintComponent g)
                  (render g simulation (.getWidth this) (.getHeight this)))
                (getToolTipText [event]
                  (let [x (int (/ (.getX event) cs))
                        y (int (/ (.getY event) cs))]
                    (format "[%s %s] %s"
                            x y
                            (str @(grid/coordinate->field (:world simulation)
                                                          [x y]))))))]
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

(comment
  (defn process-queue [simulation queue]
    (let [world (:world simulation)]
      (loop []
        (when-not (.isEmpty queue)
          (let [item (.pop queue)]
            (prn item)
            (recur))))
      (prn queue))))

(defn flood-fill-pressure [simulation coords]
  (let [world (:world simulation)
        queue (LinkedList. [coords])]
    (loop []
      (let [[x y] (.pop queue)
            field-ref (grid/coordinate->field world [x y])
            field @field-ref
            pressure (get-in field [:water :temp :pressure] 0)
            up (grid/neighbour world [x y] :up)
            down (grid/neighbour world [x y] :down)
            left (grid/neighbour world [x y] :left)
            right (grid/neighbour world [x y] :right)]
        (pprint ["next:" [x y] {:field field
                                :field-ref field-ref
                                :up up
                                :down down
                                :left left
                                :right right}])
        (reset! field-ref (-> field
                              (assoc-in [:water :temp :pressure] pressure)
                              (assoc-in [:water :processed] true)))
        (when (and up
                   (:water @up)
                   (> pressure (inc (get-in @up [:water :temp :pressure] 0))))
          (prn :up)
          (reset! up (assoc-in field [:water :temp :pressure] (dec pressure)))
          (.push queue [x (dec y)]))
        (when (and down
                   (:water @down)
                   (> pressure (dec (get-in @down [:water :temp :pressure] 0))))
          (prn :down)
          (reset! down (assoc-in field [:water :temp :pressure] (inc pressure)))
          (.push queue [x (inc y)]))
        (when (and left
                   (:water @left)
                   (> pressure (get-in @left [:water :temp :pressure] 0)))
          (prn :left)
          (reset! left (assoc-in field [:water :temp :pressure] pressure))
          (.push queue [(dec x) y]))
        (when (and right
                   (:water @right)
                   (> pressure (get-in @right [:water :temp :pressure] 0)))
          (prn :right)
          (reset! right (assoc-in field [:water :temp :pressure] pressure))
          (.push queue [(inc x) y]))
        (when-not (.isEmpty queue)
          (recur))))))

(defn step [simulation]
  (let [world (:world simulation)]
    (doseq [y (range (.height world))
            x (range (.width world))]
      (let [field-ref (grid/coordinate->field world [x y])]
        (when-let [water (and (not (get-in @field-ref [:water :processed]))
                              (:water @field-ref))] (prn "!!!!"
                                                         (grid/coordinate->field world [x y])
                                                         water [x y])
          (flood-fill-pressure simulation [x y]))))))

(defn run [& args]
  (let [world (load-world "demo.edn")
        simulation {:world world
                    :cell-size 16}
        grid-panel (create-grid-panel simulation)
        control-panel (create-control-panel #(do (step simulation)
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
