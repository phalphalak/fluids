(ns fluids.demo1
  (:require [kachel.core :as grid]
            [clojure.pprint :refer [pprint]]
            [fluids.helper :as helper])
  (:import [javax.swing JFrame SpringLayout]
           [java.awt Color Dimension]
           [java.util LinkedList]))

(defmulti render-cell (fn [_ _ _ _ cell] (first (keys cell))))

(defmethod render-cell nil [g x y size cell])

(defmethod render-cell :terrain [g x y size cell]
  (doto g
    (.setColor (Color. 64 160 48))
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
