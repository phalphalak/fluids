(ns fluids.core
  (:require [fluids.demo1 :as demo1]
            [fluids.demo2 :as demo2])
  (:gen-class))

(defn -main [& args]
  (demo2/run))
