(ns fluids.core
  (:require [fluids.demo1 :as demo1])
  (:gen-class))

(defn -main [& args]
  (demo1/run))
