(ns advent.config
  (:require [cprop.core :refer [load-config]]))

(def env
  (load-config :resource "env.edn"))
