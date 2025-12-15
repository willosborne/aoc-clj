(ns advent.yr-2025.day-4
  (:require [advent.core :refer [get-input]]
            [advent.utils :as utils]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (get-input 2025 4))

(defn parse-grid [input]
  (mapv vec (str/split-lines input)))

(defn get-surrounding-rolls [grid x y]
  (->> (utils/get-neighbours grid x y)
       (filter #(= \@ (:val %)))))
