(ns advent.yr-2022.day-10
  (:require [advent.core :refer [get-input]]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn parse-instruction [line]
  (let [[op & commands] (str/split line #" ")]
    (apply vector (keyword op) (map edn/read-string commands))))

(def instructions
  (->> (get-input 2022 10)
      (str/split-lines)
      (map parse-instruction)))

(def instruction-cycles
  {:noop 1
   :addx 2})
