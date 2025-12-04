(ns advent.yr-2025.day-2
  (:require [advent.core :refer [get-input]]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (get-input 2025 2))

(defn expand-range [r]
  (let [[start end] (str/split r #"-")]
    (range (Long/parseLong start) (inc (Long/parseLong end)))))

(defn split-half [s]
  (let [l (count s)
        s1 (subs s 0 (/ l 2))
        s2 (subs s (/ l 2) l)]
    [s1 s2]))

(defn valid? [num]
  (let [numstr (str num)]
    (not (apply = (split-half numstr)))))

(defn ans1 []
  (let [rangestrs (str/split (apply str (drop-last input)) #",")
        ranges (map expand-range rangestrs)
        counts (map #(filter valid? %) ranges)]
    (apply + (map #(apply + %) counts))))
