(ns advent.yr-2022.day-4
  (:require [advent.core :refer [get-input]]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def input (get-input 2022 4))

(defn fully-contains-one-way? [a b]
  (let [[a1 a2] a
        [b1 b2] b]
    (and (<= a1 b1)
         (>= a2 b2))))

(defn fully-contains? [a b]
  (or (fully-contains-one-way? a b)
      (fully-contains-one-way? b a)))

(def lines (str/split-lines input))

(defn parse-line [line]
  (let [[ar br] (str/split line #",")
        a (map edn/read-string (str/split ar #"-"))
        b (map edn/read-string (str/split br #"-"))]
    [a b]))


(defn answer-1 []
  (let [parsed-lines (map parse-line lines)]
    (count (filter #(apply fully-contains? %) parsed-lines))))

(defn overlaps? [a b]
  (let [[a1 a2] a
        [b1 b2] b]
    (<= (max a1 b1)
        (min a2 b2))))

(defn answer-2 []
  (let [parsed-lines (map parse-line lines)]
    (count (filter #(apply overlaps? %) parsed-lines))))
