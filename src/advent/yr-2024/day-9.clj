(ns advent.yr-2024.day-9
  (:require [advent.core :refer [get-input]]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (get-input 2024 9))

(defn expand-line [line]
  (into [] (flatten
    (for [[n free? val] (map vector
                             (iterate inc 0)
                             (iterate not false)
                             (map (comp edn/read-string str) (str/trim line)))]
      (if free?
        (repeat val nil)
        (repeat val (quot n 2)))))))

(defn fill-last [line]
  ;; (println line)
  (if (not (last line))
    (pop line)
    (let [first-i (.indexOf line nil)
         last-i (dec (count line))]
     (assoc (pop line)
            first-i (nth line last-i)))))


(defn not-finished? [line]
  (some (fn [i]
          (and (not (nth line i))
               (any? (subvec line i))))
        (range (count line))))

(defn fill-line [line]
  (first (drop-while not-finished? (iterate fill-last line))))


(defn calc-checksum [line]
  (reduce + (map (fn [x y]
                   (* (or x 0)
                      (or y 0)))
                 line
                 (iterate inc 0))))

(defn run [line]
  (->> (expand-line line)
       (fill-line)
       ;; (calc-checksum)

       ))

(defn answer1 []
  (run (get-input 2024 9)))
