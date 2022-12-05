(ns advent.yr-2022.day-5
  (:require [advent.core :refer [get-input]]
            [instaparse.core :as insta]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (get-input 2022 5))
(def lines (str/split-lines input))

(def crate-line-parser (insta/parser "
<S> = ENTRY
<ENTRY> = (CRATE | GAP) <\" \"> ENTRY | (CRATE | GAP)
<CRATE> = <\"[\"> #\"[A-Z]\" <\"]\">
<GAP> = \"   \"
"))

(comment (crate-line-parser "[A] [B]     [C]")
         )

(defn parse-crates-rows []
  (take-while #(not (:reason %)) (map crate-line-parser lines)))

(defn rows-to-cols [rows]
  (let [transposed (apply map list rows)]
    (mapv (fn [col] (filter #(not (str/blank? %)) col))
         transposed)))

(defn move-crate
  ([stacks source dest n]
   (let [crates (vec (take n (nth stacks source)))]
     (-> stacks
         (update source #(drop n %))
         (update dest #(into crates %)))))
  ([stacks source dest]
   (move-crate stacks source dest 1)))

(def initial-crates (rows-to-cols (parse-crates)))

(def op-line-parser (insta/parser "
<S> = <\"move \"> #\"[0-9]+\" <\" from \"> #\"[0-9]+\" <\" to \"> #\"[0-9]+\"
"))

(defn parse-op [line]
  (mapv edn/read-string (op-line-parser line)))

(defn run-op [stacks op]
  (let [[n s d] op
        source (dec s)
        dest (dec d)]
    (nth (iterate #(move-crate % source dest) stacks) ;; apply move-crate N times
         n)))

(def ops (mapv parse-op (drop (+ 1 (count initial-crates)) lines)))

(defn answer-1 []
  (->> (loop [crates initial-crates
              [o & os] ops]
         (if o
           (recur (run-op crates o)
                  os)
           crates))
       (mapv first)
       (apply str)))

(defn run-op-2 [stacks op]
  (let [[n s d] op
        source (dec s)
        dest (dec d)]
    (move-crate stacks source dest n)))


(defn answer-2 []
  (->> (loop [crates initial-crates
              [o & os] ops]
         (if o
           (recur (run-op-2 crates o)
                  os)
           crates))
       (mapv first)
       (apply str)))
