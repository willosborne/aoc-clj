(ns advent.yr-2024.day-5
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [advent.core :refer [get-input]]
            ))

(def input (str/split (get-input 2024 5) #"\n\n"))

(def rules (mapv #(mapv edn/read-string (str/split % #"\|")) (str/split-lines (nth input 0))))
(def pages (mapv #(mapv edn/read-string (str/split % #",")) (str/split-lines (nth input 1))))

(defn rule-valid [pageset rule]
  (let [[before after] rule]
    ;; if either is not defined, we're good
    (let [i1 (.indexOf pageset before)
          i2 (.indexOf pageset after)]
      (or (= i1 -1)
          (= i2 -1)
          (< i1 i2)))
    )
  )

(defn page-set-valid [pageset rules]
  (every?
   #(rule-valid pageset %)
   rules))

(comment
  (page-set-valid [1 2 3 4] [[1 2] [3 4]])
  (page-set-valid [1 2 3 4] [[1 2] [4 3]])
  (page-set-valid [1 2 3 4] [[1 2] [5 4]])


  )

(defn get-middle [pageset]
  (nth pageset (quot (count pageset) 2)))

(defn answer1 []
  (->> pages
       (filter #(page-set-valid % rules))
       (mapv get-middle)
       (reduce +)))

(defn swap [vec i1 i2]
  (assoc vec
         i1 (nth vec i2)
         i2 (nth vec i1))
  ;; second 'smarter' implementation that isn't needed at all
  ;; (println "swapping " i1 i2)
  ;; (let [o1 (min i1 i2)
  ;;       o2 (max i1 i2)]
  ;;   (into []
  ;;        (concat (subvec vec 0 o1)
  ;;                [(nth vec o2)
  ;;                 ;; (nth vec i1)
  ;;                 ]
  ;;                (subvec vec o1 o2)
  ;;                (subvec vec (inc o2)))))
  )

(defn fix-page-set [rules pageset]
  (loop [[r & rs] rules
         ps pageset]
    (if-not r
      ps
      (if (rule-valid ps r)
        (recur rs ps)
        (let [[r1 r2] r
              i1 (.indexOf pageset r1)
              i2 (.indexOf pageset r2)]
          (swap ps i1 i2))))))

(defn run-fix-pageset [rules pageset]
  (first (filter #(page-set-valid % rules)
                 (iterate #(fix-page-set rules %) pageset))))

(defn answer2 []
  (->> pages
       (filter #(not (page-set-valid % rules)))
       (mapv #(run-fix-pageset rules %))
       (mapv get-middle)
       (reduce +)))
