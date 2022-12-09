(ns advent.yr-2022.day-7
  (:require [advent.core :refer [get-input]]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [instaparse.core :as insta]))


(def input (get-input 2022 7))

(def lines (str/split-lines input))

(defn parse-line [line]
  (let [words (str/split line #" ")
        [fst snd thrd] words]
    (case fst
      "$" (case snd
            "ls" {:type :command
                  :command :ls}
            "cd" {:type :command
                  :command :cd
                  :arg thrd})
      "dir" {:type :dir
             :name snd}
      {:type :file
       :name snd
       :size (edn/read-string fst)})))

(comment
  (parse-line "$ cd /")
  (parse-line "$ ls")
  (parse-line "1234 guh")
  (parse-line "dir egg")
  )


(def tree-parser (insta/parser "
cd = <\"$\"> 
<space> = \" \"
"))
