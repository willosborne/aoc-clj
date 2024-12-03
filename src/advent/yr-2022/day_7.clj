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
            "ls" {:type :ls}
            "cd" {:type :cd
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


(defn parse-input [input]
  (map parse-line (str/split-lines input)))

(def commands (parse-input input))

(defn update-dir-for-file [dir command]
  (case (:type command)
    :file (assoc dir
                 :files (conj (:files dir) command))
    dir))

(comment
  (update-dir-for-file {:name "/"
                        :files []
                        :dirs []}
                       {:type :file
                        :size 123
                        :name "egg"})
  )

(defn translate-path [path]
  (interpose :dirs path))

(comment
  (translate-path ["/" "egg" "beans"])
  (update {"/" {:name "/"
                :dirs {"egg" {:name "egg"
                              :dirs {:name "beans"
                                     :dirs {}}}}}}
          (translate-path ["/" "egg" "beans"])
          #(assoc %
           :yolo
           :swag))
  )

(defn process-commands [commands]
  (loop [[command & cs] commands
         tree {"/" {:name "/"
                    :files []
                    :dirs {}}}
         path ["/"]]
    (if command
      (case (:type command)
       :cd (case (:arg command)
             ".." (recur cs
                         (pop stack)
                         (update (first stack) :size + (:size current-file)))
             (recur cs
                    (if (not stack)
                      []
                      (conj stack current-file)) ;; push the current file
                    {:size 0
                     :name (:arg command)
                     :files []}))

       :file (recur cs
                    (update tree
                            (translate-path path)
                            update-dir-for-file
                            command)
                    path)
       (recur cs stack current-file))
      [stack current-file])))
