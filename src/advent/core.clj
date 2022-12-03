(ns advent.core
  (:require [advent.config :refer [env]]
            [advent.db :as db]
            [clj-http.client :as client]
            [clj-http.cookies :as cookies]))


(defn format-url [year day]
  (format "https://adventofcode.com/%d/day/%d/input" year day))

(def cookie-map
  {"session" {:domain ".adventofcode.com"
              :value (:session-key env)
              :discard false}})

(defn- fetch-input [year day]
  (print "Fetching " year ", day " day)
  (-> (format-url year day)
      (client/get {:cookies cookie-map})
      (:body)))

(defn get-input [year day]
  (or (db/get-input year day)
      (let [input (fetch-input year day)]
        (db/insert-input year day input)
        input)))

