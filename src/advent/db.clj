(ns advent.db
  (:require [advent.config :refer [env]]
            [clojure.java.jdbc :refer :all]))

(def db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname (:db-file env)})

(defn get-input [year day]
  (-> (query db ["SELECT * FROM inputs WHERE year = ? AND day = ?" year day]
             {:row-fn :text})
      (first)))

(defn insert-input [year day text]
  (insert! db :inputs {:year year
                       :day day
                       :text text}))
