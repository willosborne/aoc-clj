(defproject advent-2021 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [cprop "0.1.19"]
                 [org.clojure/java.jdbc "0.7.8"]
                 [org.xerial/sqlite-jdbc "3.23.1"]
                 [clj-http "3.12.3"]
                 [instaparse "1.4.12"]]
  :repl-options {:init-ns advent.core}
  :jvm-opts ["-Djdk.attach.allowAttachSelf"])
