(defproject gent-clj "0.1.0-SNAPSHOT"
  :description "(a)gent"
  :url "git@github.com:merisbahti/gent-clj.git"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0",
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [clj-http "3.13.1"]
                 [org.clojure/data.json "2.5.1"]
                 [metosin/malli "0.19.1"]]
  :main gent-clj.core
  :repl-options {:init-ns gent-clj.core}
  :test-paths ["test" "src"])
