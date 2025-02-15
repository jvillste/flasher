(defproject flasher "1"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [flow-gl/flow-gl "4"]
                 [medley "1.3.0"]
                 [clojure.java-time "0.3.3"]]
  :repl-options {:init-ns flasher.core}
  :main flasher.times-table
  :profiles {:uberjar {:aot :all}}
  :clean-targets ^{:protect false} ["targets"])
