(defproject jardingaard "0.0.1"
  :description "Jardingaard"
  :jvm-opts ["-server"]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [quil/jogl-all-fat "2.3.2"]
                 [quil/gluegen-rt-fat "2.3.2"]
                 [seesaw "1.4.4"]]
  :main jardingaard.client
  :aot [jardingaard.standalone
        jardingaard.client])
