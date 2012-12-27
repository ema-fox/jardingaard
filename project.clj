(defproject jardingaard "0.0.1"
  :description "Jardingaard"
  :jvm-opts ["-server"]
  :extra-classpath-dirs ~(.listFiles (File. "jogamp"))
  :dependencies [[org.clojure/clojure "1.5.0-RC1"]
                 [seesaw "1.4.0"]])