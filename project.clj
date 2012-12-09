(defproject jardingaard "0.0.1"
  :description "Jardingaard"
  :extra-classpath-dirs ~(.listFiles (File. "jogamp"))
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [seesaw "1.4.0"]])