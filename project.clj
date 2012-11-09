(defproject lom "1.0.0-SNAPSHOT"
  :description "League of Mundanes"
  :jvm-opts ["-Dsun.java2d.pmoffscreen=true"
             "-Dsun.java2d.opengl=false"
             "-Dsun.java2d.trace=count"]
  :extra-classpath-dirs ~(.listFiles (File. "jogamp"))
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [seesaw "1.4.0"]])