(ns lom.client
  (:gen-class)
  (:use [seesaw core]
        [clojure.java io]
        [lom util shared])
  (:import [java.net Socket]
           [java.util Date]
           [java.io OutputStreamWriter InputStreamReader]
           [java.awt Color Dimension Graphics2D RenderingHints Transparency Image Frame Font]
           [java.awt.image BufferedImage]
           [java.awt.event KeyEvent MouseEvent]
           [javax.media.opengl.awt GLCanvas]
           [javax.media.opengl GLEventListener GL GLCapabilities GLProfile
            GL2 GLAutoDrawable GLDrawableFactory]
           [com.jogamp.opengl.util.awt TextRenderer]
           [com.jogamp.opengl.util.texture Texture TextureIO]
           [clojure.lang LineNumberingPushbackReader]))

(set! *warn-on-reflection* true)

(def tsz 32)

(defn fill-rect! [^GL2 gl [p0 p1] [s0 s1]]
  (.glTexCoord2f gl 0 0)
  (.glVertex2f gl p0 p1)
  (.glTexCoord2f gl 0 1)
  (.glVertex2f gl p0 (+ p1 s1))
  (.glTexCoord2f gl 1 1)
  (.glVertex2f gl (+ p0 s0) (+ p1 s1))
  (.glTexCoord2f gl 1 0)
  (.glVertex2f gl (+ p0 s0) p1))

(defn draw-string! [^TextRenderer rnd ^String s [p0 p1] [s0 s1]]
  (.draw rnd s (int p0) (int (- s1 p1))))

(defn set-color! [^GL2 gl red green blue]
  (.glColor3f gl (/ red 255.0) (/ green 255.0) (/ blue 255.0)))

(defn draw-image! [^Graphics2D g ^BufferedImage img [p0 p1]]
  (.drawImage g img (int p0) (int p1) nil))

(def fr-ts (ref '()))

(def bgimgs (ref {}))

(def conn)

(def data (ref {}))

(def txtr)

(def tile-cs {:wall [240 60 30]
              :windowed-wall [170 50 5]
              :dirt [255 255 255]
              :door [60 50 10]
              :gras [255 255 255]
              :shrub [255 255 255]
              nil [200 0 200]})

(defn repaint-tile! [p]
  (let [chunkp (mult (map int (div p 25)) 25)
        relp (minus p chunkp)
        ^BufferedImage img (@bgimgs chunkp)]
    (if img
      (let [world (:world @data)
            ^Graphics2D g (.createGraphics img)]
        (condp = (get-in world p)
          :wall (.setColor g (Color. 240 60 30))
          :windowed-wall (.setColor g (Color. 170 50 5))
          :dirt (.setColor g (Color. 100 80 40))
          :door (.setColor g (Color. 60 50 10))
          :gras (.setColor g (Color. 20 200 60))
          :shrub (.setColor g (Color. 50 160 20))
          (set-color! g 255 0 255))
        (fill-rect! g (mult relp 20) [20 20])
        (.dispose g)))))

(defn repaint-bgimg! []
  (doseq [p0 (range (count (:world @data)))
          p1 (range (count (:world @data)))]
    (repaint-tile! [p0 p1])))

(defn prepare-gl! [^GL2 gl trans size]
  (.glMatrixMode gl GL2/GL_MODELVIEW)
  (.glLoadIdentity gl)
  (.glTranslatef gl -1 1 0)
  (let [[a0 a1] size
        [b0 b1] trans]
    (.glViewport gl 0 0 a0 a1)
    (.glScalef gl (/ 2 a0) (/ -2 a1) 1)
    (.glTranslatef gl b0 b1 0))
  (.glClearColor gl 0.5 0.5 0.5 1)
  (.glClear gl GL/GL_COLOR_BUFFER_BIT)
  (.glEnable gl GL/GL_BLEND)
  (.glBlendFunc gl GL/GL_SRC_ALPHA GL/GL_ONE_MINUS_SRC_ALPHA))

(defn render [^GL2 gl size]
  (dosync
   (ref-set fr-ts (conj (take 50 @fr-ts) (.getTime (Date.)))))
  (if (:players @data)
    (let [{:keys [players bullets world c-sites hello world]} @data
          pfoo (round (get-in players [hello :p]))
          offset (minus [320 320] (mult (get-in players [hello :p]) tsz))]
      (prepare-gl! gl offset size)
      (.glEnable gl GL2/GL_TEXTURE_2D)
      (set-color! gl 255 255 255)
      (doseq [[bg tiles] (group-by #(get-in world %)
                                   (for [p0 (range (max 0 (- (first pfoo) 11))
                                                   (min (count world) (+ (first pfoo) 15)))
                                         p1 (range (max 0 (- (second pfoo) 11))
                                                   (min (count world) (+ (second pfoo) 15)))]
                                     [p0 p1]))
              :let [^Texture tex (txtr bg)]
              :when tex]
        (.bind tex gl)
        (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MIN_FILTER GL2/GL_NEAREST)
        (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MAG_FILTER GL2/GL_NEAREST)
        (.glBegin gl GL2/GL_QUADS)
        (doseq [p tiles]
          (fill-rect! gl (mult p tsz) [tsz tsz]))
        (.glEnd gl))
      (.glDisable gl GL2/GL_TEXTURE_2D)
      (.glBegin gl GL2/GL_QUADS)
      (set-color! gl 128 128 128)
      (fill-rect! gl (minus [780 0] offset) [50 800])
      (doseq [{:keys [p t]} c-sites]
        (set-color! gl 0 0 0)
        (fill-rect! gl (mult p tsz) [(inc (/ t 5)) 5]))
      (set-color! gl 0 0 0)
      (doseq [[pid {:keys [p hp]}] players]
        (set-color! gl 150 20 50)
        (fill-rect! gl (minus (mult p tsz) [0 10]) [hp 5])
        (set-color! gl 0 0 0)
        (fill-rect! gl (mult p tsz) [tsz tsz]))
      (doseq [{p :p} bullets]
        (fill-rect! gl (plus [12 12] (mult p tsz)) [5 5]))
      (.glEnd gl)
      (let [rnd (TextRenderer. (Font. "SansSerif" Font/PLAIN 12))]
        (.beginRendering rnd (first size) (second size))
        (.setColor rnd 0 0 0 1)
        (doseq [[pid {:keys [p inventar inventar-p died name]}] players]
          (draw-string! rnd (str name (if (= pid hello)
                                        (apply str (map-indexed (fn [i x]
                                                                  (if (= i inventar-p)
                                                                    (str " (" x ")")
                                                                    (str " "  x)))
                                                                inventar))
                                        (str " " (nth inventar inventar-p)))
                                 " - " died)
                        (plus offset (minus (mult p tsz) [0 20]))
                        size))
        (draw-string! rnd (if (< 2 (count @fr-ts))
                            (str (if-not (= (first @fr-ts) (last @fr-ts))
                                   (int (/ 1000 (/ (- (first @fr-ts) (last @fr-ts))
                                                   (count @fr-ts))))
                                   "oo")
                                 (let [foo (map (fn [[a b]]
                                                  (Math/abs ^Integer (- a b)))
                                                (partition 2 1 @fr-ts))]
                                   (str " " (apply min foo)
                                        " " (int (/ (apply + foo) (count foo)))
                                        " " (apply max foo))))
                            "---")
                      [10 10]
                      size)
        (.endRendering rnd)
        (.dispose rnd))
      (.glFlush gl))))

(defn msg [m]
  (binding [*out* (OutputStreamWriter. (.getOutputStream conn))]
    (prn m)))

(defn key-pressed [e]
  (condp = (.getKeyCode e)
    KeyEvent/VK_E (msg [:enhance])
    KeyEvent/VK_W (msg [:incip])
    KeyEvent/VK_Q (msg [:decip])
    KeyEvent/VK_ESCAPE (System/exit 0)
    nil))

(defn get-event-p [e]
  (-> [(.getX e) (.getY e)]
      (minus [(/ tsz 2) (/ tsz 2)])
      (div tsz)
      (plus (minus (get-in @data [:players (:hello @data) :p]) [10 10]))))

(defn mouse-pressed [e]
  (condp = (.getButton e)
    MouseEvent/BUTTON1
    (msg [:shot (get-event-p e)])
    MouseEvent/BUTTON3
    (msg [:walk (get-event-p e)])
    nil))

(def size (ref [0 0]))

(defn load-textures! []
  (def txtr (into {} (map (fn [name]
                            [(keyword name)
                             (TextureIO/newTexture (file (str name ".png")) false)])
                          ['gras 'dirt 'shrub 'door 'wall 'windowed-wall]))))

(defn -main [& [host]]
  (def conn (loop []
              (if-let [c (try
                           (Socket. (or host "localhost") 8282)
                           (catch java.net.ConnectException e
                             (Thread/sleep 1000)
                             nil))]
                c
                (recur))))
  (let [can (doto (GLCanvas.)
              (.setPreferredSize (Dimension. 600 600)))
        fr (doto (Frame.)
             (.add can))
        r (LineNumberingPushbackReader. (InputStreamReader. (.getInputStream conn)))]
    (.addGLEventListener can (proxy [GLEventListener] []
                               (init [d]
                                 (load-textures!))
                               (display [^GLAutoDrawable d]
                                 (let [gl (.getGL2 (.getGL d))]
                                   (render gl @size)))
                               (reshape [d x y w h]
                                 (dosync
                                  (ref-set size [w h])))))
    (listen can
            :key-pressed key-pressed
            :mouse-pressed mouse-pressed)
    (show! fr)
    (request-focus! can)
    (loop [d {}]
      (dosync
       (cond (map? d)
             (do
               (alter data merge d)
               (cond (:world d)
                     (repaint-bgimg!)
                     (:players d)
                     (repaint! can)))
             (vector? d)
             (condp = (first d)
               :world-tile (do
                             (alter data assoc-in (cons :world (second d)) (nth d 2))
                             (repaint-tile! (second d)))
               :frame-count (msg d))
             (and (future? d)
                  (:players @data))
             (do
               (alter data merge (step-bullets&players @data))
               (repaint! can))))
      (recur (let [fut (if (future? d)
                         d
                         (future (read r)))]
               (deref fut 33 fut))))))