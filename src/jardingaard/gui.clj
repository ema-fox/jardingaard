(ns jardingaard.gui
  (:use [jardingaard util helpers]
        [clojure.java io]
        [seesaw core])
  (:import [java.util Date]
           [java.nio FloatBuffer]
           [java.awt Color Dimension Graphics2D RenderingHints Transparency Image Frame Font]
           [java.awt.image BufferedImage]
           [java.awt.event KeyEvent MouseEvent]
           [com.jogamp.opengl.awt GLCanvas]
           [com.jogamp.opengl GLEventListener GL GLCapabilities GLProfile
            GL2 GLAutoDrawable GLDrawableFactory]
           [com.jogamp.opengl.util.awt TextRenderer]
           [com.jogamp.opengl.util.texture Texture TextureIO]
           [com.jogamp.common.nio Buffers]))

(declare txtr ^TextRenderer rnd)

(def tsz 128)
(def dtsz (* tsz 2))
(def qtsz (* tsz 4))
(def halftsz (/ tsz 2))
(def quarttsz (/ tsz 4))
(def hp-offset (+ halftsz 10))

(def isz 32)

(def fr-ts (ref '()))

(def hello (ref nil))

(def build-mode (ref false))
(def build-p (ref :dirt))

(def dbg-info (atom nil))

(defn ->gui [[p0 p1]]
  (let [foo [(* (- p0 p1) halftsz)
             (* (+ p0 p1) quarttsz)]]
    foo))

(defn <-gui [[p0 p1]]
  [(+ (/ p0 tsz) (/ p1 halftsz))
   (- (/ p1 halftsz) (/ p0 tsz))])

(defn config-gui [cs apc pr sv]
  (def current-state cs)
  (def add-plcmd apc)
  (def possible-recipes pr)
  (def save! sv))

(defn add-rect! [^FloatBuffer buf [p0 p1] [s0 s1]]
  (let [p0 (float p0)
        p1 (float p1)
        s0 (float s0)
        s1 (float s1)]
    (dounroll [n [p0 p1
                  p0 (+ p1 s1)
                  (+ p0 s0) (+ p1 s1)
                  (+ p0 s0) p1]]
      (.put buf (float n)))))

(defn add-tex-coords! [^FloatBuffer buf]
  (dounroll [f [0.0 1.0
                0.0 0.0
                1.0 0.0
                1.0 1.0]]
    (.put buf (float f))))

(defn draw-string! [^TextRenderer rnd ^String s [p0 p1] [s0 s1]]
  (.draw rnd s (int p0) (int (- s1 p1))))

(defn set-color!
  ([^GL2 gl red green blue]
   (.glColor3f gl (/ red 255.0) (/ green 255.0) (/ blue 255.0)))
  ([^GL2 gl red green blue alpha]
   (.glColor4f gl (/ red 255.0) (/ green 255.0) (/ blue 255.0) (/ alpha 255.0))))


(defn prepare-gl! [^GL2 gl trans size]
  (.glMatrixMode gl GL2/GL_MODELVIEW)
  (.glLoadIdentity gl)
  (.glTranslatef gl -1 1 0)
  (let [[a0 a1] size
        [b0 b1] trans]
    (.glViewport gl 0 0 a0 a1)
    (.glScalef gl (/ 2 a0) (/ -2 a1) 1)
    (.glTranslatef gl b0 b1 0)))

(defn load-tex [^GL2 gl ^Texture tex]
  (.bind tex gl)
  (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MIN_FILTER GL2/GL_NEAREST)
  (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MAG_FILTER GL2/GL_NEAREST))

(def ^FloatBuffer vert-buf (Buffers/newDirectFloatBuffer 0))
(def ^FloatBuffer texc-buf (Buffers/newDirectFloatBuffer 0))

(defn draw-rects! [^GL2 gl tex rects]
  (let [nrects (count rects)]
    (if-not (= 0 nrects)
      (let [nverts (* 4 nrects)]
        (when (< (.capacity vert-buf) (* 2 nverts))
          (def ^FloatBuffer vert-buf (Buffers/newDirectFloatBuffer (* 2 nverts))))
        (when (< (.capacity texc-buf) (* 2 nverts))
          (def ^FloatBuffer texc-buf (Buffers/newDirectFloatBuffer (* 2 nverts))))
        (.clear vert-buf)
        (.clear texc-buf)
        (dotimes [_ nrects]
          (add-tex-coords! texc-buf))
        (doseq [[p size] rects]
          (add-rect! vert-buf p size))
        (load-tex gl tex)
        (.flip vert-buf)
        (.flip texc-buf)
        (.glVertexPointer gl 2 GL2/GL_FLOAT 0 vert-buf)
        (.glTexCoordPointer gl 2 GL2/GL_FLOAT 0 texc-buf)
        (.glDrawArrays gl GL2/GL_QUADS 0 nverts)))))

(defn draw-beings! [gl tex tiles]
  (draw-rects! gl tex (for [p tiles]
                        [(minus (->gui p) [quarttsz halftsz]) [halftsz halftsz]])))

(defn draw-building! [gl tex p]
  (draw-rects! gl tex [[(minus (->gui p) [tsz (+ halftsz tsz)]) [dtsz dtsz]]]))

(defn draw-double-tiles! [gl tex tiles]
  (draw-rects! gl tex (for [p tiles]
                        [(minus (->gui p) [dtsz (+ tsz dtsz)]) [qtsz qtsz]])))

(defn fill-rects! [^GL2 gl rects]
  (let [nrects (count rects)]
    (if-not (= 0 nrects)
      (let [nverts (* 4 nrects)]
        (when (< (.capacity vert-buf) (* 2 nverts))
          (def ^FloatBuffer vert-buf (Buffers/newDirectFloatBuffer (* 2 nverts))))
        (.clear vert-buf)
        (doseq [[p s] rects]
          (add-rect! vert-buf p s))
        (.flip vert-buf)
        (.glVertexPointer gl 2 GL2/GL_FLOAT 0 vert-buf)
        (.glDrawArrays gl GL2/GL_QUADS 0 nverts)))))

(defn draw-lines! [^GL2 gl ps]
  (if (< 1 (count ps))
    (let [vbuf (Buffers/newDirectFloatBuffer (* 2 (count ps)))]
      (doseq [[p0 p1] ps]
        (.put vbuf (float p0))
        (.put vbuf (float p1)))
      (.flip vbuf)
      (.glVertexPointer gl 2 GL2/GL_FLOAT 0 vbuf)
      (.glDrawArrays gl GL2/GL_LINE_STRIP 0 (count ps)))))

(defn render [^GL2 gl size]
  (dosync
   (ref-set fr-ts (conj (take 50 @fr-ts) (.getTime (Date.)))))
  (.glClearColor gl 0.5 0.5 0.5 1)
  (.glClear gl GL/GL_COLOR_BUFFER_BIT)
  (let [{:keys [players bullets world buildings zombies chests tick lumberjacks carpenters arrows]} (current-state)]
    (binding [*tick* tick]
    (when (get players @hello)
      (let [pfoo (->tilep (get-in players [@hello :p]))
            offset (minus (mult size 0.5) (->gui (get-in players [@hello :p])))
            {:keys [p path]} (players @hello)]
        (prepare-gl! gl offset size)
        (set-color! gl 255 255 255)
        (.glEnable gl GL2/GL_TEXTURE_2D)
        (.glEnableClientState gl GL2/GL_VERTEX_ARRAY)
        (.glEnableClientState gl GL2/GL_TEXTURE_COORD_ARRAY)
        (let [mp (get-map-part world pfoo (plus [1 1] (->tilep (div size halftsz))))
              bp (get-map-part buildings pfoo (plus [1 1] (->tilep (div size halftsz))))]
          (doseq [{:keys [p ground]} (sort-by :p mp)]
            (if-let [tex (txtr ground)]
              (draw-double-tiles! gl tex [p]))
            (doseq [bl (->> (map-part buildings p [2 2])
                            (sort-by :p))]
              (if-let [tex (txtr (:type bl))]
                (draw-building! gl tex (:p bl))))
            (draw-beings! gl (txtr :player) (filter #(= p (->tilep %)) (map :p players)))
            (draw-beings! gl (txtr :player) (filter #(= p (->tilep %)) (map :p lumberjacks)))
            (draw-beings! gl (txtr :player) (filter #(= p (->tilep %)) (map :p carpenters)))
            (draw-beings! gl (txtr :zombie) (filter #(= p (->tilep %)) (map :p zombies))))
          (.glDisable gl GL2/GL_TEXTURE_2D)
          (.glDisableClientState gl GL2/GL_TEXTURE_COORD_ARRAY)
          (set-color! gl 90 40 25)
          (doseq [a arrows]
            (draw-lines! gl [(minus (->gui (:p a)) [0 quarttsz])
                             (minus (->gui (plus (:p a) (mult (direction (:p a) (:p (zombies (:target a)))) 0.1))) [0 quarttsz])]))
          (set-color! gl 250 220 25)
          (fill-rects! gl (for [tile bp
                                :when (:type tile)]
                            [(minus (->gui (:p tile)) [halftsz halftsz]) [(* (spawn-progress tile) tsz) 5]]))
          (set-color! gl 250 20 25)
          (fill-rects! gl (for [tile bp
                                :when (:broken tile)]
                            [(minus (->gui (:p tile)) [halftsz hp-offset]) [(* (broken-progress tile) tsz) 5]]))

          (fill-rects! gl (for [tile bp
                                :when (and (not (:broken tile))
                                           (:hp tile))]
                            [(minus (->gui (:p tile)) [halftsz hp-offset]) [tsz 5]]))
          (fill-rects! gl (for [zb zombies]
                            [(minus (->gui (:p zb)) [quarttsz hp-offset]) [halftsz 5]]))
          (set-color! gl 40 120 20)
          (fill-rects! gl (for [tile bp
                                :when (and (not (:broken tile))
                                           (:hp tile))]
                            [(minus (->gui (:p tile)) [halftsz hp-offset]) [(* (health-fraction tile) tsz) 5]]))

          (fill-rects! gl (for [zb zombies]
                            [(minus (->gui (:p zb)) [quarttsz hp-offset]) [(* (health-fraction zb) halftsz) 5]]))
          (set-color! gl 220 180 20)
          (draw-lines! gl (for [pp (cons p path)]
                            (->gui pp)))
          (.glTranslatef gl (- 0 (first offset)) (- 0 (second offset)) 0)
          (.glDisableClientState gl GL2/GL_VERTEX_ARRAY)
          (.beginRendering rnd (first size) (second size))
          (.setColor rnd 1 1 0.5 1)
          (doseq [{:keys [type merit p]} bp
                  :when (= type :idol)]
            (draw-string! rnd (str merit) (minus (plus (->gui p) offset) [20 tsz]) size))
          (doseq [{:keys [p merit name]} players]
            (draw-string! rnd (str name " - " merit)
                          (minus (plus (->gui p) offset) [20 halftsz]) size))
          (.endRendering rnd)
          (.glEnableClientState gl GL2/GL_VERTEX_ARRAY))
        (let [{:keys [inventar inventar-p hp energy]}
              (players @hello)
              active-slots (get-active-slots inventar inventar-p)
              ninventar (count active-slots)
              pa [(- (first size) 40) (- (/ (second size) 2) (* ninventar 20))]
              receipes (possible-recipes)
              bs (get-active-slots receipes @build-p)
              mid1 (- (/ (second size) 2) 100)]
          (set-color! gl 50 5 15)
          (fill-rects! gl [[[0 mid1] [20 200]]])
          (set-color! gl 150 15 50)
          (fill-rects! gl [[[0 mid1] [20 (* hp 10)]]])
          (set-color! gl 50 40 5)
          (fill-rects! gl [[[20 mid1] [20 200]]])
          (set-color! gl 150 120 15)
          (fill-rects! gl [[[20 mid1] [20 energy]]])
          (set-color! gl 20 40 10)
          (fill-rects! gl (cons [pa [40 (* ninventar 40)]]
                                (if @build-mode
                                  [[(minus pa [40 0]) [40 (* (count bs) 40)]]])))
          (set-color! gl 40 80 20)
          (fill-rects! gl [[(plus pa [0 (* (index-of inventar-p active-slots) 40)])
                            [40 40]]])
          (if @build-mode
            (fill-rects! gl [[(plus pa [-40 (* (index-of @build-p bs) 40)]) [40 40]]]))
          (.glEnable gl GL2/GL_TEXTURE_2D)
          (.glEnableClientState gl GL2/GL_TEXTURE_COORD_ARRAY)
          (set-color! gl 255 255 255)
          (doseq [i (range ninventar)
                  :let [tex (txtr (nth active-slots i))]
                  :when tex]
            (if-not (inventar (nth active-slots i))
              (set-color! gl 80 80 80))
            (draw-rects! gl tex [[(plus (plus pa [4 4]) [0 (* i 40)]) [isz isz]]])
            (if-not (inventar (nth active-slots i))
              (set-color! gl 255 255 255)))
          (if @build-mode
            (doseq [i (range (count bs))
                    :let [tex (txtr (nth bs i))]
                    :when tex]
              (if-not (receipes (nth bs i))
                (set-color! gl 80 80 80))
              (draw-rects! gl tex [[(plus (plus pa [(- 4 40) 4]) [0 (* i 40)]) [isz isz]]])
              (if-not (receipes (nth bs i))
                (set-color! gl 255 255 255))))
          (.glDisable gl GL2/GL_TEXTURE_2D)
          (.glDisableClientState gl GL2/GL_TEXTURE_COORD_ARRAY)
          (.glDisableClientState gl GL2/GL_VERTEX_ARRAY)
          (.beginRendering rnd (first size) (second size))
          (.setColor rnd 1 1 1 1)
          (doseq [i (range ninventar)
                  :let [n (inventar (nth active-slots i) 0)]
                  :when (> n 1)]
            (draw-string! rnd (str n) (plus (plus pa [2 35]) [0 (* i 40)]) size))
          (.setColor rnd 0 0 0 1)
          )
        (.endRendering rnd)))))
  (.beginRendering rnd (first size) (second size))
  (.setColor rnd 1 1 1 1)
  (draw-string! rnd (if (< 2 (count @fr-ts))
                      (str (if-not (= (first @fr-ts) (last @fr-ts))
                             (int (/ 1000 (/ (- (first @fr-ts) (last @fr-ts))
                                             (count @fr-ts))))
                             "oo")
                           " "
                           @dbg-info
                           #_(let [foo (map (fn [[a b]]
                                              (Math/abs ^Integer (- a b)))
                                            (partition 2 1 @fr-ts))]
                               (str " " (apply min foo)
                                    " " (int (/ (apply + foo) (count foo)))
                                    " " (apply max foo))))
                      "---")
                [10 10]
                size)
  (.endRendering rnd)
  (.glFlush gl))

(defn scroll [direction]
  (if @build-mode
    (dosync
     (alter build-p scroll-items (possible-recipes) direction))
    (add-plcmd [:scrollip direction])))

(defn mouse-wheeled [e]
  (let [x (.getWheelRotation e)]
    (if (neg? x)
      (dotimes [_ (* -1 x)]
        (scroll :dec))
      (dotimes [_ x]
        (scroll :inc)))))

(defn key-pressed [e]
  (condp = (.getKeyCode e)
    KeyEvent/VK_W (scroll :dec)
    KeyEvent/VK_S (scroll :inc)
    ;KeyEvent/VK_A (scroll :left)
    ;KeyEvent/VK_D (scroll :right)
    KeyEvent/VK_TAB (dosync
                     (alter build-mode not))
    KeyEvent/VK_SPACE (if @build-mode
                        (add-plcmd [:build @build-p]))
    KeyEvent/VK_0 (save!)
    KeyEvent/VK_Q (if (= (.getModifiers e) KeyEvent/CTRL_MASK)
                    (System/exit 0))
    nil))

(def size (ref [0 0]))

(defn get-event-p [e]
  (-> [(.getX e) (.getY e)]
      (plus (minus (->gui (get-in (current-state) [:players @hello :p]))
                   (div @size 2)))
      <-gui))

(defn mouse-pressed [e]
  (condp = (.getButton e)
    MouseEvent/BUTTON1
    (add-plcmd [:shot (get-event-p e)])
    MouseEvent/BUTTON3
    (add-plcmd [:walk (dbg get-event-p e)])
    nil))

(defn load-textures! [class-loader]
  (def txtr (into {} (map (fn [name]
                            [(keyword name)
                             (TextureIO/newTexture (resource (str name ".png") class-loader)
                                                   false "png")])
                          ['grass 'tall-grass 'dirt 'shrub 'door 'wall 'windowed-wall 'tree
                           'twig 'gun 'pickaxe 'tree-crown 'water 'sand 'steak 'steak-fried
                           'thread 'fur 'axe 'chest 'shrub-pear 'granite 'granite-floor
                           'rock 'stone 'spear 'campfire-on 'campfire-off 'campfire-empty
                           'bunny 'deadbunny 'zombie 'player 'trunk 'hands 'pear 'lumberjack
                           'idol 'gold 'wood 'tower 'carpenter]))))

(defn create-gui []
  (let [can (doto (GLCanvas.)
              (.setPreferredSize (Dimension. 600 600)))
        fr (doto (Frame.)
             (.add can))
        class-loader (.getContextClassLoader (Thread/currentThread))]
    (.setFocusTraversalKeysEnabled can false)
    (.addGLEventListener can (proxy [GLEventListener] []
                               (init [d]
                                 (load-textures! class-loader)
                                 (def rnd (TextRenderer. (Font. "SansSerif" Font/PLAIN 12)))
                                 (let [gl (.getGL2 (.getGL d))]
                                   (.glDisable gl GL/GL_DEPTH_TEST)
                                   (.glEnable gl GL/GL_BLEND)
                                   (.glBlendFunc gl GL/GL_SRC_ALPHA GL/GL_ONE_MINUS_SRC_ALPHA)))
                               (display [^GLAutoDrawable d]
                                 (let [gl (.getGL2 (.getGL d))]
                                   (render gl @size)))
                               (reshape [d x y w h]
                                 (dosync
                                  (ref-set size [w h])))))
    (listen fr :window-closing (fn [_] (System/exit 0)))
    (listen can
            :key-pressed key-pressed
            :mouse-wheel-moved mouse-wheeled
            :mouse-pressed mouse-pressed)
    (pack! fr)
    (show! fr)
    (request-focus! can)
    can))
