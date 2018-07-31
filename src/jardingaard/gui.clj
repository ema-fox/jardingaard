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
(def halftsz (/ tsz 2))
(def quarttsz (/ tsz 4))
(def hp-offset (+ halftsz 10))

(def isz 32)

(def fr-ts (ref '()))

(def hello (ref nil))

(def build-index (ref nil))

(def build-list (ref nil))

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

(defn set-color! [^GL2 gl red green blue]
  (.glColor3f gl (/ red 255.0) (/ green 255.0) (/ blue 255.0)))


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

(defn draw-double-tiles! [gl tex tiles]
  (draw-rects! gl tex (for [p tiles]
                        [(minus (->gui p) [tsz (+ halftsz tsz)]) [dtsz dtsz]])))

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
  (let [{:keys [players bullets world c-sites bunnies deadbunnies zombies chests tick lumberjacks arrows]} (current-state)]
    (binding [*tick* tick]
    (when (get players @hello)
      (let [pfoo (round2 (get-in players [@hello :p]))
            offset (minus (mult size 0.5) (->gui (get-in players [@hello :p])))
            {:keys [p path]} (players @hello)]
        (prepare-gl! gl offset size)
        (set-color! gl 255 255 255)
        (.glEnable gl GL2/GL_TEXTURE_2D)
        (.glEnableClientState gl GL2/GL_VERTEX_ARRAY)
        (.glEnableClientState gl GL2/GL_TEXTURE_COORD_ARRAY)
        (let [mp (get-map-part world pfoo (plus [1 1] (round (div size halftsz))))]
          (doseq [{:keys [p ground type]} (sort-by :p mp)]
            (if-let [tex (txtr ground)]
              (draw-double-tiles! gl tex [p]))
            (if-let [tex (txtr type)]
              (draw-double-tiles! gl tex [p]))
            (draw-beings! gl (txtr :player) (filter #(= p (round2 %)) (map :p (vals players))))
            (draw-beings! gl (txtr :player) (filter #(= p (round2 %)) (map :p (vals lumberjacks))))
            (draw-beings! gl (txtr :zombie) (filter #(= p (round2 %)) (map :p (vals zombies)))))
          #_(draw-beings! gl (txtr :bunny) (map :p bunnies))
          #_(draw-beings! gl (txtr :deadbunny) (map :p deadbunnies))
          (.glDisable gl GL2/GL_TEXTURE_2D)
          (.glDisableClientState gl GL2/GL_TEXTURE_COORD_ARRAY)
          (set-color! gl 90 40 25)
          (doseq [a (vals arrows)]
            (draw-lines! gl [(minus (->gui (:p a)) [0 quarttsz])
                             (minus (->gui (plus (:p a) (mult (direction (:p a) (:p (zombies (:target a)))) 0.1))) [0 quarttsz])]))
          (set-color! gl 250 220 25)
          (fill-rects! gl (for [tile mp
                                :when (:type tile)]
                            [(minus (->gui (:p tile)) [halftsz halftsz]) [(* (spawn-progress tile) tsz) 5]]))
          (set-color! gl 250 20 25)
          (fill-rects! gl (for [tile mp
                                :when (:broken tile)]
                            [(minus (->gui (:p tile)) [halftsz hp-offset]) [(* (broken-progress tile) tsz) 5]]))

          (fill-rects! gl (for [tile mp
                                :when (and (not (:broken tile))
                                           (:hp tile))]
                            [(minus (->gui (:p tile)) [halftsz hp-offset]) [tsz 5]]))
          (fill-rects! gl (for [zb (vals zombies)]
                            [(minus (->gui (:p zb)) [quarttsz hp-offset]) [halftsz 5]]))
          (set-color! gl 40 120 20)
          (fill-rects! gl (for [tile mp
                                :when (and (not (:broken tile))
                                           (:hp tile))]
                            [(minus (->gui (:p tile)) [halftsz hp-offset]) [(* (health-fraction tile) tsz) 5]]))

          (fill-rects! gl (for [zb (vals zombies)]
                            [(minus (->gui (:p zb)) [quarttsz hp-offset]) [(* (health-fraction zb) halftsz) 5]]))
          (set-color! gl 220 180 20)
          (draw-lines! gl (for [pp (cons p path)]
                            (->gui pp)))
          #_(fill-rects! gl (for [{:keys [p t]} c-sites]
                            [(mult p tsz) [(inc (* t 0.3)) 5]]))
          (set-color! gl 0 0 0)
          #_(fill-rects! gl (for [{p :p} bullets]
                            [(plus [12 12] (mult p tsz)) [5 5]]))
          (.glTranslatef gl (- 0 (first offset)) (- 0 (second offset)) 0)
          (.glDisableClientState gl GL2/GL_VERTEX_ARRAY)
          (.beginRendering rnd (first size) (second size))
          (.setColor rnd 1 1 0.5 1)
          (doseq [{:keys [type merit p]} mp
                  :when (= type :idol)]
            (draw-string! rnd (str merit) (minus (plus (->gui p) offset) [20 tsz]) size))
          (doseq [[pid {:keys [p merit name]}] players]
            (draw-string! rnd (str name " - " merit)
                          (minus (plus (->gui p) offset) [20 halftsz]) size))
          (.endRendering rnd)
          (.glEnableClientState gl GL2/GL_VERTEX_ARRAY))
        (let [{:keys [inventar inventar-p inventar-category-p open-chest hp energy]}
              (players @hello)
              ninventar (count inventar)
              pa [(- (first size) 40) (- (/ (second size) 2) (* ninventar 20))]
              bs @build-list
              mid1 (- (/ (second size) 2) 100)]
          (set-color! gl 50 5 15)
          (fill-rects! gl [[[0 mid1] [20 200]]])
          (set-color! gl 150 15 50)
          (fill-rects! gl [[[0 mid1] [20 hp]]])
          (set-color! gl 50 40 5)
          (fill-rects! gl [[[20 mid1] [20 200]]])
          (set-color! gl 150 120 15)
          (fill-rects! gl [[[20 mid1] [20 energy]]])
          (set-color! gl 20 40 10)
          (fill-rects! gl (cons [pa [40 (* ninventar 40)]]
                                (if @build-index
                                  [[(minus pa [40 0]) [40 (* (count bs) 40)]]])))
          (set-color! gl 40 80 20)
          (fill-rects! gl [[(plus pa [(if (= inventar-category-p :inventar)
                                        0
                                        -40)
                                      (* inventar-p 40)])
                            [40 40]]])
          (if @build-index
            (fill-rects! gl [[(plus pa [-40 (* @build-index 40)]) [40 40]]]))
          (.glEnable gl GL2/GL_TEXTURE_2D)
          (.glEnableClientState gl GL2/GL_TEXTURE_COORD_ARRAY)
          (set-color! gl 255 255 255)
          (doseq [i (range ninventar)
                  :let [tex (txtr (first (nth inventar i)))]
                  :when tex]
            (draw-rects! gl tex [[(plus (plus pa [4 4]) [0 (* i 40)]) [isz isz]]]))
          (if open-chest
            (let [items (map first (get chests open-chest))]
              (doseq [i (range (count items))
                      :let [tex (txtr (nth items i))]
                      :when tex]
                (draw-rects! gl tex [[(plus (plus pa [(- 4 40) 4]) [0 (* i 40)]) [isz isz]]]))))
          (if @build-index
            (doseq [i (range (count bs))
                    :let [cs (nth bs i)]
                    j (range (count cs))
                    :let [tex (txtr (nth cs j))]
                    :when (and tex (some #(= cs %) (possible-recipes)))]
              (draw-rects! gl tex [[(plus (plus pa [(- 4 40) 4]) [(* j -40) (* i 40)]) [isz isz]]])))
          (.glDisable gl GL2/GL_TEXTURE_2D)
          (.glDisableClientState gl GL2/GL_TEXTURE_COORD_ARRAY)
          (.glDisableClientState gl GL2/GL_VERTEX_ARRAY)
          (.beginRendering rnd (first size) (second size))
          (.setColor rnd 1 1 1 1)
          (doseq [i (range ninventar)
                  :let [n (second (nth inventar i))]
                  :when (< 1 n)]
            (draw-string! rnd (str n) (plus (plus pa [2 35]) [0 (* i 40)]) size))
          (if open-chest
            (let [ns (map second (get chests open-chest))]
              (doseq [i (range (count ns))
                      :let [n (nth ns i)]
                      :when (< 1 n)]
                (draw-string! rnd (str n) (plus (plus pa [2 35]) [-40 (* i 40)]) size))))
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

(defn scroll [x]
  (if @build-index
    (dosync (alter build-index #(let [n (count @build-list)]
                                  (if (= n 0)
                                    nil
                                    (mod (({:inc inc :dec dec :left identity :right identity} x) %) n)))))
    (add-plcmd [:scrollip x])))

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
    KeyEvent/VK_A (scroll :left)
    KeyEvent/VK_D (scroll :right)
    KeyEvent/VK_TAB (if (get-in (current-state) [:players @hello :open-chest])
                      (add-plcmd [:close-chest])
                      (dosync
                       (ref-set build-list (possible-recipes))
                       (alter build-index #(if % nil 0))))
    KeyEvent/VK_SPACE (if @build-index
                        (if-let [build (nth @build-list @build-index)]
                          (add-plcmd [:build build]))
                        (add-plcmd [:move-item]))
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
                           'idol 'gold 'wood]))))

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
