(ns jardingaard.client
  (:gen-class)
  (:use [seesaw core]
        [clojure.java io]
        [jardingaard util shared reducers rules])
  (:import [java.net Socket]
           [java.util Date]
           [java.io OutputStreamWriter InputStreamReader BufferedWriter BufferedReader]
           [java.nio FloatBuffer]
           [java.awt Color Dimension Graphics2D RenderingHints Transparency Image Frame Font]
           [java.awt.image BufferedImage]
           [java.awt.event KeyEvent MouseEvent]
           [javax.media.opengl.awt GLCanvas]
           [javax.media.opengl GLEventListener GL GLCapabilities GLProfile
            GL2 GLAutoDrawable GLDrawableFactory]
           [com.jogamp.opengl.util.awt TextRenderer]
           [com.jogamp.opengl.util.texture Texture TextureIO]
           [com.jogamp.common.nio Buffers]
           [clojure.lang LineNumberingPushbackReader]))

(set! *warn-on-reflection* true)

(def tsz 32)

(defn add-rect! [^FloatBuffer buf [p0 p1] [s0 s1]]
  (doseq [n [p0 p1
             p0 (+ p1 s1)
             (+ p0 s0) (+ p1 s1)
             (+ p0 s0) p1]]
    (.put buf (float n))))

(defn add-tex-coords! [^FloatBuffer buf]
  (doseq [^Float f [0.0 1.0
                    0.0 0.0
                    1.0 0.0
                    1.0 1.0]]
    (.put buf f)))
  
(defn draw-string! [^TextRenderer rnd ^String s [p0 p1] [s0 s1]]
  (.draw rnd s (int p0) (int (- s1 p1))))

(defn set-color! [^GL2 gl red green blue]
  (.glColor3f gl (/ red 255.0) (/ green 255.0) (/ blue 255.0)))

(def fr-ts (ref '()))

(def bgimgs (ref {}))

(def ^Socket conn)

(def messenger (loud-agent nil))

(def state (ref nil))

(def srv-messages (ref nil))

(def cl-messages (ref {}))

(def build-index (ref nil))

(def hello (ref nil))

(declare txtr ^TextRenderer rnd)

(def skew (ref nil))

(def fr-counter (loud-agent nil))

(defn count-inc [c]
  (Thread/sleep (let [t (+ tick-duration
                           (condp = @skew
                             :plus
                             -20
                             :minus
                             20
                             0))]
                  (dosync (ref-set skew nil))
                  t))
  (send-off fr-counter count-inc)
  (inc c))

(defn step-to [[start st] end msgs]
  (reduce (fn [sta n]
            (binding [*seed* n]
              (step sta (msgs n))))
          st
          (range start end)))

(defn current-state []
  (step-to @state @fr-counter (merge-with concat @srv-messages @cl-messages)))

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

#_(defn tile-groups [world pfoo [s0 s1]]
  (group-by #(get-in-map world %)
            (for [p0 (range (max 0 (- (first pfoo) s0))
                            (min (count world) (+ (first pfoo) s0)))
                  p1 (range (max 0 (- (second pfoo) s1))
                            (min (count world) (+ (second pfoo) s1)))]
              [p0 p1])))

(defn tile-groups [world pfoo [s0 s1]]
  (group-by #(get-in-map world %)
            (product (rrange (max 0 (- (first pfoo) s0))
                             (+ (first pfoo) s0))
                     (rrange (max 0 (- (second pfoo) s1))
                             (+ (second pfoo) s1)))))

(def ^FloatBuffer vert-buf (Buffers/newDirectFloatBuffer 0))
(def ^FloatBuffer texc-buf (Buffers/newDirectFloatBuffer 0))

(defn draw-rects! [^GL2 gl tex rects]
  (let [nrects (count rects)]
    (if-not (= 0 nrects)
      (let [nverts (* 4 nrects)]
        (when (< (.capacity vert-buf) (* 2 nverts))
          (def ^FloatBuffer vert-buf (Buffers/newDirectFloatBuffer (* 2 nverts)))
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

(defn draw-tiles! [gl tex tiles]
  (draw-rects! gl tex (for [p tiles]
                        [(mult p tsz) [tsz tsz]])))

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

(defn possible-recipes []
  (keep (fn [[x r]]
          (if (every? (fn [[y n]]
                        (player-has? (get-in @state [1 :players @hello]) y n))
                      r)
            x))
        recipes))

(defn render [^GL2 gl size]
  (dosync
   (ref-set fr-ts (conj (take 50 @fr-ts) (.getTime (Date.)))))
  (.glClearColor gl 0.5 0.5 0.5 1)
  (.glClear gl GL/GL_COLOR_BUFFER_BIT)
  (if (and (get-in @state [1 :players @hello])
           (< (- @fr-counter (first @state)) 40))
    (let [{:keys [players bullets world c-sites world bunnies deadbunnies zombies]} (current-state)
          pfoo (round (get-in players [@hello :p]))
          offset (minus (mult size 0.5) (mult (get-in players [@hello :p]) tsz))]
      (prepare-gl! gl offset size)
      (set-color! gl 255 255 255)
      (.glEnable gl GL2/GL_TEXTURE_2D)
      (.glEnableClientState gl GL2/GL_VERTEX_ARRAY)
      (.glEnableClientState gl GL2/GL_TEXTURE_COORD_ARRAY)
      (let [foo (tile-groups world pfoo (plus [1 1] (round (div size (* tsz 2.0)))))]
        (doseq [[bg tiles] foo
                :let [tex (txtr bg)]
                :when tex]
          (draw-tiles! gl tex tiles))
        (draw-tiles! gl (txtr :bunny) (map :p bunnies))
        (draw-tiles! gl (txtr :deadbunny) (map :p deadbunnies))
        (draw-tiles! gl (txtr :zombie) (map :p zombies))
        (draw-tiles! gl (txtr :player) (map #(:p (second %)) players))
        (.glColor4f gl 1.0 1.0 1.0 0.2)
        (draw-rects! gl (txtr :tree-crown)
                     (for [p (:tree foo)]
                       [(mult (minus p [2 2]) tsz) (mult [5 5] tsz)]))
        (.glDisable gl GL2/GL_TEXTURE_2D)
        (.glDisableClientState gl GL2/GL_TEXTURE_COORD_ARRAY)
        (set-color! gl 0 0 0)
        (fill-rects! gl (for [{:keys [p t]} c-sites]
                          [(mult p tsz) [(inc (* t 0.3)) 5]]))
        (set-color! gl 150 20 50)
        (fill-rects! gl (for [[pid {:keys [p hp]}] players]
                          [(minus (mult p tsz) [0 10]) [hp 5]]))
        (set-color! gl 0 0 0)
        (fill-rects! gl (for [{p :p} bullets]
                          [(plus [12 12] (mult p tsz)) [5 5]])))
      (let [{:keys [inventar inventar-p]} (players @hello)
            ninventar (count inventar)
            pb [(- (first size) 40) (- (/ (second size) 2) (* ninventar 20))]
            pa (minus pb offset)
            bs (possible-recipes)]
        (set-color! gl 20 40 10)
        (fill-rects! gl (cons [pa [40 (* ninventar 40)]]
                              (if @build-index
                                [[(minus pa [40 0]) [40 (* (count bs) 40)]]])))
        (set-color! gl 40 80 20)
        (fill-rects! gl [[(plus pa [0 (* inventar-p 40)]) [40 40]]])
        (if @build-index
          (fill-rects! gl [[(plus pa [-40 (* @build-index 40)]) [40 40]]]))
        (.glEnable gl GL2/GL_TEXTURE_2D)
        (.glEnableClientState gl GL2/GL_TEXTURE_COORD_ARRAY)
        (set-color! gl 255 255 255)
        (doseq [i (range ninventar)
                :let [tex (txtr (first (nth inventar i)))]
                :when tex]
          (draw-rects! gl tex [[(plus (plus pa [4 4]) [0 (* i 40)]) [tsz tsz]]]))
        (if @build-index
          (doseq [i (range (count bs))
                  :let [cs (nth bs i)]
                  j (range (count cs))
                  :let [tex (txtr (nth cs j))]
                  :when tex]
            (draw-rects! gl tex [[(plus (plus pa [(- 4 40) 4]) [(* j -40) (* i 40)]) [tsz tsz]]])))
        (.glDisable gl GL2/GL_TEXTURE_2D)
        (.glDisableClientState gl GL2/GL_TEXTURE_COORD_ARRAY)
        (.glDisableClientState gl GL2/GL_VERTEX_ARRAY)
        (.beginRendering rnd (first size) (second size))
        (.setColor rnd 1 1 1 1)
        (doseq [i (range ninventar)
                :let [n (second (nth inventar i))]
                :when (< 1 n)]
          (draw-string! rnd (str n) (plus (plus pb [2 35]) [0 (* i 40)]) size))
        (.setColor rnd 0 0 0 1)
        (doseq [[pid {:keys [p inventar inventar-p died name]}] players]
          (draw-string! rnd (str name " - " died " - " (round p))
                        (plus offset (minus (mult p tsz) [0 20]))
                        size)))
      (.endRendering rnd)))
  (.beginRendering rnd (first size) (second size))
  (.setColor rnd 1 1 1 1)
  (draw-string! rnd (if (< 2 (count @fr-ts))
                      (str (if-not (= (first @fr-ts) (last @fr-ts))
                             (int (/ 1000 (/ (- (first @fr-ts) (last @fr-ts))
                                             (count @fr-ts))))
                             "oo")
                           #_(let [foo (map (fn [[a b]]
                                              (Math/abs ^Integer (- a b)))
                                            (partition 2 1 @fr-ts))]
                               (str " " (apply min foo)
                                    " " (int (/ (apply + foo) (count foo)))
                                    " " (apply max foo)))
                           " "
                           @fr-counter
                           " "
                           (if @state
                             (- @fr-counter (first @state))
                             "..."))
                      "---")
                [10 10]
                size)
  (.endRendering rnd)
  (.glFlush gl))

(defn connection-lost! []
  (show! (pack! (dialog :content "connection lost" :type :warning)))
  (System/exit 1))

(defn msg [m]
  (send-off messenger (fn [a]
                        (try 
                          (binding [*out* (BufferedWriter. (OutputStreamWriter. (.getOutputStream conn)))]
                            (prn m))
                          (catch java.net.SocketException e)))))

(add-watch fr-counter :send-msgs (fn [_ _ n nn]
                                   (msg [:cmds (or n nn) (@cl-messages n)])))

(defn add-msg [m]
  (dosync
   (alter cl-messages update-in [@fr-counter] conj [:plcmd @hello m])))

(defn key-pressed [e]
  (condp = (.getKeyCode e)
    KeyEvent/VK_A (if @build-index
                    (dosync (alter build-index #(let [n (count (possible-recipes))]
                                                  (if (= n 0)
                                                    nil
                                                    (mod (inc %) n)))))
                    (add-msg [:incip]))
    KeyEvent/VK_Q (if @build-index
                    (dosync (alter build-index #(let [n (count (possible-recipes))]
                                                  (if (= n 0)
                                                    nil
                                                    (mod (dec %) (count (possible-recipes)))))))
                    (add-msg [:decip]))
    KeyEvent/VK_TAB (dosync (alter build-index #(if % nil 0)))
    KeyEvent/VK_SPACE (add-msg [:build (nth (possible-recipes) @build-index)])
    KeyEvent/VK_0 (msg [:save])
    KeyEvent/VK_ESCAPE (System/exit 0)
    KeyEvent/VK_S (do (pr-summary @state)
                      (pr-summary (sort @srv-messages))
                      (pr-summary @fr-counter))
    nil))

(def size (ref [0 0]))

(defn get-event-p [e]
  (-> [(.getX e) (.getY e)]
      (minus [(/ tsz 2) (/ tsz 2)])
      (div tsz)
      (plus (minus (get-in (current-state) [:players @hello :p]) (div @size (* tsz 2.0))))))

(defn mouse-pressed [e]
  (condp = (.getButton e)
    MouseEvent/BUTTON1
    (add-msg [:shot (get-event-p e)])
    MouseEvent/BUTTON3
    (add-msg [:walk (get-event-p e)])
    nil))

(defn load-textures! [class-loader]
  (def txtr (into {} (map (fn [name]
                            [(keyword name)
                             (TextureIO/newTexture (resource (str name ".png") class-loader)
                                                   false "png")])
                          ['grass 'tall-grass 'dirt 'shrub 'door 'wall 'windowed-wall 'tree
                           'twig 'gun 'pickaxe 'tree-crown 'water 'sand 'steak 'thread 'fur
                           'bunny 'deadbunny 'zombie 'player 'trunk 'hands]))))

(defn handle-msg [d]
  (dosync
   (cond (vector? d)
         (condp = (first d)
           :patch-state (alter state apply-patch (second d))
           :patch-messages (do (alter srv-messages apply-patch (second d))
                               (alter cl-messages dissoc (nth d 2)))
           :hello (ref-set hello (second d))
           :skew (ref-set skew (second d))
           :frame-count (do (when-not @fr-counter
                              (send fr-counter (constantly (second d)))
                              (send-off fr-counter count-inc))
                            (msg (conj d (or @fr-counter (second d)))))))))

(defn handle-msgs []
  (let [r (LineNumberingPushbackReader. (BufferedReader. (InputStreamReader. (.getInputStream conn))))]
    (loop []
      (try
        (handle-msg (read r))
        (catch clojure.lang.LispReader$ReaderException e
          (connection-lost!)))
      (recur))))
  
(defn -main [& [^String host]]
  (def conn (loop []
              (if-let [c (try
                           (doto (Socket. (or host "soupwhale.com") 8282)
                             (.setTcpNoDelay true))
                           (catch java.net.ConnectException e
                             (Thread/sleep 1000)
                             nil))]
                c
                (recur))))
  (forkIO #(msg [:name (show! (pack! (dialog :content
                                             (flow-panel :items ["Give yourself a name"
                                                                 (text :id :name :columns 10)])
                                             :type :question
                                             :option-type :ok-cancel
                                             :success-fn (fn [p]
                                                           (text (select (to-root p) [:#name]))))))]))
  (let [can (doto (GLCanvas.)
              (.setPreferredSize (Dimension. 600 600)))
        fr (doto (Frame.)
             (.add can))
        drawer (loud-agent nil)
        class-loader (.getContextClassLoader (Thread/currentThread))]
    (.setFocusTraversalKeysEnabled can false)
    (.addGLEventListener can (proxy [GLEventListener] []
                               (init [d]
                                 (load-textures! class-loader)
                                 (def rnd (TextRenderer. (Font. "SansSerif" Font/PLAIN 12)))
                                 (let [gl (.getGL2 (.getGL d))]
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
            :mouse-pressed mouse-pressed)
    (pack! fr)
    (show! fr)
    (request-focus! can)
    (add-watch fr-counter :paint (fn [_ _ _ _]
                                   (send-off drawer (fn [_]
                                                      (.display can)))))
    (handle-msgs)))
