;;; AgentCubes Splash Screen
;;; Alexander Repenning



(in-package :xlui)

;; OpenGL Helper functions

(defun GET-OPENGL-INFO (Type)
  (with-glcontext (shared-opengl-view)
    (let ((&bytes (glGetString Type)))
      (with-output-to-string (Info)
        (dotimes (i 10000)
          (let ((Byte (ccl::%get-byte &bytes i)))
            (when (zerop Byte) (return))
            (princ (code-char Byte) Info)))))))


;; Globals 

(defvar *Splash-Screen-Animation-Process* nil)


(defvar *Animated-Splash-View* nil "There can only be one")


;; window

(defclass AGENTCUBES-SPLASH-WINDOW (application-window)
  ((timeout :accessor timeout :initform 0.0 :type float :documentation "Time in seconds after which window will disapper"))
  (:default-initargs
      :use-custom-window-controller t))


(defmethod INITIALIZE-INSTANCE :after ((Self agentcubes-splash-window) &rest Args)
  (declare (ignore Args) (special cl-user::*AgentCubes-Version-String*))
  (center Self)
  (let ((View (view-named Self "cubes")))
    (when (and View (view-named Self "agentcubes version"))
      (lui::in-main-thread ()  ;;; necessary on OS X 10.7.0, why??
        (cond (*AgentCubes-lite-mode*
               (setf (width (view-named Self "agentcubes name")) 170)
               (setf (text (view-named Self "agentcubes name")) "AgentCubes Lite"))
              (t
               (setf (width (view-named Self "agentcubes name")) 130)
               (setf (text (view-named Self "agentcubes name")) "AgentCubes")))
        (setf (text (view-named Self "agentcubes version")) cl-user::*AgentCubes-Version-String*)
        (setf (text (view-named Self "vendor")) (get-opengl-info GL_VENDOR))
        (setf (text (view-named Self "renderer")) (get-opengl-info GL_RENDERER))
        (setf (text (view-named Self "version")) (get-opengl-info GL_VERSION)))
      (start-animation View))))


(defmethod ADJUST-SEGMENTS-ACTION ((Window agentcubes-splash-window) (Slider slider))
  (setf (segments (first (agents (view-named Window "cubes")))) (truncate (value Slider)))
  (display (view-named Window "cubes")))


(defmethod ADJUST-GAP-ACTION ((Window agentcubes-splash-window) (Slider slider))
  (setf (gap (first (agents (view-named Window "cubes")))) (value Slider))
  (display (view-named Window "cubes")))


(defmethod WINDOW-SHOULD-CLOSE ((Self agentcubes-splash-window))
  (cond
   ;; if there is an animated view start closing process but pretend not to be able to close
   (*Animated-Splash-View* 
    (stop-animation 
     *Animated-Splash-View* 
     :stop-function
     #'(lambda (View)
         (close-with-no-questions-asked (window View))))
    nil)  ;; should NOT close
   ;; no annimated view
   (t 
    t))) ;; should CLOSE


(defmethod WINDOW-WILL-CLOSE ((Self agentcubes-splash-window) Notification)
  (declare (ignore Notification))
  ;; do nothing
  )


(defmethod CLOSE-WITH-NO-QUESTIONS-ASKED ((Self agentcubes-splash-window))
  (setq *Animated-Splash-View* nil)
  (call-next-method))


;; view


(defclass AGENTCUBE-VIEW (agent-3d-view)
  ((animation-stop-function :accessor animation-stop-function :initform nil :documentation "If there is a stop-function then the animation of this view will be finished, and the stop-function called with the view (in the animation thread)" )))


(defmethod CLEAR-BACKGROUND ((Self agentcube-view))
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClear #.(logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT)))


(defmethod START-ANIMATION ((Self agentcube-view))
  (setq *Animated-Splash-View* Self)
  (setq *Splash-Screen-Animation-Process* 
        (ccl::process-run-function
         '(:name "OpenGL Splash Screen Animations" :priority 0)
         #'(lambda ()
             (let ((Sample-Time (+ (get-internal-real-time) (* 1.0 internal-time-units-per-second)))
                   (Stop-Time (+ (get-internal-real-time) (* (timeout (window Self)) internal-time-units-per-second)))
                   (Frames 0))
               (ccl::with-autorelease-pool
                   (loop
                     ;; run stop function and stop thread if there is a stop function
                     (when (animation-stop-function Self)
                       (funcall (animation-stop-function Self) Self)
                       (setq *Splash-Screen-Animation-Process* nil)
                       (return))
                     (incf Frames)
                     ;; animate and display without frame rate caps
                     (animate Self (delta-time Self))
                     (display Self)
                     ;; time out?
                     (when (and (not (zerop (timeout (window Self))))
                                (>= (get-internal-real-time) Stop-Time))
                       (setf *Animated-Splash-View* nil)
                       (window-close (window Self))
                       (return))
                (sleep 0.014))))))))


                     ;; reset timer if needed
                     (when (>= (get-internal-real-time) Sample-Time)
                       ;; update frame rate once per second
                       (let ((Label (view-named (window Self) "frame rate")))
                         (when Label 
                           (lui::in-main-thread ()  ;;; needed in OS X 10.7.0, early Lion: WHY?
                             (setf (text Label) (format nil "~A" Frames)))))
                       (setf Frames 0)
                       (setf Sample-Time (+ (get-internal-real-time) (* 1.0 internal-time-units-per-second)))))))))))


(defmethod STOP-ANIMATION ((Self agentcube-view) &key (Stop-Function #'identity))
  (cond
   ;; thread is running: delay stop-function until thread stops
   (*Splash-Screen-Animation-Process* 
    (setf (animation-stop-function *Animated-Splash-View*) Stop-Function))
   ;; no thread: run stop-function immediately
   (t
    (funcall Stop-Function *Animated-Splash-View*))))


(defmethod PREPARE-OPENGL ((Self agentcube-view))
  ;; GL setup
  (glClearColor 0.0 0.0 0.0 0.0)
  (glShadeModel GL_SMOOTH)
  (glPixelStorei GL_UNPACK_ALIGNMENT 1)
  ;; define material
  (glMaterialfv GL_FRONT_AND_BACK GL_SPECULAR { 1.0 0.5 0.5 0.0 })
  (glMaterialf GL_FRONT_AND_BACK GL_SHININESS 50.0)
  (glMaterialfv GL_FRONT_AND_BACK GL_AMBIENT_AND_DIFFUSE { 1.0 1.0 1.0 1.0 })
  ;; light
  (glLightfv GL_LIGHT0 GL_POSITION { 50.0 50.0 50.0 1.0 })
  (glLightfv GL_LIGHT0 GL_DIFFUSE { 1.0 1.0 1.0 1.0 })
  (glLightfv GL_LIGHT0 GL_SPECULAR { 1.0 1.0 1.0 1.0 })
  ;; use white material
  (glMaterialfv GL_FRONT_AND_BACK GL_AMBIENT_AND_DIFFUSE { 1.0 1.0 1.0 1.0 })
  ;; enablers
  (glEnable GL_LIGHTING)
  (glEnable GL_LIGHT0)
  (glEnable GL_DEPTH_TEST)
  (glEnable GL_POINT_SMOOTH)
  ;; Textures
  (glEnable GL_TEXTURE_2D)
  (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
  ;; Alpha
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  ;; camera
  ;;(aim-camera (camera Self) :eye-z 40.0 :near 1.0)
  )



;; agents

(defclass AGENT-HYPER-CUBE (agent-3d)
  ((segments :accessor segments :initform 3 :initarg :segments :type integer)
   (total-time :accessor total-time :initform 0.0)
   (gap :accessor gap :initform 0.2 :initarg :gap)))


(defmethod ANIMATE ((Self agent-hyper-cube) Time)
  (incf (roll Self) (* 90.0 Time))
  (incf (pitch Self) (* 40.0 Time))
  (incf (heading Self) (* 13 Time))
  (incf (total-time Self) Time)
  (setf (gap Self) (* 0.2 (+ 1.0 (sin (* 0.5 (total-time Self)))))))
  
        
(defmethod DRAW ((Self agent-hyper-cube))
  ;; define material
  (glMaterialfv GL_FRONT_AND_BACK GL_SPECULAR { 1.0 1.0 1.0 1.0 })
  (glMaterialf GL_FRONT_AND_BACK GL_SHININESS 128.0)
  (glMaterialfv GL_FRONT_AND_BACK GL_AMBIENT_AND_DIFFUSE { 1.0 1.0 1.0 1.0 })
  ;; light 0
  (glLightfv GL_LIGHT0 GL_POSITION { 2.0 1.3 1.0 1.0 })
  (glLightfv GL_LIGHT0 GL_DIFFUSE { 1.0 1.0 1.0 1.0 })
  ;; (glLightfv GL_LIGHT0 GL_AMBIENT { 1.0 1.0 1.0 1.0 })
  (glLightfv GL_LIGHT0 GL_SPECULAR { 3.0 3.0 3.0 1.0 })
  ;; light 1
  (glEnable GL_LIGHT1)
  (glLightfv GL_LIGHT1 GL_POSITION { -5.0 -5.0 -5.0 1.0 })
  (glLightfv GL_LIGHT1 GL_DIFFUSE { 5.0 5.0 5.0 1.0 })
  (glPushMatrix)
  (let ((Center (* -1 (/ (+ 1 (* (gap Self) (segments Self))) 2.0))))
    (glTranslatef Center Center Center))
  ;; highly repeditive code: factor out into 2 functions
  (let ((d (/ 1.0 (segments Self)))
        (*Default-OpenGL-Texture-Magnification-Filter* GL_NEAREST))
    ;; front
    (use-texture Self "agent-orange.png")
    (glBegin GL_QUADS)
    (glNormal3f 0.0 0.0 1.0)
    (dotimes (r (segments Self))
      (dotimes (c (segments Self))
        (dotimes (l (segments Self))
          (let ((x (* c (+ d (gap Self))))
                (y (* r (+ d (gap Self))))
                (xt (* c d))
                (yt (* r d))
                (z (+ (* l (+ d (gap Self))) d)))
            (glTexCoord2f xt yt)             (glVertex3f x y z)
            (glTexCoord2f xt (+ yt d))       (glVertex3f x (+ y d) z)
            (glTexCoord2f (+ xt d) (+ yt d)) (glVertex3f (+ x d)  (+ y d) z)
            (glTexCoord2f (+ xt d) yt)       (glVertex3f  (+ x d) y z)))))
    (glEnd)
    ;; back
    (use-texture Self "agent-red.png")
    (glBegin GL_QUADS)
    (glNormal3f 0.0 0.0 -1.0)
    (dotimes (r (segments Self))
      (dotimes (c (segments Self))
        (dotimes (l (segments Self))
          (let ((x (* c (+ d (gap Self))))
                (y (* r (+ d (gap Self))))
                (xt (* c d))
                (yt (* r d))
                (z (* l (+ d (gap Self)))))
            (glTexCoord2f xt yt)             (glVertex3f x y z)
            (glTexCoord2f xt (+ yt d))       (glVertex3f x (+ y d) z)
            (glTexCoord2f (+ xt d) (+ yt d)) (glVertex3f (+ x d)  (+ y d) z)
            (glTexCoord2f (+ xt d) yt)       (glVertex3f  (+ x d) y z)))))
    (glEnd)
    ;; Left
    (use-texture Self "agent-blue.png")
    (glBegin GL_QUADS)
    (glNormal3f -1.0 0.0 0.0)
    (dotimes (r (segments Self))
      (dotimes (c (segments Self))
        (dotimes (l (segments Self))
          (let ((x (* c (+ d (gap Self))))
                (y (* r (+ d (gap Self))))
                (xt (* c d))
                (yt (* r d))
                (z (* l (+ d (gap Self)))))
            (glTexCoord2f xt yt)             (glVertex3f z y x)
            (glTexCoord2f xt (+ yt d))       (glVertex3f z (+ y d) x)
            (glTexCoord2f (+ xt d) (+ yt d)) (glVertex3f z  (+ y d)  (+ x d))
            (glTexCoord2f (+ xt d) yt)       (glVertex3f z y (+ x d))))))
    (glEnd)
    ;; right
    (use-texture Self "agent-yellow.png")
    (glBegin GL_QUADS)
    (glNormal3f 1.0 0.0 0.0)
    (dotimes (r (segments Self))
      (dotimes (c (segments Self))
        (dotimes (l (segments Self))
          (let ((x (* c (+ d (gap Self))))
                (y (* r (+ d (gap Self))))
                (xt (* c d))
                (yt (* r d))
                (z (+ (* l (+ d (gap Self))) d)))
            (glTexCoord2f xt yt)             (glVertex3f z y x)
            (glTexCoord2f xt (+ yt d))       (glVertex3f z (+ y d) x)
            (glTexCoord2f (+ xt d) (+ yt d)) (glVertex3f z  (+ y d)  (+ x d))
            (glTexCoord2f (+ xt d) yt)       (glVertex3f z y (+ x d))))))
    (glEnd)
    ;; Top
    (use-texture Self "agent-lightgreen.png")
    (glBegin GL_QUADS)
    (glNormal3f 0.0 1.0 0.0)
    (dotimes (r (segments Self))
      (dotimes (c (segments Self))
        (dotimes (l (segments Self))
          (let ((x (* c (+ d (gap Self))))
                (y (* r (+ d (gap Self))))
                (xt (* c d))
                (yt (* r d))
                (z (+ (* l (+ d (gap Self))) d)))
            (glTexCoord2f xt (- 1 yt))             (glVertex3f x z y)
            (glTexCoord2f xt (- 1 (+ yt d)))       (glVertex3f x z (+ y d))
            (glTexCoord2f (+ xt d) (- 1 (+ yt d))) (glVertex3f (+ x d)  z (+ y d))
            (glTexCoord2f (+ xt d) (- 1 yt))       (glVertex3f  (+ x d) z y)))))
    (glEnd)
    ;; bottom
    (use-texture Self "agent-lightgreen.png")
    (glBegin GL_QUADS)
    (glNormal3f 0.0 -1.0 0.0)
    (dotimes (r (segments Self))
      (dotimes (c (segments Self))
        (dotimes (l (segments Self))
          (let ((x (* c (+ d (gap Self))))
                (y (* r (+ d (gap Self))))
                (xt (* c d))
                (yt (* r d))
                (z (* l (+ d (gap Self)))))
            (glTexCoord2f xt (- 1 yt))             (glVertex3f x z y)
            (glTexCoord2f xt (- 1 (+ yt d)))       (glVertex3f x z (+ y d))
            (glTexCoord2f (+ xt d) (- 1 (+ yt d))) (glVertex3f (+ x d)  z (+ y d))
            (glTexCoord2f (+ xt d) (- 1 yt))       (glVertex3f  (+ x d) z y)))))
    (glEnd))
  (glPopMatrix))



;;;;

(defun SHOW-SPLASH-SCREEN (&key (Timeout nil))
  (if *Animated-Splash-View*
    (lui::bring-to-front (window *Animated-Splash-View*))
    (if Timeout
      (load-object "lui:resources;windows;Splash_Screen_timeout.window" :package (find-package :xlui))
      (load-object "lui:resources;windows;Splash_Screen.window" :package (find-package :xlui))))
  ;; depending on whether we are in Lite mode or not set the text and url of the AgentCubes link
  (let ((Link (view-named (window *Animated-Splash-View*) "agentcubes link")))
    (lui::in-main-thread ()  ;;; necessary on OS X 10.7.0, why??
       (cond (*AgentCubes-lite-mode*
              (setf (text Link) "About AgentCubes Lite.")
              (setf (url Link) "https://199.45.162.68/wiki/AgentCubes_Lite"))
             (t 
              (setf (text Link) "About AgentCubes.")
              (setf (url Link) "https://199.45.162.68/wiki/"))))))






#|  Examples:

(show-splash-screen)

(show-splash-screen :timeout t)



(defparameter *Window*

<agentcubes-splash-window margin="0">
  <column align="stretch" valign="stretch">>
    <agentcube-view name="cubes" vflex="1">
      <agent-hyper-cube segments="4"/>
    </agentcube-view>
    <slider action="adjust-segments-action" min-value="2.0" max-value="16.0" tick-marks="15"/>
    <slider action="adjust-gap-action" min-value="0.0" max-value="4.0"/>
  </column>
</agentcubes-splash-window>

)



(lui::save-frame-buffer-as-image (view-named *Window* "cubes") "/Users/alex/Desktop/AgentCubesIcon.png")


(load-object "lui:resources;windows;Splash_Screen 720p.window" :package (find-package :xlui))



|#



