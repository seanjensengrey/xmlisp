;;; XMLisp example: AgentSheet Globe
;;; Alexander Repenning 11/18/02
;;; concepts: basic animation, interaction between controls and animation
;;; Updated: 05/27/09
;;; 10/18/06 stricter typing gltexenvf -> gltexenvi
;;; 04/20/09 Clozure CL

(in-package :xlui)

;; View 

(defclass GLOBE-VIEW (opengl-dialog)
  ((quadric :accessor quadric :initform nil)
   (roll :accessor roll :initform 0.0 :initarg :roll)
   (pitch :accessor pitch :initform -90.0 :initarg :pitch)
   (heading :accessor heading :initform 0.0 :initarg :heading)
   (rotation-speed :accessor rotation-speed :initform 0.0 :documentation "degrees per second")))


(defmethod PRINT-SLOTS ((Self globe-view))
   '())


(defmethod PREPARE-OPENGL ((Self globe-view))
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
  (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_DECAL)
  ;; Quadric used for globe sphere
  (setf (quadric Self) (gluNewQuadric))
  (gluQuadricDrawStyle (quadric Self) GLU_FILL)
  (gluQuadricOrientation (quadric Self) GLU_OUTSIDE)
  (gluQuadricNormals (quadric Self) GLU_SMOOTH)
  (gluQuadricTexture (quadric Self) GL_TRUE)
  ;; Alpha
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  ;; camera
  (aim-camera (camera Self) :eye-z 40.0 :near 1.0))
    

(defmethod DRAW ((Self globe-view))
  (glPushMatrix)
  (glRotatef (roll Self) 0.0 0.0 1.0)
  (glRotatef (heading Self) 0.0 1.0 0.0)
  (glRotatef (pitch Self) 1.0 0.0 0.0)
  ;; sphere
  (gltexenvi gl_texture_env gl_texture_env_mode gl_modulate)
  (use-texture Self "warhol_flat.png")
  (glEnable gl_texture_2d)
  (gluSphere (quadric Self) 10d0 40 40)
  (glPopMatrix))

(defmethod DRAW :after ((Self globe-view))
  (glFlush))


(defmethod ANIMATE ((Self globe-view) Dt)
  ;; if frame rate is dropping below 25 [fps] => alarm. 
  ;; Probably some other CPU intensive application like iTunes
  ;;(when (>= dt 0.04) (ed-beep))   
  ;( (name self))
  (setf (heading Self)
        (mod (+ (heading Self) (* dt (rotation-speed Self))) 360.0)))


;; Window 

(defclass LOGO-WINDOW (application-window)
  ())



;; actions 

(defmethod ADJUST-SPEED ((Window logo-window) (Slider slider))
  (setf (rotation-speed (view-named Window "globe")) (value Slider))
  (setf (text (view-named Window "speed label")) (format nil "~,1F [deg/s]" (value Slider))))


(defmethod START-ACTION ((Window logo-window) (button bevel-button))
  (start-animation (view-named Window "globe")))


(defmethod STOP-ACTION ((Window logo-window) (button bevel-button))
  (stop-animation (view-named Window "globe")))


(defmethod COMPUTE-FRAME-RATE ((Window logo-window) (button bevel-button))
  (print (frame-rate (view-named Window "globe"))))



;; Window with speed slider and start/stop animation control

<logo-window title="Logo" margin="0">
  <column align="stretch" valign="stretch">
    <globe-view name="globe" vflex="1"/>
      <row minimize="vertical" align="stretch" valign="middle">
         <label text="speed" align="right" width="50"/>
         <slider name="speed slider" action="adjust-speed" max-value="360.0" flex="1"/>
         <label text="0.0 [deg/s]" name="speed label" width="100"/>
      </row>
      <row minimize="vertical" align="center">
        <bevel-button text="start" action="start-action" width="55"/>
        <bevel-button text="stop" action="stop-action" width="55"/>
        <bevel-button text="frame rate" action="compute-frame-rate" width="100"/>
      </row>
  </column>
</logo-window>
