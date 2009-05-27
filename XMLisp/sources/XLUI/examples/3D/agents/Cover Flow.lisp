;;; LUI Example: Cover Flow
;;; Alexander Repenning 04/25/09

(in-package :xlui)

;;-------------------------
;; window                  |
;;-------------------------

(defclass COVER-FLOW-WINDOW (application-window)
  ())

;;-------------------------
;; View                    |
;;-------------------------

(defclass COVER-FLOW-VIEW (agent-3d-view)
  ((value :accessor value :initform 0.0)))


(defmethod INIT ((Self cover-flow-view))
  ;; GL setup
  (glClearColor 0.0 0.0 0.0 0.0)
  (glShadeModel gl_smooth)
  (glPixelStorei gl_unpack_alignment 1)
  ;; define material
  (glmaterialfv gl_front_and_back gl_specular { 0.5 0.5 0.5 0.0 })
  (glmaterialf gl_front_and_back gl_shininess 128.0)
  (glmaterialfv gl_front_and_back gl_ambient_and_diffuse { 1.0 1.0 1.0 1.0 })
  (glEnable gl_color_material)
  (glColorMaterial gl_front_and_back gl_ambient_and_diffuse)
  ;; light
  (gllightfv gl_light0 gl_position { 3.0 3.0 3.0 1.0 })
  (gllightfv gl_light0 gl_diffuse { 1.0 1.0 1.0 1.0 })
  (gllightfv gl_light0 gl_specular { 1.0 1.0 1.0 1.0 })
  ;; enablers
  (gldisable gl_lighting)
  (glenable gl_light0)
  (glenable gl_depth_test)
  ;; Alpha
  (glenable gl_blend)
  (glBlendFunc gl_src_alpha gl_one_minus_src_alpha)
  ;; texture
  (glEnable gl_texture_2d)
  (gltexenvi gl_texture_env gl_texture_env_mode gl_modulate)
  ;; camera
  (aim-camera (camera Self) :eye-z 10.0)) 


(defmethod CLEAR-BACKGROUND ((Self cover-flow-view))
  (glClearColor 0.0 0.0 0.0 1.0)
  (glClear #.(logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT)))

;;-------------------------
;; Agent                   |
;;-------------------------

(defclass COVER (agent-3d)
  ((texture :accessor texture :initform "skyIsland.png")))


(defmethod DRAW ((Self cover))
  (call-next-method)
  (glEnable GL_TEXTURE_2D)
  (setf (heading Self) (* 90.0 (sin (+ (x self) (* 1.0 (value (view Self)))))))
  (setf (z Self) (* 2.0 (sin (+ (x self) (* 1.0 (value (view Self)))))))
  (use-texture Self (texture Self))
  (glbegin gl_quads)
  ;; Cover 
  (glnormal3f 0.0 0.0 1.0)
  (glcolor4f 1.0 1.0 1.0 1.0)
  (gltexcoord2f 0.0 0.0) (glvertex2f 0.0 0.0)
  (gltexcoord2f 0.0 1.0) (glvertex2f 0.0 1.0)
  (gltexcoord2f 1.0 1.0) (glvertex2f 1.0 1.0)
  (gltexcoord2f 1.0 0.0) (glvertex2f 1.0 0.0)
  ;; reflection
  (glcolor4f 1.0 1.0 1.0 0.5)
  (gltexcoord2f 0.0 0.0) (glvertex2f 0.0 0.0)
  (glcolor4f 1.0 1.0 1.0 0.1)
  (gltexcoord2f 0.0 1.0) (glvertex2f 0.0 -1.0)
  (gltexcoord2f 1.0 1.0) (glvertex2f 1.0 -1.0)
  (glcolor4f 1.0 1.0 1.0 0.5)
  (gltexcoord2f 1.0 0.0) (glvertex2f 1.0 0.0)
  (glEnd))

#| Examples:


(defmethod ADJUST-POSITION ((Window cover-flow-window) (Slider slider))
  (let ((Value (* 10.0 (value Slider))))
    (setf (value (view-named Window "cover flow view")) Value)
    (aim-camera (camera (view-named Window "cover flow view")) :eye-x Value :center-x Value)
    (display (view-named Window "cover flow view"))))


(defmethod initialize-instance :after ((Self cover-flow-view) &rest Args)
  ;; space covers
  (let ((X 0.0))
    (dolist (Agent (agents Self))
      (setf (x Agent) x)
      (incf x 1.0))))


<cover-flow-window title="Cover Flow" margin="0">
  <column align="stretch" valign="stretch">
    <cover-flow-view name="cover flow view" vflex="1">
      <cover texture="skyIsland.png"/>
      <cover texture="skyIsland.png"/>
      <cover texture="skyIsland.png"/>
      <cover texture="skyIsland.png"/>
      <cover texture="skyIsland.png"/>
      <cover texture="skyIsland.png"/>
      <cover texture="skyIsland.png"/>
      <cover texture="skyIsland.png"/>
      <cover texture="skyIsland.png"/>
      <cover texture="skyIsland.png"/>
      <cover texture="skyIsland.png"/>
      <cover texture="skyIsland.png"/>
    </cover-flow-view>
    <row minimize="vertical" align="stretch" valign="middle">
       <label text="cover" align="right" width="50"/>
       <slider name="cover slider" action="adjust-position" max-value="1.0" flex="1"/>
       <label text="0.0" name="position label" width="50"/>
    </row>
  </column>
</cover-flow-window>




|#