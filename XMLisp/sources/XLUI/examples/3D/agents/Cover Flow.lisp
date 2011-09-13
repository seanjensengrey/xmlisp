;;; LUI Example: Cover Flow
;;; concepts: camera control through slider; cover flow-like reflections
;;; Alexander Repenning 04/25/09

;; Windows Issues:
;; When you open this example, for some reason all of the items show up lined up in a row, 
;; but when you click on one they will warp to the location that they begin at when you open
;; the example on the mac.  
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


(defmethod PREPARE-OPENGL ((Self cover-flow-view))
  ;; GL setup
  (glClearColor 0.0 0.0 0.0 0.0)
  (glShadeModel GL_SMOOTH)
  (glPixelStorei GL_UNPACK_ALIGNMENT 1)
  ;; define material
  (glmaterialfv GL_FRONT_AND_BACK GL_SPECULAR { 0.5 0.5 0.5 0.0 })
  (glMaterialf  GL_FRONT_AND_BACK GL_SHININESS 128.0)
  (glMaterialfv GL_FRONT_AND_BACK GL_AMBIENT_AND_DIFFUSE { 1.0 1.0 1.0 1.0 })
  (glEnable GL_COLOR_MATERIAL)
  (glColorMaterial GL_FRONT_AND_BACK GL_AMBIENT_AND_DIFFUSE)
  ;; light
  (glLightfv GL_LIGHT0 GL_POSITION { 3.0 3.0 3.0 1.0 })
  (glLightfv GL_LIGHT0 GL_DIFFUSE { 1.0 1.0 1.0 1.0 })
  (glLightfv GL_LIGHT0 GL_SPECULAR { 1.0 1.0 1.0 1.0 })
  ;; enablers
  (glDisable GL_LIGHTING)
  (glEnable GL_LIGHT0)
  (glEnable GL_DEPTH_TEST)
  ;; Alpha
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  ;; texture
  (glEnable GL_TEXTURE_2D)
  (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
  ;; camera
  (aim-camera (camera Self) :eye-z 10.0)) 


(defmethod CLEAR-BACKGROUND ((Self cover-flow-view))
  (glClearColor 0.0 0.0 0.0 1.0)
  (glClear #.(logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT)))


(defmethod INITIALIZE-INSTANCE :after ((Self cover-flow-view) &rest Args)
  (declare (ignore Args))
  ;; space covers
  (let ((X 0.0))
    (dolist (Agent (agents Self))
      (setf (x Agent) x)
      (incf x 1.0))))

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
  (glBegin GL_QUADS)
  ;; Cover 
  (glNormal3f 0.0 0.0 1.0)
  (glColor4f 1.0 1.0 1.0 1.0)
  (glTexCoord2f 0.0 0.0) (glVertex2f 0.0 0.0)
  (glTexCoord2f 0.0 1.0) (glVertex2f 0.0 1.0)
  (glTexCoord2f 1.0 1.0) (glVertex2f 1.0 1.0)
  (glTexCoord2f 1.0 0.0) (glVertex2f 1.0 0.0)
  ;; reflection
  (glColor4f 1.0 1.0 1.0 0.5)
  (glTexCoord2f 0.0 0.0) (glVertex2f 0.0 0.0)
  (glColor4f 1.0 1.0 1.0 0.1)
  (glTexCoord2f 0.0 1.0) (glVertex2f 0.0 -1.0)
  (glTexCoord2f 1.0 1.0) (glVertex2f 1.0 -1.0)
  (glColor4f 1.0 1.0 1.0 0.5)
  (glTexCoord2f 1.0 0.0) (glVertex2f 1.0 0.0)
  (glEnd))


;; Actions

(defmethod ADJUST-POSITION ((Window cover-flow-window) (Slider slider))
  (let ((Value (* 10.0 (value Slider))))
    (setf (value (view-named Window "cover flow view")) Value)
    (aim-camera (camera (view-named Window "cover flow view")) :eye-x Value :center-x Value)
    (display (view-named Window "cover flow view"))))


;; Window

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

