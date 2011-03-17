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
   (red :accessor red :initform 1.0 :initarg :red)
   (green :accessor green :initform 0.0 :initarg :green)
   (blue :accessor blue :initform 0.0 :initarg :blue)
   (rotation-speed :accessor rotation-speed :initform 0.0 :documentation "degrees per second")))


(defmethod PREPARE-OPENGL ((Self globe-view))
  ;; GL setup
  (glClearColor 0.0 0.0 0.0 0.0)
  ;; camera
  (aim-camera (camera Self) :eye-z 10.0 :near 1.0)
  (start-animation self))
    

(defmethod DRAW ((Self globe-view))
  (glClearColor 0.0 0.0 0.0 0.0)
  (glPushMatrix)
  (glRotatef (roll Self) 0.0 0.0 1.0)
  (glRotatef (heading Self) 0.0 1.0 0.0)
  (glRotatef (pitch Self) 1.0 0.0 0.0)


  (glBegin GL_POLYGON)

  (glColor3f (red self) (green self) (blue self))

  (glVertex3f 1.0 1.0 -1.0)
  (glVertex3f -1.0 1.0 -1.0)
  (glVertex3f -1.0 1.0 1.0)
  (glVertex3f 1.0 1.0 0.0)

  (glVertex3f 1.0 -1.0 1.0)
  (glVertex3f -1.0 -1.0 1.0)
  (glVertex3f -1.0 -1.0 -1.0)
  (glVertex3f 1.0 -1.0 -1.0)

  (glVertex3f 1.0 1.0 1.0)
  (glVertex3f -1.0 1.0 1.0)
  (glVertex3f -1.0 -1.0 1.0)
  (glVertex3f 1.0 -1.0 1.0)

  (glVertex3f 1.0 -1.0 -1.0)
  (glVertex3f -1.0 -1.0 -1.0)
  (glVertex3f -1.0 1.0 -1.0)
  (glVertex3f 1.0 1.0 -1.0)
  
  (glVertex3f -1.0 1.0 1.0)
  (glVertex3f -1.0 1.0 -1.0)
  (glVertex3f -1.0 -1.0 -1.0)
  (glVertex3f -1.0 -1.0 1.0)

  (glVertex3f -1.0 1.0 1.0)
  (glVertex3f -1.0 1.0 -1.0)
  (glVertex3f -1.0 -1.0 -1.0)
  (glVertex3f -1.0 -1.0 1.0)

  (glVertex3f 1.0 1.0 -1.0)
  (glVertex3f 1.0 1.0 1.0)
  (glVertex3f 1.0 -1.0 1.0)
  (glVertex3f 1.0 -1.0 -1.0)

  (glEnd)
  
  (glPopMatrix))

(defmethod ANIMATE ((Self globe-view) Dt)
  ;; if frame rate is dropping below 25 [fps] => alarm. 
  ;; Probably some other CPU intensive application like iTunes
  ;;(when (>= dt 0.04) (ed-beep))   
  (setf (heading Self)
        (mod (+ (heading Self) (* dt (rotation-speed Self))) 360.0)))

;; Window 

(defclass LOGO-WINDOW (application-window)
  ())

;; actions 

(defmethod ADJUST-SPEED ((Window logo-window) (Slider slider))
  (setf (rotation-speed (view-named Window "globe")) (value Slider))
  (setf (text (view-named Window "speed label")) (format nil "~,1F [deg/s]" (value Slider))))

(defmethod COMPUTE-FRAME-RATE ((Window logo-window) (button bevel-button))
  (print (frame-rate (view-named Window "globe"))))

(defmethod color-action ((window window) (self COLOR-WELL-Control))
  (let ((globe (view-named Window "globe")))
    (setf (red   globe) (do-divide (get-red self))) 
    (setf (green globe)  (do-divide (get-green self)))
    (setf (blue  globe) (do-divide (get-blue self)))
    (display globe)))
  
(defun do-divide (value)
  (/ value 255.0))

;; Window with speed slider and start/stop animation control
(setq cw 
<logo-window title="Logo" margin="0">
  <column align="stretch" valign="stretch">
    <globe-view name="globe" vflex="1"/>
      <row minimize="vertical" align="stretch" valign="middle">
         <label text="speed" align="right" width="50"/>
         <slider name="speed slider" action="adjust-speed" max-value="360.0" flex="1"/>
         <label text="0.0 [deg/s]" name="speed label" width="100"/>
      </row>
      <row minimize="vertical" valign="middle" align="stretch">
        <separator  flex="1"/>
        <bevel-button text="frame rate" action="compute-frame-rate" width="100"/>
        <separator width="50" flex="1"/>
        <color-well  action="color-action" color="FF00FF" width="100" height="30"/>
      </row>
  </column>
</logo-window>

)

#| Examples:

<application-window margin="0" title="AgentSheets Nation">
  <globe-view/>
</application-window>

|#