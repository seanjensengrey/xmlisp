;;; Open Agent Engine example: Voice Rings
;;; an animation of concentric circles to indicate the location of speech
;;; Alexander Repenning 2/26/07


(in-package :xlui)


(defclass RING-VIEW (opengl-dialog)
  ((radius :accessor radius :initform 1.0)
   (growth :accessor growth :initform 0.2))
  (:documentation "window with aninmation to show the location of a sound or speech source"))


(defmethod INIT ((Self ring-view))
  ;; GL setup
  (glClearColor 0.0 0.0 0.0 0.0)
  (glShadeModel GL_SMOOTH)
  (glPixelStorei GL_UNPACK_ALIGNMENT 1)
  ;; enablers
  (glDisable GL_LIGHTING)
  (glDisable GL_DEPTH_TEST)
  (glEnable GL_POINT_SMOOTH)
  ;; Textures
  (glEnable GL_TEXTURE_2D)
  ;; Alpha
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  ;; camera
  (aim-camera (camera Self) :eye-z 2.0 :near 1.0))
    

(defmethod DRAW ((Self ring-view))
  (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
  (use-texture Self "voicering.png") 
  (glNormal3f 0.0 0.0 1.0)
  (let ((R (radius Self)) (-R (- (radius Self))))
    (glDisable GL_DEPTH_TEST)
    (loop
      (glColor4f 1.0 1.0 1.0 (- 1.0 R))  ;; fade color for large radius
      (glBegin GL_QUADS)
        (glTexCoord2i 0 0) (glVertex2f -r -r)
        (glTexCoord2i 0 1) (glVertex2f -r  r)
        (glTexCoord2i 1 1) (glVertex2f  r  r)
        (glTexCoord2i 1 0) (glVertex2f  r -r)
      (glEnd)
      (decf R (growth Self))
      (when (< R 0.0) (return))
      (incf -R (growth Self)))))



(defmethod ANIMATE ((Self ring-view) Dt)
  ;; advance radius or snap back to next smaller ring
  (if (>= (radius Self) 1.0)
    (setf (radius Self) (- 1.0 (growth Self)))
    (incf (radius Self) (* 0.6 Dt))))


;; GUI

(defclass RING-WINDOW (application-window)
  ())


(defmethod ADJUST-SPEED ((Window ring-window) (Slider slider))
  (setf (growth (view-named Window "rings")) (value Slider))
  (setf (text (view-named Window "speed label")) (format nil "~,2F" (value Slider))))


(defmethod START-ACTION ((Window ring-window) (button bevel-button))
  (start-animation (view-named Window "rings")))


(defmethod STOP-ACTION ((Window ring-window) (button bevel-button))
  (stop-animation (view-named Window "rings")))


<ring-window title="Voice Rings" margin="0">
  <column align="stretch" valign="stretch">
    <ring-view name="rings" vflex="1"/>
      <row minimize="vertical" align="stretch" valign="middle">
         <label text="speed" align="right" width="50"/>
         <slider name="speed slider" action="adjust-speed" min-value="0.05" max-value="2.0" flex="1"/>
         <label text="0.0" name="speed label" width="50"/>
      </row>
      <row minimize="vertical" align="center">
        <bevel-button text="start" action="start-action" width="55"/>
        <bevel-button text="stop" action="stop-action" width="55"/>
      </row>
  </column>
</ring-window>

