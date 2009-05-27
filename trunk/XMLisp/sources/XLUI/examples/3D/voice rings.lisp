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
  (glShadeModel gl_smooth)
  (glPixelStorei gl_unpack_alignment 1)
  ;; enablers
  (glDisable gl_lighting)
  (glDisable gl_depth_test)
  (glEnable gl_point_smooth)
  ;; Textures
  (glenable gl_texture_2d)
  ;; Alpha
  (glenable gl_blend)
  (glBlendFunc gl_src_alpha gl_one_minus_src_alpha)
  ;; camera
  (aim-camera (camera Self) :eye-z 2.0 :near 1.0))
    

(defmethod DRAW ((Self ring-view))
  (gltexenvi gl_texture_env gl_texture_env_mode gl_modulate)
  (use-texture Self "voicering.png") 
  (glnormal3f 0.0 0.0 1.0)
  (let ((R (radius Self)) (-R (- (radius Self))))
    (glDisable gl_depth_test)
    (loop
      (glColor4f 1.0 1.0 1.0 (- 1.0 R))  ;; fade color for large radius
      (glbegin gl_quads)
        (gltexcoord2i 0 0) (glvertex2f -r -r)
        (gltexcoord2i 0 1) (glvertex2f -r  r)
        (gltexcoord2i 1 1) (glvertex2f  r  r)
        (gltexcoord2i 1 0) (glvertex2f  r -r)
      (glend)
      (decf R (growth Self))
      (when (< R 0.0) (return))
      (incf -R (growth Self)))))



(defmethod ANIMATE ((Self ring-view) Dt)
  ;; advance radius or snap back to next smaller ring
  (if (>= (radius Self) 1.0)
    (setf (radius Self) (- 1.0 (growth Self)))
    (incf (radius Self) (* 0.6 Dt))))




#| Examples:


(defclass RING-WINDOW (application-window)
  ())


(defmethod ADJUST-SPEED ((Window ring-window) (Slider slider))
  (setf (growth (view-named Window "rings")) (value Slider))
  (setf (text (view-named Window "speed label")) (format nil "~,2F" (value Slider))))


(defmethod START ((Window ring-window) (button bevel-button))
  (start-animation (view-named Window "rings")))


(defmethod STOP ((Window ring-window) (button bevel-button))
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
        <bevel-button text="start" action="start" width="55"/>
        <bevel-button text="stop" action="stop" width="55"/>
      </row>
  </column>
</ring-window>


|#
