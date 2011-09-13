;; Trivial OpenGL example
;; 8/25/09 Alexander Repenning
;; how to make a slow circle
;; Windows Issues:
;; When this example first stats up, the slider has no knob, as soon as you click somewhere
;; on the slider the knob will appear at the spot where you click.  Also, sometimes when you
;; hove over the openGL view, you see the text editing cursor instead of the arrow cursor.   
(in-package :xlui)

(defclass CIRCLE-VIEW (opengl-dialog)
  ((radius :accessor radius :initform 1.0 :type float)
   (slices :accessor slices :initform 3 :type integer)))


(defmethod PREPARE-OPENGL ((Self circle-view))
  ;; this code will run once before the first drawing
  (glColor3f 1.0 0.0 0.0))


(defmethod DRAW ((Self circle-view))
  (let ((delta (/ (* (float pi 0.0) 2.0) (slices Self))))
    (glBegin GL_LINE_LOOP)
    (dotimes (i (slices Self))
      (let ((angle (* i delta)))
        (glVertex2f (* (cos angle) (radius Self)) (* (sin angle) (radius Self)))))
    (glEnd)))


(defmethod ADJUST-SLICES-ACTION ((Window application-window) (Slider slider))
  (setf (slices (view-named Window "crappy circle")) (truncate (value Slider)))
  (display (view-named Window "crappy circle")))


<application-window title="circle, really?" y="50">
  <column align="stretch" valign="stretch">>
    <circle-view name="crappy circle" vflex="1"/>
    <slider action="adjust-slices-action" min-value="3.0" max-value="14.0" tick-marks="12"/>
  </column>
</application-window>



#| Examples:

(add-subview 
 (make-instance 'window :width 300 :height 300)
 (make-instance 'circle-view :width 300 :height 300))

|#