;; Trivial OpenGL example
;; 8/25/09 Alexander Repenning
;; how to make a slow circle

(in-package :xlui)

(defclass CIRCLE-VIEW (opengl-dialog)
  ((radius :accessor radius :initform 1.0 :type float)
   (slices :accessor slices :initform 10 :type integer)))
 

(defmethod DRAW ((Self circle-view))
  (let ((delta (/ (* (float pi 0.0) 2.0) (slices Self))))
    (glBegin GL_LINE_LOOP)
    (dotimes (i (slices Self))
      (let ((angle (* i delta)))
        (glVertex2f (* (cos angle) (radius Self)) (* (sin angle) (radius Self)))))
    (glEnd)))


<application-window title="circle, really?" y="50">
  <circle-view slices="3"/>
</application-window>


<application-window title="circle, kinda?" y="300">
  <circle-view slices="6"/>
</application-window>


<application-window title="circle" y="550">
  <circle-view slices="40"/>
</application-window>


#| Examples:

(add-subview 
 (make-instance 'window :width 300 :height 300)
 (make-instance 'circle-view :width 300 :height 300))

|#