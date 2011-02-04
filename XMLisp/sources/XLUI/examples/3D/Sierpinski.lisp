;;; LUI/OpenGL example: Sierpinski Triangles (based on Example openmcl FFI by hamlink)
;;; Alexander Repenning 04/27/09

;; Windows Issues:
;; The first time this example is laoded, the triangles all show up white, if you close the window
;; and reload the example, the triangles will be red (ax they should be).  Also, if you resize
;; the window after reloading it you will get an error in the method windowDidResize (this does
;; not seem related to the other windowDidResize issue because windowDidResize: should in fact
;; be called at this point.  
(in-package :xlui)


(defclass SIERPINSKI (opengl-dialog)
  ((iterations :accessor iterations :initform 5000 :type integer)))


(defmethod PREPARE-OPENGL ((Self sierpinski))
  (glColor3f 1.0 0.0 0.0)
  ;; camera
  (aim-camera (camera Self) :eye-z 1000.0))


(defmethod DRAW ((Self sierpinski))
  (let ((bounds #2a((0.0 0.0) (250.0 500.0) (500.0 0.0)))
	(point #(75.0 50.0)))
    (glBegin GL_POINTS)
    (dotimes (i (iterations Self))
      (let ((j (random 3)))
	(setf (aref point 0) (/ (+ (aref point 0) (aref bounds j 0)) 2.0)
	      (aref point 1) (/ (+ (aref point 1) (aref bounds j 1)) 2.0))
	(glVertex2f (aref point 0) (aref point 1))))
    (glEnd)))
  

;; GUI

<application-window title="Sierpinski Triangles" margin="0">
  <sierpinski iterations="10000"/>
</application-window>
