;;; LUI/OpenGL example: Brownian Motion, 3D trajectory of a particle with alpha plotting
;;; Alexander Repenning 04/26/09

(in-package :xlui)


(defclass BROWNIAN-MOTION (opengl-dialog)
  ((points :accessor points :initform nil)))


(defmethod PREPARE-OPENGL ((Self brownian-motion))
  ;; Alpha
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  ;; walk
  (let ((x 0.0) (y 0.0) (z 0.0))
    (dotimes (i 50000)
      (incf x (- (random 2.0) 1.0))
      (incf y (- (random 2.0) 1.0))
      (incf z (- (random 2.0) 1.0))
      (push (list x y z) (points Self))))
  ;; camera
  (aim-camera (camera Self) :eye-z 200.0))


(defmethod DRAW ((Self brownian-motion))
  (glBegin GL_LINE_STRIP)  ;; direct mode is slow but can still handle quite a few points
  (glColor4f 0.7 0.7 1.0 0.15)  ;; alpha to make clusters more visible
  (dolist (Point (points Self))
    (glVertex3f (first Point) (second Point) (third Point)))
  (glEnd))


(defmethod CLEAR-BACKGROUND ((Self brownian-motion))
  (glClearColor 0.0 0.0 0.0 1.0)
  (glClear #.(logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT)))



<application-window title="Brownian Motion" margin="0">
  <brownian-motion/>
</application-window>


#| Examples:


|#