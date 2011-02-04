;;; LUI/OpenGL example: Brownian Motion, 3D trajectory of a particle with alpha plotting
;;; Alexander Repenning 04/26/09

;; Windows Issues:
;; This example always seems to have the "text editing" cursor when ever you mouse over the openGLView
;; I experienced this issue on the an IMac running Windows 7 on Paralells.  
(in-package :xlui)


(defclass BROWNIAN-MOTION (opengl-dialog)
  ((points :accessor points :initform nil)))


(defmethod PREPARE-OPENGL ((Self brownian-motion))
  ;; Alpha
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  ;; walk
  (let ((x 0.0) (y 0.0) (z 0.0))
    (dotimes (i 500000)
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


;; Mouse Gesture Support

(defmethod VIEW-MOUSE-SCROLL-WHEEL-EVENT-HANDLER ((Self brownian-motion) x y dx dy)
  (declare (ignore x y))
  ;;(format t "~%dy=\"~A\"" dy)
  (with-animation-locked
      (track-mouse-pan (camera Self) dx dy 0.01)
    (display Self)))


(defmethod GESTURE-MAGNIFY-EVENT-HANDLER ((Self brownian-motion) x y Magnification)
  (declare (ignore x y))
  (with-animation-locked
      (track-mouse-zoom (camera Self) 0 Magnification -40.0)
    (display Self)))


(defmethod GESTURE-ROTATE-EVENT-HANDLER ((Self brownian-motion) x y Rotation)
  (declare (ignore x y))
  (with-animation-locked
      (track-mouse-spin (camera Self) Rotation 0.0 +0.1)
    (display Self)))


<application-window title="Brownian Motion" margin="0">
  <brownian-motion/>
</application-window>


#| Examples:


|#