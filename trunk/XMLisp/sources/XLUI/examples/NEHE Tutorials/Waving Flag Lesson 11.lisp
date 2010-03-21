;;;;
;;; Nehe Lesson #11 - A Waving Flag
;;; http://nehe.gamedev.net/data/lessons/lesson.asp?lesson=11 
;;; XMLisp version by John Miller, millejoh@mac.com, 08/31/09
;;; different from NEHE: separate rendering/animation; frame rate independent

(in-package :xlui)


(defclass FLAG-VIEW (agent-3d-view)
  ()
  (:documentation "NEHE Lesson 11: Waving Flag"))


(defmethod PREPARE-OPENGL  ((view flag-view))
  ;; Back facing polygons are filled, while front facing are only outlined.
  ;; Apparently this is a setting of "personal preference" and the topic is more
  ;; fully discussedin the Red Book.
  (glShadeModel GL_SMOOTH)
  (glEnable GL_TEXTURE_2D)
  (glClearColor 0.0 0.0 0.0 0.5)
  (glClearDepth 1.0d0)
  (glEnable GL_DEPTH_TEST)
  (glDepthFunc GL_LEQUAL)
  (glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST)
  (glPolygonMode GL_BACK GL_FILL)
  (glPolygonMode GL_FRONT GL_LINE))
  

(defclass FLAG (agent-3d)
  ((wiggle-time :accessor wiggle-time :initarg :time-elapsed :initform 0 :documentation "time since last wiggle.")
   (points :accessor points :documentation "Points on the mesh of the texture that will be 'waved.'")))


(defmethod INITIALIZE-INSTANCE :after ((self flag) &rest initargs)
  (declare (ignore initargs))
  (let ((arr (make-array '(45 45 3) :initial-element 0.0)))
    (dotimes (x 45)
      (dotimes (y 45)
        (setf (aref arr x y 0) (- (/ x 5.0) 4.5)
              (aref arr x y 1) (- (/ y 5.0) 4.5)
              (aref arr x y 2) (sin (* 2 #.(coerce PI 'single-float) (/ (* (/ x 5.0) 40.0) 360.0))))))
    (setf (points self) arr)))


(defmethod DRAW ((agent flag))
  (with-slots (points) agent
    (glEnable GL_TEXTURE_2D)
    (use-texture (view agent) "AmericanFlag.png")  ;;; image in /resources/textures/
    (glBegin GL_QUADS)
      (dotimes (x 44)
        (dotimes (y 44)
          (let ((xb (1+ x))           (yb (1+ y))
                (fx (/ x 44.0))       (fy (/ y 44.0))
                (fxb (/ (1+ x) 44.0)) (fyb (/ (1+ y) 44.0))) ;; Texture coordinates for the flag.
            (glTexCoord2f fx fy)
            (glVertex3f (aref points x y 0) (aref points x y 1) (aref points x y 2))
            (glTexCoord2f fx fyb)
            (glVertex3f (aref points x yb 0) (aref points x yb 1) (aref points x yb 2))
            (glTexCoord2f fxb fyb)
            (glVertex3f (aref points xb yb 0) (aref points xb yb 1) (aref points xb yb 2))
            (glTexCoord2f fxb fy)
            (glVertex3f (aref points xb y 0) (aref points xb y 1) (aref points xb y 2)))))
    (glEnd)))


(defmethod WIGGLE ((Self flag))
  ;; propagate wave one grid position
  (with-slots (points) Self
    (dotimes (y 45)
      (let ((hold (aref points 0 y 2)))
        (dotimes (x 44)
          (setf (aref points x y 2) (aref points (1+ x) y 2)))
        (setf (aref points 44 y 2) hold)))))


(defmethod ANIMATE ((agent flag) dt)
  ;; frame rate independent animation
  (incf (wiggle-time agent) dt)
  (when (> (wiggle-time agent) 0.02)
    (wiggle agent)
    (setf (wiggle-time agent) 0))
  (incf (pitch agent) (* 10 dt)) ;; Rotation around x plane.
  (incf (heading agent) (* 10 dt)) ;; Rotation around y plane.
  (incf (roll agent) (* 10 dt))) ;; Rotation around z plane.


;; GUI: window with resizable 3D view

(defparameter *American-Flag*

<application-window title="Nehe Lesson 11, Waving Flag" margin="0">
  <flag-view name="the flag">
    <camera eye-z="10.0"/>
    <flag/>
  </flag-view>
</application-window>

)


(start-animation (view-named *American-Flag* "the flag"))
;; (stop-animation (view-named *American-Flag* "the flag"))

;; (frame-rate (view-named *American-Flag* "the flag"))


