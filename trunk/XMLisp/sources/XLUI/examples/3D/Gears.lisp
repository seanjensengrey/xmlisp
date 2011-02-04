;;; Translation of Gears.c by Brian Paul.
;;; Object-Oriented by Alexander Repenning 12/13/02
;;; 10/06/05 removed macros, fixed camera, removed MCL specific keys
;;; 10/16/06 avoid creating double floats
;;; 04/01/09 XMlisp version
;;; concepts: display lists

;; Windows Issues:
;; The frame rate on this this example is terrible, if you put the speed up to the max
;; you seem to only get about 2 frames per second.  This likely has something to do with 
;; the timing issue.  

(in-package :xlui)

(defvar *Pi-Single* (float Pi 0.0))


(defclass GEAR-DEMO (opengl-dialog)
  ((rotation-speed :accessor rotation-speed :initform 90.0 :initarg :rotation-speed :documentation "degrees per second")
   (rotx :accessor rotx :initform 20.0)
   (roty :accessor roty :initform 30.0)
   (rotz :accessor rotz :initform 0.0)
   (angle :accessor angle :initform 0.0)
   (gear1 :accessor gear1)
   (gear2 :accessor gear2)
   (gear3 :accessor gear3)))


(defun GEAR (Inner_Radius Outer_Radius Width Teeth Tooth_Depth)
 (let ((R0 inner_radius)
       (R1 (/ (- outer_radius tooth_depth) 2.0))
       (R2 (/ (+ outer_radius tooth_depth) 2.0))
       (Da (/ (* 2.0 *Pi-Single*) teeth 4.0)))
  
  (glNormal3f 0.0 0.0 1.0)
  
  ;;/* draw front face */
  (glBegin GL_QUAD_STRIP)
  (dotimes (I teeth)
   (let* ((Angle (float (/ (* i 2.0 *Pi-Single*) teeth) 0.0)))
    (glVertex3f (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5))
    (glVertex3f (* r1 (cos angle)) (* r1 (sin angle)) (* width 0.5))
    (glVertex3f (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5))
    (glVertex3f (* r1 (cos (+ angle (* 3 da))))
	      (* r1 (sin (+ angle (* 3 da))))
	      (* width 0.5))))
  (glEnd)

  ;;/* draw front sides of teeth */
  (glBegin GL_QUADS)
  (let ((Da (float (/ (* 2.0 *Pi-Single*) teeth 4.0) 0.0)))
   (dotimes (I teeth)
    (let ((Angle (float (/ (* i 2.0 *Pi-Single*) teeth) 0.0)))
     (glVertex3f (* r1 (cos angle)) (* r1 (sin angle)) (* width 0.5))
     (glVertex3f (* r2 (cos (+ angle da))) (* r2 (sin (+ angle da)))
	       (* width 0.5))
     (glVertex3f (* r2 (cos (+ angle (* 2 da))))
	       (* r2 (sin (+ angle (* 2 da))))
	       (* width 0.5))
     (glVertex3f (* r1 (cos (+ angle (* 3 da))))
	       (* r1 (sin (+ angle (* 3 da))))
	       (* width 0.5)))))
  (glEnd)

  (glNormal3f 0.0 0.0 -1.0)

  ;;/* draw back face */
  (glBegin GL_QUAD_STRIP)
  (dotimes (I teeth)
   (let ((Angle (float (/ (* i 2.0 *Pi-Single*) teeth) 0.0)))
    (glVertex3f (* r1 (cos angle)) (* r1 (sin angle)) (* (- width) 0.5))
    (glVertex3f (* r0 (cos angle)) (* r0 (sin angle)) (* (- width) 0.5))
    (glVertex3f (* r1 (cos (+ angle (* 3 da))))
	      (* r1 (sin (+ angle (* 3 da))))
	      (* (- width) 0.5))
    (glVertex3f (* r0 (cos angle)) (* r0 (sin angle)) (* (- width) 0.5))))
  (glEnd)

  ;;/* draw back sides of teeth */
  (glBegin GL_QUADS)
  (let ((Da (/ (* 2.0 *Pi-Single*) teeth 4.0)))
   (dotimes (I teeth)
    (let ((Angle (float (/ (* i 2.0 *Pi-Single*) teeth) 0.0)))
     (glVertex3f (* r1 (cos (+ angle (* 3 da))))
	       (* r1 (sin (+ angle (* 3 da))))
	       (* (- width) 0.5))
     (glVertex3f (* r2 (cos (+ angle (* 2 da))))
	       (* r2 (sin (+ angle (* 2 da))))
	       (* (- width) 0.5))
     (glVertex3f (* r2 (cos (+ angle da))) (* r2 (sin (+ angle da)))
	       (* (- width) 0.5))
     (glVertex3f (* r1 (cos angle)) (* r1 (sin angle)) (* (- width) 0.5)))))
  (glEnd)

  ;;/* draw outward faces of teeth */
  (glBegin GL_QUAD_STRIP)
  (dotimes (I teeth)
   (let ((Angle (float (/ (* i 2.0 *Pi-Single*) teeth) 0.0)))
    (glVertex3f (* r1 (cos angle)) (* r1 (sin angle)) (* width 0.5))
    (glVertex3f (* r1 (cos angle)) (* r1 (sin angle)) (* (- width) 0.5))
    (let* ((U (- (* r2 (cos (+ angle da))) (* r1 (cos angle))))
	   (V (- (* r2 (sin (+ angle da))) (* r1 (sin angle))))
	   (Len (sqrt (+ (* u u) (* v v)))))
     (setq u (/ u len))
     (setq v (/ v len))
     (glNormal3f v (- u) 0.0)
     (glVertex3f  (* r2 (cos (+ angle da)))
	       (* r2 (sin (+ angle da)))
	       (* width 0.5))
     (glVertex3f  (* r2 (cos (+ angle da)))
	       (* r2 (sin (+ angle da))) (* (- width) 0.5))
     (glNormal3f (cos angle) (sin angle) 0.0)
     (glVertex3f (* r2 (cos (+ angle (* 2 da))))
	       (* r2 (sin (+ angle (* 2 da))))
	       (* width 0.5))
     (glVertex3f (* r2 (cos (+ angle (* 2 da))))
	       (* r2 (sin (+ angle (* 2 da))))
	       (* (- width) 0.5))
     (let ((U (- (* r1 (cos (+ angle (* 3 da))))
		 (* r2 (cos (+ angle (* 2 da))))))
	   (V (- (* r1 (sin (+ angle (* 3 da))))
		 (* r2 (sin (+ angle (* 2 da)))))))
      (glNormal3f v (- u) 0.0)
      (glVertex3f (* r1 (cos (+ angle (* 3 da))))
		(* r1 (sin (+ angle (* 3 da))))
		(* width 0.5))
      (glVertex3f (* r1 (cos (+ angle (* 3 da))))
		(* r1 (sin (+ angle (* 3 da))))
		(* (- width) 0.5))
      (glNormal3f (cos angle) (sin angle) 0.0)))))
  (glVertex3f (* r1 (cos 0.0)) (* r1 (sin 0.0)) (* width 0.5))
  (glVertex3f (* r1 (cos 0.0)) (* r1 (sin 0.0)) (* (- width) 0.5))
  (glEnd)

  
  ;;/* draw inside radius cylinder */
  (glBegin GL_QUAD_STRIP)
  (dotimes (I teeth)
   (let ((Angle (float (/ (* i 2.0 *Pi-Single*) teeth) 0.0)))
    (glNormal3f (- (cos angle)) (- (sin angle)) 0.0)
    (glVertex3f (* r0 (cos angle)) (* r0 (sin angle)) (* (- width) 0.5))
    (glVertex3f (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5))))
  (glEnd)))


(defmethod DRAW ((Self gear-demo))
  (glPushMatrix)

  (glRotatef (rotx Self) 1.0 0.0 0.0)
  (glRotatef (roty Self) 0.0 1.0 0.0)
  (glRotatef (rotz Self) 0.0 0.0 1.0)
  
  (glPushMatrix)
  (glTranslatef  -3.0 -2.0 0.0)
  (glRotatef (angle Self) 0.0 0.0 1.0)
  (glCallList (gear1 Self))
  (glPopMatrix)
  
  (glPushMatrix)
  (glTranslatef  0.5 -2.0 0.0)
  (glRotatef (- (* -2.0 (angle Self)) 9.0) 0.0 0.0 1.0)
  (glCallList (gear2 Self))
  (glPopMatrix)
  
  (glPushMatrix)
  (glTranslatef  -3.1 1.5 0.0)
  (glRotatef (- (* -2.0 (angle Self)) 25.0) 0.0 0.0 1.0)
  (glCallList (gear3 Self))
  (glPopMatrix)
  
  (glPopMatrix))


(defmethod ANIMATE ((Self gear-demo) Time)
 (setf (angle Self)
       (mod (+ (angle Self) (* Time (rotation-speed Self))) 360.0)))


(defmethod VIEW-KEY-EVENT-HANDLER ((Self gear-demo) Key)
  (case Key
    (#\u  (print (incf (rotation-speed Self) 5)))
    (#\d (print (decf (rotation-speed Self) 5)))))


(defmethod PREPARE-OPENGL ((Self gear-demo))
  (glClearColor 0.5 0.5 0.5 1.0)
  (glShadeModel gl_smooth)
  ;; define material
  (glmaterialfv gl_front_and_back gl_specular {0.5 0.5 0.5 0.0})
  (glmaterialf gl_front_and_back gl_shininess 120.0)
  ;; light
  (gllightfv gl_light0 gl_position {4.0 1.0 1.0 1.0})
  (gllightfv gl_light0 gl_diffuse {0.5 0.5 0.5 1.0})
  (gllightfv gl_light0 gl_specular {5.0 5.0 5.0 1.0})
  ;; enablers
  (glenable gl_lighting)
  (glenable gl_light0)
  (glenable gl_depth_test)

  ;;/* make the gears */
  (setf (gear1 Self) (glGenLists 1))
  (glNewList (gear1 Self) GL_COMPILE)
  (glMaterialfv GL_FRONT GL_AMBIENT_AND_DIFFUSE {1.0 0.0 0.0 0.0})
  (gear 1.0 4.0 1.0 20 0.7)
  (glEndList)

  (setf (gear2 Self) (glGenLists 1))
  (glNewList (gear2 Self) GL_COMPILE)
  (glMaterialfv GL_FRONT GL_AMBIENT_AND_DIFFUSE {0.0 1.0 0.0 0.0})
  (gear 0.5 2.0 2.0 10 0.7)
  (glEndList)

  (setf (gear3 Self) (glGenLists 1))
  (glNewList (gear3 Self) GL_COMPILE)
  (glMaterialfv GL_FRONT GL_AMBIENT_AND_DIFFUSE {0.0 0.0 1.0 0.0})
  (gear 0.7 2.0 1.0 10 0.7)
  (glEndList)

  (glEnable GL_NORMALIZE)
  ;; optional: FSAA
  (glEnable GL_MULTISAMPLE)
  ;; Camera
  (aim-camera (camera Self) :eye-z 15.0))




;; GUI

(defclass GEAR-WINDOW (application-window)
  ())


(defmethod ADJUST-SPEED ((Window gear-window) (Slider slider))
  (setf (rotation-speed (view-named Window "gears")) (value Slider))
  (setf (text (view-named Window "speed label")) (format nil "~,1F [deg/s]" (value Slider))))


(defmethod START-ACTION ((Window gear-window) (button bevel-button))
  (start-animation (view-named Window "gears")))


(defmethod STOP-ACTION ((Window gear-window) (button bevel-button))
  (stop-animation (view-named Window "gears")))



(defparameter Wi 

<gear-window title="OpenGL 3D Gears" margin="0">
  <column align="stretch" valign="stretch">
    <gear-demo name="gears" vflex="1"/>
      <row minimize="vertical" align="stretch" valign="middle">
         <label text="speed" align="right" width="50"/>
         <slider name="speed slider" action="adjust-speed" value="90.0" min-value="-360.0" max-value="360.0" tick-marks="3" flex="1"/>
         <label text="90.0 [deg/s]" name="speed label" width="100"/>
      </row>
      <row minimize="vertical" align="center">
        <bevel-button text="Start" action="start-action" width="55"/>
        <bevel-button text="Stop" action="stop-action" width="55"/>
      </row>
  </column>
</gear-window>

)


#| Explorations:

(view-named Wi "gears")

(frame-rate (view-named Wi "gears"))

(inspect (view-named Wi "gears"))

(setf (rotation-speed (view-named Wi "gears")) 10.0)

(start-animation (view-named Wi "gears"))

(stop-animation (view-named Wi "gears"))

(switch-to-full-screen-mode wi)
(switch-to-window-mode wi)


|#

