;;; Mac OpenGL for MCL example: Frame Rate Benchmark
;;; explore differences between OS 9 vs OS X
;;; Alexander Repenning 1/5/2
;;; Updated: 1/5/2
;;; 10/06/05 tested with OpenGL for MCL 1.6
;;; 03/25/09 Clozure CL

;; Windows Issues:
;; This example has framerates of a factor of ten worse then on the Mac, I get these framerates 
;; (around 155) on Windows running paralells, this virtual machine may have something to do with this.  
(in-package :xlui)


(defclass BENCHMARK (opengl-dialog)
  ((data-points :accessor data-points 
                :initform '((0.4 0.2 0.0)
                            (0.3 0.4 0.0)
                            (0.7 0.7 0.1)
                            (0.2 0.7 0.0)
                            (0.2 0.4 0.0)
                            (0.8 0.0 0.7)
                            (0.7 0.2 0.2)
                            (0.2 0.1 0.0)
                            (0.4 0.25 0.0)
                            (0.8 0.8 0.8)))))


(defmethod PREPARE-OPENGL ((Self benchmark))
  ;; GL setup
  (glClearColor 0.0 0.0 0.0 1.0)
  (glcolor3f 1.0 1.0 1.0)
  (glShadeModel gl_smooth)
  ;; define material
  (glmaterialfv gl_front_and_back gl_specular {1.0 1.0 1.0 0.0})
  (glmaterialf gl_front_and_back gl_shininess 128.0)
  (glmaterialfv gl_front_and_back gl_ambient_and_diffuse {1.0 0.0 0.0 1.0})
  ;; light
  (gllightfv gl_light0 gl_position {3.0 3.0 3.0 1.0})
  (gllightfv gl_light0 gl_diffuse {1.0 1.0 1.0 1.0})
  (gllightfv gl_light0 gl_specular {1.0 1.0 1.0 1.0})
  ;; enablers
  (glenable gl_lighting)
  (glenable gl_light0)
  (glenable gl_depth_test)
  (glenable gl_line_smooth)
  ;; camera
  ;;; (aim-camera (camera Self) :eye-x 1.0 :eye-y 1.0 :eye-z 1.0)
  )



(defmethod DRAW ((Self benchmark)) 
  (glmaterialfv gl_front_and_back gl_ambient_and_diffuse {1.0 1.0 1.0 1.0})
  ;; axes
  (glBegin gl_lines)
     (glVertex3f 0.0 0.0 0.0)
     (glVertex3f 1.0 0.0 0.0)
     (glVertex3f 0.0 0.0 0.0)
     (glVertex3f 0.0 1.0 0.0)
     (glVertex3f 0.0 0.0 0.0)
     (glVertex3f 0.0 0.0 1.0)
  (glEnd)
  ;; data points
  (dolist (Point (data-points Self))
    (plot-sphere Self (first Point) (second Point) (third Point))))


(defmethod PLOT-SPHERE ((Self benchmark) X Y Z)
  (declare (optimize))
  (let ((Ball (gluNewQuadric)))
    ;; establish position
    (glPushmatrix)
    (glTranslatef x y z)
    ;; sphere
    (glmaterialfv gl_front_and_back gl_ambient_and_diffuse {1.0 0.0 0.0 1.0})
    (gluQuadricDrawstyle Ball glu_fill)
    (gluQuadricOrientation Ball glu_outside)
    (gluQuadricNormals Ball glu_smooth)
    (gluSphere Ball 0.05d0 20 20)
    (glPopmatrix)
    (gluDeleteQuadric Ball)))

        

(defparameter Wi 
  <application-window title="OpenGL 3D" margin="0">
    <benchmark full-scene-anti-aliasing="false"/>
  </application-window>)


#| Examples:


(frame-rate (first (subviews wi)))

(switch-to-full-screen-mode wi)
(switch-to-window-mode wi)



Rates: 
- PowerBook G3, 500 Mhz, ATI Rage Mobility:                     86 fps
- PowerBook G4, 800 Mhz, , OS X classic, Radeon 7500 mobility: 177 fps
- PowerBook G4, 800 Mhz, , OS 9, Radeon 7500 mobility:         440 fps
- PowerBook G4, 800 Mhz, , OS X carbon, Radeon 7500 mobility:  393 fps
- PowerBook G4, 800 Mhz, 10.3.7, Radeon 7500 mobility:         430 fps
- G4, 400 Mhz, ATI Radeon AGP, OS 9,                           270 fps
- G4, 400 Mhz, ATI Radeon AGP, OS X carbon,                    250 fps
- G5, 1.8 Mhz dual, ATI 9600?, OS X 10.3.2:                    740 fps
- PowerBook G4, 1.67 Ghz, Mob Radeon 9700, OS X 10.4.7:        693 fps

CCL 
- MacBookPro, 2.6 Ghz Core 2 Duo, OS X 1.5.6, CCL 1.3 32bit, GForce 8600 GT     2200 fps
- MacBookPro, 2.6 Ghz Core 2 Duo, OS X 1.5.6, CCL 1.3 64bit, GForce 8600 GT     2200 fps
- MacBookPro, 2.6 Ghz Core 2 Duo, OS X 1.6.1, CCL 1.4 64bit, GForce 8600 GT     2900 fps
- MacBook Pro, 2.66 Ghz, i7, OS X 10.6.5, CCL 1.6 65 bit, GeForce 330M          4200 fps

|#
