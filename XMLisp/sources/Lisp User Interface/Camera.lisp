;;-*- mode: lisp; package: lui -*-
;*********************************************************************
;*                                                                   *
;*                        C A M E R A                                *
;*                                                                   *
;*********************************************************************
;* Author       : Alexander Repenning, alexander@agentsheets.com     *
;*                http://www.agentsheets.com                         *
;* Copyright    : (c) 1996-2009, AgentSheets Inc.                    *
;* Filename     : camera.lisp                                        * 
;* Last Update  : 04/17/09                                           *
;* Version      :                                                    *
;*    1.0       : 08/27/01                                           *
;*    1.1       : 12/06/02 sustained animation                       *
;*    1.1.1     : 02/24/03 wrap track-mouse-3d into glFinish         *
;*    1.2       : 02/01/04 Camera can be animated                    *
;*    1.2.1     : 02/06/04 mouse spin using polar coordinates        *
;*    1.3       : 11/02/04 render-for-selection-mode                 *
;*    1.3.1     : 09/28/05 double format for ACL                     *
;*    1.3.2     : 10/27/05 recompute polar                           *
;*    1.3.3     : 11/04/05 double-float type camera slots            *
;*    1.4       : 02/22/06 same-settings                             *
;*    1.4.1     : 02/13/07 track-mouse-3d with view parameter        *
;*    1.4.2     : 08/13/07 :invalidate-view param to aim-camera      *
;*    1.4.3     : 02/13/08 render-for-selection-mode double float    * 
;*    1.4.4     : 05/05/08 make-me-the-current-context in tracking   * 
;*    2.0       : 04/10/09 CCL: no more polling!                     *
;* Systems      : G4, CCL 1.3, OS X 10.5.6                           *
;* Abstract     : OpenGL camera base class                           *
;*                                                                   *
;*********************************************************************

(in-package :LUI)


(export '(track-mouse-zoom track-mouse-pan track-mouse-spin track-mouse-3d compute-polar-orientation))


(defclass CAMERA ()
 ((view :accessor view :initform nil :initarg :view :documentation "view or window")
  (eye-x :accessor eye-x :initarg :eye-x :initform 0.0d0 :type double-float)
  (eye-y :accessor eye-y :initarg :eye-y :initform 0.0d0 :type double-float)
  (eye-z :accessor eye-z :initarg :eye-z :initform 2.0d0 :type double-float)
  (center-x :accessor center-x :initarg :center-x :initform 0.0d0 :type double-float)
  (center-y :accessor center-y :initarg :center-y :initform 0.0d0 :type double-float)
  (center-z :accessor center-z :initarg :center-z :initform 0.0d0 :type double-float)
  (up-x :accessor up-x :initarg :up-x :initform 0.0d0 :type double-float)
  (up-y :accessor up-y :initarg :up-y :initform 1.0d0 :type double-float)
  (up-z :accessor up-z :initarg :up-z :initform 0.0d0 :type double-float)
  (fovy :accessor fovy :initarg :fovy :initform 60.0d0 :type double-float :documentation "viewing angle")
  (aspect :accessor aspect :initarg :aspect :initform 1.0d0 :type double-float)
  (near :accessor near :initarg :near :initform 0.005d0 :type double-float)
  (far :accessor far :initarg :far :initform 2000.0d0 :type double-float)
  (azimuth :accessor azimuth :initarg :azimuth :initform 0.0d0 :type double-float)
  (zenith :accessor zenith :initarg :zenith :initform 0.0d0 :type double-float))
 (:documentation "OpenGL camera to control user perspective"))

;________________________________
; Specification                  |
;_______________________________/


(defgeneric AIM-CAMERA (Camera &key Eye-X Eye-Y Eye-Z Center-X Center-Y Center-Z 
                               Up-X Up-Y Up-Z
                               Fovy Aspect Near Far Animation-Time)
  (:documentation "Set the aim of the camera. Use Animation-Time to make a smoot transition"))


(defgeneric RENDER-FOR-SELECTION-MODE (Camera &optional X Y Width Height)
  (:documentation "Render camera in selection mode. Need to define a picking matrix with focus on <x,y>"))


(defgeneric TRACK-MOUSE-3D (Camera View dx dy &optional disable-spin)
  (:documentation "Track the mouse to control the aim of the camera"))


(defgeneric SAME-SETTINGS (Camera Camera2)
  (:documentation "true if both cameras have the same settings"))

;________________________________
; Implementation                 |
;_______________________________/

(defmethod ANIMATE ((Self camera) Time)
  (declare (ignore Time)))


(defmethod DRAW ((Self camera))
  ;; set the Transformation matrix
  (glmatrixmode gl_projection)
  (glloadidentity)
  ;; setup camera frustrum
  (gluperspective (fovy Self) (aspect Self) (near Self) (far Self))
  (glmatrixmode gl_modelview)
  (glloadidentity)
  (gluLookat 
   (eye-x Self) (eye-y Self) (eye-z Self)
   (center-x Self) (center-y Self) (center-z Self) 
   (up-x Self) (up-y Self) (up-z Self)))


(defmethod RENDER-FOR-SELECTION-MODE ((Self camera) &optional X Y Width Height)
  ;; same as Display except: - context setting,  + Selection.
  ;; camera itself cannot be selected but it needs to zoom in on selection area
  ;; set the Transformation matrix
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  ;; Selection
  (%stack-block ((&viewport 16))
    (glGetIntegerv GL_VIEWPORT &viewport)
    (gluPickMatrix (float x 0d0) (float (- (%get-long &Viewport 12) y) 0d0)  (float Width 0d0) (float Height 0d0) &Viewport))
  ;; setup camera frustrum
  (gluPerspective (fovy Self) (aspect Self) (near Self) (far Self))
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (gluLookAt
   (eye-x Self) (eye-y Self) (eye-z Self)
   (center-x Self) (center-y Self) (center-z Self) 
   (up-x Self) (up-y Self) (up-z Self)))


(defmethod COMPUTE-POLAR-ORIENTATION ((Self camera))
   (multiple-value-bind (Azimuth Zenith)
     (carthesian->polar (- (eye-x Self) (center-x Self))
                       (- (eye-y Self) (center-y Self))
                       (- (eye-z Self) (center-z Self)))
   (setf (azimuth Self) Azimuth)
   (setf (zenith Self) Zenith)))



(defmethod AIM-CAMERA ((Self camera) &key Eye-X Eye-Y Eye-Z
                      Center-X Center-Y Center-Z
                      Up-X Up-Y Up-Z
                      Fovy Aspect Near Far
                      Animation-Time)
  (declare (ignore Animation-Time))
  ;; for now ignore Animation Time => no smoot transition, just instant warp
  (setf (eye-x Self) (float (or Eye-X (eye-x Self)) 0d0))
  (setf (eye-y Self) (float (or Eye-Y (eye-y Self)) 0d0))
  (setf (eye-z Self) (float (or Eye-Z (eye-z Self)) 0d0))
  (setf (center-x Self) (float (or Center-X (center-x Self)) 0d0))
  (setf (center-y Self) (float (or Center-Y (center-y Self)) 0d0))
  (setf (center-z Self) (float (or Center-Z (center-z Self)) 0d0))
  (setf (up-x Self) (float (or Up-x (up-x Self)) 0d0))
  (setf (up-y Self) (float (or Up-y (up-y Self)) 0d0))
  (setf (up-z Self) (float (or Up-z (up-z Self)) 0d0))
  (setf (fovy Self) (float (or Fovy (fovy Self)) 0d0))
  (setf (aspect Self) (float (or Aspect (aspect Self)) 0d0))
  (setf (near Self) (float (or Near (near Self)) 0d0))
  (setf (far Self) (float (or Far (far Self)) 0d0)))


(defmethod SAME-SETTINGS ((Camera1 camera) (Camera2 camera))
  (dolist (slot '(eye-x eye-y eye-z center-x center-y center-z up-x up-y up-z fovy #|aspect|# near far) t)  ;; ignore aspect as that is set by views based on view size
    (unless (= (slot-value Camera1 Slot) (slot-value Camera2 Slot))
      (return nil))))

;________________________________
; Rotation Matrix Functions      |
;_______________________________/

(defun ROTATE-X (X Y Z Phy)
  (let ((sin (sin Phy)) (cos (cos Phy)))
    (values
     x
     (- (* y Cos) (* z Sin))
     (+ (* y Sin) (* z Cos)))))


(defun ROTATE-Y (X Y Z Phy)
  (let ((sin (sin Phy)) (cos (cos Phy)))
    (values
     (+ (* x Cos) (* z Sin))
     y
     (- (* z Cos) (* x Sin)))))


(defun ROTATE-Z (X Y Z Phy)
  (let ((sin (sin Phy)) (cos (cos Phy)))
    (values
     (- (* x Cos) (* y Sin))
     (+ (* x Sin) (* y Cos))
     z)))

;________________________________
; Carthesian Coordinates         |
;_______________________________/

(defun CARTHESIAN->POLAR (X Y Z) "
  in:  x, y, z: float.
  out: Azimuth, Zenith: float.
  Convert carthesian into polar coordinates."
  ;; oh the fun of dealing with special cases!
  ;; does not work if zenith > pi / 2
  (values
   (if (zerop z)
     (if (> x 0)
       #.(/ pi 2)
       #.(/ pi -2))
     (let ((result (atan (/ x z))))
       (if (< z 0) 
         (+ pi Result)
         Result)))
   (asin (/ y (sqrt (+ (expt x 2) (expt y 2) (expt z 2)))))))
           


(defun POLAR->CATHESIAN (Radius Azimuth Zenith) "
  in:  Radius, Azimuth, Zenith float.
  out: z, y, z: float.
  Convert polar to carthesian coordinates."  
  (values
    (* Radius (sin Azimuth) (cos Zenith))
    (* Radius (sin Zenith))
    (* Radius (cos Azimuth) (cos Zenith))))
   
;________________________________
; Camera Mouse Control           |
;_______________________________/

(defmethod TRACK-MOUSE-ZOOM ((Self camera) dx dy Gain)
  (declare (ignore dx))
  (let* ((vx (- (center-x Self) (eye-x Self)))
         (vy (- (center-y Self) (eye-y Self)))
         (vz (- (center-z Self) (eye-z Self)))
         (length (sqrt (+ (* vx vx) (* vy vy) (* vz vz))))
         (distance (* (- Dy) Gain)))
    (setq vx (/ vx Length))
    (setq vy (/ vy Length))
    (setq vz (/ vz Length))
    (aim-camera 
     Self
     :eye-x (+ (eye-x Self) (* vx Distance))
     :eye-y (+ (eye-y Self) (* vy Distance))
     :eye-z (+ (eye-z Self) (* vz Distance)))))


(defmethod TRACK-MOUSE-PAN ((Self camera) dx dy Gain)
  (let ((x (- (eye-x Self) (center-x Self)))
        (y (- (eye-y Self) (center-y Self)))
        (z (- (eye-z Self) (center-z Self)))
        (center-x (center-x Self))
        (center-y (center-y Self))
        (center-z (center-z Self)))
    (let* ((sina (/ x (sqrt (+ (* x x) (* z z)))))
           (cosa (/ z (sqrt (+ (* x x) (* z z)))))
           (sinz (/ y (sqrt (+ (* x x) (* y y) (* z z)))))
           (cosz (/ (sqrt (+ (* x x) (* z z))) (sqrt (+ (* x x) (* y y) (* z z))))))
      ;;; (format t "~%sina: ~A, cosa: ~A, sinz: ~A, cosz: ~A" sina cosa sinz cosz)
      ;; compute translation vector
      (let* ((mx (* (- dx) Gain))
             (my (* dy Gain))
             (dex (+ (* mx cosa) (- (* my sina sinz))))
             (dey (* my cosz))
             (dez (- (+ (* mx sina) (* my cosa sinz)))))
        (aim-camera 
         Self
         :eye-x (+ x dex center-x)
         :eye-y (+ y dey center-y)
         :eye-z (+ z dez center-z)
         :center-x (+ center-x dex)
         :center-y (+ center-y dey)
         :center-z (+ center-z dez))))))


(defmethod TRACK-MOUSE-SPIN ((Self camera) dx dy Gain)
  ;; Polar Coordinates: Zenith + Azimuth based
  (let* ((eye-x (eye-x Self)) (eye-y (eye-y Self)) (eye-z (eye-z Self))
         (center-x (center-x Self)) (center-y (center-y Self)) (center-z (center-z Self))
         (vx (- Eye-x Center-x))
         (vy (- Eye-y Center-y))
         (vz (- Eye-z Center-z))
         (r (sqrt (+ (expt vx 2) (expt vy 2) (expt vz 2)))))
    (incf (azimuth Self) (* (- dx) Gain))
    (incf (zenith Self) (* dy (* 0.4 Gain)))
    (multiple-value-bind (x2 y2 z2)  ;; the new position
                         (polar->cathesian r (azimuth Self) (zenith Self))
      (multiple-value-bind (x3 y3 z3) ;; end point of up vector
                           (polar->cathesian r (azimuth Self) (+ (zenith Self) 1.0))
        (aim-camera 
         Self 
         :eye-x (+ Center-x x2)
         :eye-y (+ Center-y y2)
         :eye-z  (+ Center-z z2)
         :up-x (- x3 x2)
         :up-y (- y3 y2)
         :up-z (- z3 z2))))))
    

(defmethod TRACK-MOUSE-3D ((Self camera) View dx dy &optional disable-spin)
  (declare (ignore View))
  ;; use modifier keys to select camera action
  (cond 
   ((alt-key-p) 
    (track-mouse-zoom Self dx dy (if (shift-key-p) 0.005 0.2)))
   ((command-key-p) 
    (track-mouse-pan Self dx dy (if (shift-key-p) 0.01 0.05)))
   (t 
    (unless disable-spin
      (track-mouse-spin Self dx dy (if (shift-key-p) 0.01 0.04))))))



#| Examples:

|#

