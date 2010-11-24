(in-package :lui)


;**********************************
;* PLOT-VIEW                      *
;**********************************

(defvar *Red-Color-Vector* nil "pointer to 4 bytes holding 32 bit RGBA vector")

(defun RED-COLOR-VECTOR ()
  (or *Red-Color-Vector*
      (setq *Red-Color-Vector* (make-byte-vector 255 0 255 255))))


(defclass PLOT-VIEW (opengl-view)
  ((native-color :accessor native-color :initform nil)
   (text :accessor text :initform "mil")
   (min-value :accessor min-value :initform 0.0d0 :initarg :min-value)
   (max-value :accessor max-value :initform 0.0d0 :initarg :max-value)
   (min-time :accessor min-time :initform 0)
   (max-time :accessor max-time :initform 0.0)
   (value-array :accessor value-array :initform (make-array 5 :fill-pointer 0 :adjustable t))
   (plot-color :accessor plot-color :initform (red-color-vector))
   (min-data-points-to-end-of-screen :accessor min-data-points-to-end-of-screen :initform 10)
   (plot-list :accessor plot-list :initform () :documentation "This list of lists.  Each sublist contains first, the name of the representation, second, the color and third the vector of values and the fourth is ther starting time of the plot element")
   (y-padding-factor :accessor y-padding-factor :initform 1.1))
  (:documentation "A view for plotting values vs time")
  (:default-initargs 
    :x 100
    :y 100))


(defmethod VIEW-LEFT-MOUSE-DRAGGED-EVENT-HANDLER ((Self plot-view) X Y DX DY)
  (declare (ignore dx dy))
  (declare (ignore X Y))
  (unless (is-animated Self) (display Self)))


(defmethod PREPARE-OPENGL :after ((Self plot-view))
  (glClearColor 1.0 0.5 0.5 0.0)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (gluOrtho2D 0.0d0 (* 1.0d0 (Width self)) 0.0d0 (* 1.0d0 (height self)))
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity))


(defmethod ADD-PLOT-VALUE ((Self plot-view) value representing &key (color nil))
  (when (> value (max-value self))
    (setf (max-value self) value))
  (when (< value (min-value self))
    (setf (min-value self) value))
  (let ((plot-list (find representing (plot-list self) :key #'first :test 'equal)))
    (unless plot-list   
      (setf (plot-list self) (append (plot-list self) (list (list  representing  color  (make-array 5 :fill-pointer 0 :adjustable t) (max-time self))))))
    (when plot-list
      (unless (equal color (second plot-list))
        (setf (second plot-list) color))
      (vector-push-extend  value (third plot-list))
      (when (> (length (third plot-list)) (max-time self))
        (setf (max-time self) (length (third plot-list))))))  
  (display self))


(defmethod DRAW ((Self plot-view))
  ;; should not be necessary
  ;; (glClearColor 0.0 0.0 0.0 1.0)
  ;; (glClear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glMatrixMode GL_PROJECTION)
  (glEnable GL_LINE_SMOOTH)
  (glLoadIdentity)
  (gluOrtho2D
   0.0d0
   (* 1.0d0 (max-time self))
   (+
    (if (equal (max-value self) 0.0)
      0.0
      (* -.1 (- (max-value self) (min-value self))))
    (* 1.0d0 
       (y-padding-factor self)
       (if (< (min-value self) 0.0) 
         (min-value self)
         0.0)))
   (* 1.0d0 (y-padding-factor self) (max-value self)))
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (glColor3f 1.0 1.0 1.0)
  (glBegin GL_LINES)
  (glVertex3f 0.0 0.0 0.0)
  (glVertex3f (* 1.0 (if (> (max-time self) (width self)) (max-time self) (width self))) 0.0 0.0)
  (glEnd)
  (dolist (plot-sublist (plot-list self))
    (glcolor4f (first (second plot-sublist)) (second (second plot-sublist)) (third (second plot-sublist))  (fourth (second plot-sublist)))
    (glBegin GL_LINE_STRIP)
    (let ((increment-value 1))
      (setf increment-value 1)
      (dotimes (i (round (length (third plot-sublist)) increment-value))
        (glVertex3f (+ (fourth plot-sublist)(* i  1.0)) (aref (third plot-sublist) (* increment-value i)) 0.0))      
      (glEnd))))


#| Examples:

(defparameter *Harry-Plotter* (make-instance 'plot-view :x 0 :y 0 :width 500 :height 300))


(add-subviews
  (make-instance 'window :width 500 :height 300 :title "plot")
  *Harry-Plotter*)

(dotimes (i 100)
  (add-plot-value *Harry-Plotter* (random 2.0) "Kings"))


|#

