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
   ;(value-array :accessor value-array :initform (make-array 5 :fill-pointer 0 :adjustable t))
   (plot-color :accessor plot-color :initform (red-color-vector))
   (min-data-points-to-end-of-screen :accessor min-data-points-to-end-of-screen :initform 10)
   (plot-lists :accessor plot-lists :initform () :documentation "This list of lists.  Each sublist contains first, the name of the representation, second, the color and third the vector of values and the fourth is ther starting time of the plot element")
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
  (let ((plot-list (find representing (plot-lists self) :key #'xlui::representing :test 'equal)))
    (unless plot-list   
      #+cocotron(display (xlui::view-named (window self) "export-button"))
      #+cocotron(display (xlui::view-named (window self) "reset-button"))
      (setf (plot-lists self) (append (plot-lists self) (list (make-instance 'xlui::plot-list :representing representing :color color :plot-vector  (make-array 5 :fill-pointer 0 :adjustable t) :starting-time (max-time self)));(list (list  representing  color  (make-array 5 :fill-pointer 0 :adjustable t) (max-time self)))
                                     )))
      (when plot-list
        (when (xlui::set-this-cycle-p plot-list)
          (xlui::new-cycle (window self))
          (error "Can't print to the same \"Representing\" in the same plot window twice in one simulation cycle. Please look at all of the \"plot to window\" actions in the selected agent before running this simulation again.")
         
          )
        (setf (xlui::set-this-cycle-p plot-list) t)
        (unless (equal color (color plot-list))
          (setf (color plot-list) color))
        (vector-push-extend  value (xlui::plot-vector plot-list))
        (when (> (length (xlui::plot-vector plot-list)) (max-time self))
          (setf (max-time self) (length (xlui::plot-vector plot-list))))))  
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
  (dolist (plot-sublist (plot-lists self))
    (glcolor4f (first (color plot-sublist)) (second (color plot-sublist)) (third (color plot-sublist))  (fourth (color plot-sublist)))
    (glBegin GL_LINE_STRIP)
    (let ((increment-value 1))
      (setf increment-value 1)
      (dotimes (i (round (length (xlui::plot-vector plot-sublist)) increment-value))
        (glVertex2f (+ (xlui::starting-time plot-sublist) (* i  1.0)) (* 1.0 (aref (xlui::plot-vector plot-sublist) (* increment-value i)))))      
      (glEnd))))

(defmethod RESET ((self plot-view))
  (setf (lui::min-value self) 0.0d0)
  (setf (lui::max-value self) 0.0d0)
  (setf (lui::max-time self) 0.0)
  (setf (lui::plot-lists self) nil))




#| Examples:

(defparameter *Harry-Plotter* (make-instance 'plot-view :x 0 :y 0 :width 500 :height 300))


(add-subviews
  (make-instance 'window :width 500 :height 300 :title "plot")
  *Harry-Plotter*)

(dotimes (i 100)
  (add-plot-value *Harry-Plotter* (random 2.0) "Kings"))


|#

