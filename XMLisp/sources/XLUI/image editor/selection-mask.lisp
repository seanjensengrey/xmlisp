;*********************************************************************
;*                                                                   *
;*            S E L E C T I O N   M A S K                            *
;*                                                                   *
;*********************************************************************
;* Author       : Ronald Sudomo (ronald@agentsheets.com)             *
;*                http://www.agentsheets.com                         *
;* Copyright    : (c) 2009, AgentSheets Inc.                         *
;* Last Update  : 08/21/09                                           *
;* Version      :                                                    *
;*    1.0       : 06/14/06 initial implementation                    *
;*    1.0.1     : 06/26/06 comments added                            *
;*    1.0.2     : 07/07/06 pixel-selected-p                          *
;*    1.0.3     : 07/19/06 select-at, select-pixel, deselect-pixel   *
;*    2.0       : 08/21/09 CCL, Alexander Repenning                  *
;* Abstract     : Selection mask used in the implementation of       *
;*                pixel selection in image editor.                   *
;*                                                                   *
;*********************************************************************

(in-package :xlui)


;**************************************
;* Selection-Mask                     *
;**************************************

(defclass SELECTION-MASK ()
  ((width :reader width :initarg :width :initform 16 :documentation "width of selection mask")
   (height :reader height :initarg :height :initform 16 :documentation "height of selection mask")
   (pixels :accessor pixels :initform nil :documentation "pixel map containing selection data"))
  (:documentation "Selection mask stores arbitrarily-shaped selection as a pixel map,
                   where selected pixels are 1's, unselected pixels are 0's,
                   and partially selected pixels are in between 0 and 1."))


;_______________________________
; Interface                     |
;_______________________________/

(defgeneric PIXEL-AT (Selection-Mask Col Row)
  (:documentation "Returns the mask value for the pixel at the specified indices."))


(defgeneric SET-PIXEL-AT (Selection-Mask Col Row Value)
  (:documentation "Sets the mask value for the pixel at the specified indices."))


(defgeneric SELECT-PIXEL (Selection-Mask Col Row)
  (:documentation "Select one pixel."))


(defgeneric DESELECT-PIXEL (Selection-Mask Col Row)
  (:documentation "Deselect one pixel."))


(defgeneric PIXEL-SELECTED-P (Selection-Mask Col Row)
  (:documentation "Returns t if the specified pixel is selected."))


(defgeneric ADD-SELECTION (Selection-Mask Shape &rest Shape-Specs)
  (:documentation "Adds a selection of the given shape to the selection mask."))


(defgeneric SUBTRACT-SELECTION (Selection-Mask Shape &rest Shape-Specs)
  (:documentation "Subtracts a selection of the given shape from the selection mask."))


(defgeneric INTERSECT-SELECTION (Selection-Mask Shape &rest Shape-Specs)
  (:documentation "Intersects a selection of the given shape with what is currently in the selection mask."))


(defgeneric INVERT-SELECTION (Selection-Mask)
  (:documentation "Inverts the current selection in the selection mask."))


(defgeneric SELECT-ALL (Selection-Mask)
  (:documentation "Marks all pixels in the selection mask selected."))


(defgeneric UNSELECT-ALL (Selection-Mask)
  (:documentation "Marks all pixels in the selection mask unselected."))


(defgeneric FILL-PIXELS-FOR-SHAPE (Pixels Shape Shape-Specs Value)
  (:documentation "Fills the mask pixels for the specified shape with the given value.
                   Specialized this generic function to implement selection for new shapes."))


;_______________________________
; Implementation                |
;_______________________________/

(defmethod INITIALIZE-INSTANCE ((Self selection-mask) &key)
  (call-next-method)
  (setf (pixels Self) (make-array (list (width Self) (height Self)) :initial-element 0)))


(defmethod PIXEL-WITHIN-BOUNDS-P (Col Row (Pixels array))
  "Returns t if the specified indices are valid for the given array, nil otherwise."
  (destructuring-bind (Width Height) (array-dimensions Pixels)
    (and (>= Col 0) (< Col Width) (>= Row 0) (< Row Height))))


(defmethod PIXEL-AT ((Self selection-mask) Col Row)
  "Returns the mask value for the pixel at the specified indices.
   If the pixel indices are outside bounds, returns zero."
  (if (pixel-within-bounds-p Col Row (pixels Self))
      (aref (pixels Self) Col Row)
    0))


(defmethod SET-PIXEL-AT ((Self selection-mask) Col Row Value)
  "Sets the massk value for the pixel at the specified indices."
  (when (pixel-within-bounds-p Col Row (pixels Self))
    (setf (aref (pixels Self) Col Row) Value)))


(defmethod SELECT-PIXEL ((Self selection-mask) Col Row)
  "Selects one pixel."
  (set-pixel-at Self Col Row 1))


(defmethod DESELECT-PIXEL ((Self selection-mask) Col Row)
  "Deselects one pixel."
  (set-pixel-at Self Col Row 0))


(defmethod PIXEL-SELECTED-P ((Self selection-mask) Col Row)
  "Returns t if the specified pixel is selected."
  (= 1 (pixel-at Self Col Row)))


(defmethod ADD-SELECTION ((Self selection-mask) Shape &rest Shape-Specs)
  "Adds a selection of the given shape to the selection mask."
  (fill-pixels-for-shape (pixels Self) Shape Shape-Specs 1))


(defmethod SUBTRACT-SELECTION ((Self selection-mask) Shape &rest Shape-Specs)
  "Subtracts a selection of the given shape from the selection mask."
  (fill-pixels-for-shape (pixels Self) Shape Shape-Specs 0))


(defmethod INTERSECT-SELECTION ((Self selection-mask) Shape &rest Shape-Specs)
  "Intersects a selection of the given shape with what is currently in the selection mask."
  (let ((Pixels (make-array (list (width Self) (height Self)) :initial-element 0)))
    (fill-pixels-for-shape Pixels Shape Shape-Specs 1)
    (dotimes (Row (height Self))
      (dotimes (Col (width Self))
        (setf (aref (pixels Self) Col Row)
          (logand (aref (pixels Self) Col Row) (aref Pixels Col Row)))))))


(defmethod INVERT-SELECTION ((Self selection-mask))
  "Inverts the current selection in the selection mask."
  (dotimes (Row (height Self))
    (dotimes (Col (width Self))
      (setf (aref (pixels Self) Col Row) (logxor (aref (pixels Self) Col Row) 1)))))


(defmethod SELECT-ALL ((Self selection-mask))
  "Marks all pixels in the selection mask selected."
  (dotimes (Row (height Self))
    (dotimes (Col (width Self))
      (setf (aref (pixels Self) Col Row) 1))))


(defmethod UNSELECT-ALL ((Self selection-mask))
  "Marks all pixels in the selection mask unselected."
  (dotimes (Row (height Self))
    (dotimes (Col (width Self))
      (setf (aref (pixels Self) Col Row) 0))))


(defmethod NUDGE-SELECTION-UP ((Self selection-mask))
  "Marks all pixels in the selection mask unselected."
  (dotimes (col (width self))
    (when (equal 1 (aref (pixels Self) Col 0))
      (return-from nudge-selection-up)))
  (dotimes (Row (height Self))
    (dotimes (Col (width Self))
      (unless (equal (+ 1 Row) (height self))
        (setf (aref (pixels Self) Col Row) (aref (pixels Self) Col (+ 1 Row))))
      (setf (aref (pixels Self) Col 31) 0))))


(defmethod NUDGE-SELECTION-DOWN ((Self selection-mask))
  "Marks all pixels in the selection mask unselected." 
  (dotimes (col (width self))
    (when (equal 1 (aref (pixels Self) Col 31))
      (return-from nudge-selection-down)))
  (let ((row (- (height Self) 0)))
    (dotimes (i (height Self))
      (dotimes (Col (width Self))
        (unless (or (equal Row 32) (equal (- Row 1) -1)
          (setf (aref (pixels Self) Col Row) (aref (pixels Self) Col (- Row 1)))))
        (setf (aref (pixels Self) Col 0) 0))
      (decf row))))


(defmethod NUDGE-SELECTION-LEFT ((Self selection-mask))
  "Marks all pixels in the selection mask unselected."
  (dotimes (row (width self))
    (when (equal 1 (aref (pixels Self) 0 row))
      (return-from nudge-selection-left)))
  (dotimes (Row (height Self))
    (dotimes (Col (width Self))
      (unless (equal (+ 1 col) (width self))
        (setf (aref (pixels Self) Col Row) (aref (pixels Self) (+ 1 Col) Row))
      ))
    (setf (aref (pixels Self) 31 Row) 0)))


(defmethod NUDGE-SELECTION-RIGHT ((Self selection-mask))
  "Marks all pixels in the selection mask unselected."
  (dotimes (row (width self))
    (when (equal 1 (aref (pixels Self) 31 row))
      (return-from nudge-selection-right)))
    (dotimes (Row (height Self))
      (let ((col (- (width Self) 1)))
        (dotimes (i (width Self))
          (unless (equal (- Col 1) -1)
            (setf (aref (pixels Self) Col Row) (aref (pixels Self) (- Col 1) row)))
          (decf col))
        (setf (aref (pixels Self) 0 Row) 0))))


(defun NORMALIZE-BOUNDS (Bounds)
  "Ensures that the given bounds are always specified as (Left Top Right Bottom)
   where Left < Right and Top < Bottom."
  (destructuring-bind (Left Top Right Bottom) Bounds
    (values (min Left Right) (min Top Bottom) (max Left Right) (max Top Bottom))))


(defmethod FILL-PIXELS-FOR-SHAPE (Pixels (Shape (eql :rect)) Bounds Value)
  "Fills pixels for a rectangular shape.
   A rectangular shape is specified by its bounding box."
  (multiple-value-bind (Left Top Right Bottom) (normalize-bounds Bounds)
    (dotimes (Row (- Bottom Top))
      (dotimes (Col (- Right Left))
        (when (pixel-within-bounds-p (+ Left Col) (+ Top Row) Pixels)
          (setf (aref Pixels (+ Left Col) (+ Top Row)) Value))))))


(defmethod FILL-PIXELS-FOR-SHAPE (Pixels (Shape (eql :ellipse)) Bounds Value)
  "Fills pixels for an elliptical shape.
   An elliptical shape is specified by its bounding box."
  (flet ((inside-ellipse-p (x y x0 y0 Rx Ry)
                           (<= (+ (/ (expt (- x x0) 2) (expt Rx 2))
                                  (/ (expt (- y0 y) 2) (expt Ry 2))) 1.0)))
    (multiple-value-bind (Left Top Right Bottom) (normalize-bounds Bounds)
      (let* ((Rx (/ (- Right Left) 2))
             (Ry (/ (- Bottom Top) 2))
             (x0 (+ Left Rx))
             (y0 (+ Top Ry)))
        (dotimes (Row (- Bottom Top))
          (dotimes (Col (- Right Left))
            (when (and (pixel-within-bounds-p (+ Left Col) (+ Top Row) Pixels)
                       (inside-ellipse-p (+ Left Col 0.5) (+ Top Row 0.5) x0 y0 Rx Ry))
              (setf (aref Pixels (+ Left Col) (+ Top Row)) Value))))))))


(defmethod FILL-PIXELS-FOR-SHAPE (Pixels (Shape (eql :polygon)) Vertices Value)
  "Fills pixels for an polygonal shape.
   A polygonal shape is specified by its vertices."
  (flet ((polygon-bounds (Vertices)
                         (let (Left Top Right Bottom)
                           (dolist (Vertex Vertices)
                             (destructuring-bind (x y) Vertex
                               (setq Left (min x (or Left x)))
                               (setq Right (max x (or Right x)))
                               (setq Top (min y (or Top y)))
                               (setq Bottom (max y (or Bottom y)))))
                           (values Left Top Right Bottom)))
         (inside-polygon-p (x y Vertices)
                           ;; Jordan Curve Theorem says that a point is inside a polygon if,
                           ;; for any ray from this point, there is an odd number of crossings
                           ;; of the ray with the polygon's edges.
                           ;; See http://astronomy.swin.edu.au/~pbourke/geometry/insidepoly/
                           (let ((n (length Vertices))
                                 (Inside nil))
                             (destructuring-bind (vx1 vy1) (first Vertices)
                               (dotimes (i n)
                                 (destructuring-bind (vx2 vy2) (nth (mod (1+ i) n) Vertices)
                                   (when (and (> y (min vy1 vy2))
                                              (<= y (max vy1 vy2))
                                              (<= x (max vx1 vx2))
                                              (not (= vy1 vy2)))
                                     (let ((xintersect (+ vx1 (* (- y vy1) (/ (- vx2 vx1) (- vy2 vy1))))))
                                       (when (or (= vx1 vx2) (<= x xintersect))
                                         (setq Inside (not Inside)))))
                                   (setq vx1 vx2)
                                   (setq vy1 vy2))))
                             Inside)))

    (multiple-value-bind (Left Top Right Bottom) (polygon-bounds Vertices)
      
      (dotimes (Row (- Bottom Top))
        
        (dotimes (Col (- Right Left))
          
          (when (and (pixel-within-bounds-p (+ Left Col) (+ Top Row) Pixels)
                     (inside-polygon-p (+ Left Col 0.5) (+ Top Row 0.5) Vertices))            
            (setf (aref Pixels (+ Left Col) (+ Top Row)) Value)))))))


(defmethod PRINT-OBJECT ((Self selection-mask) Stream)
  "Called by the Lisp printer to print this object.
   Specialized so it's easier to debug."
  (dotimes (Row (height Self))
    (dotimes (Col (width Self))
      (format Stream "~A" (aref (pixels Self) Col Row)))
    (format Stream "~%")))


#|

(defparameter sm (make-instance 'selection-mask :width 32 :height 32))


(select-all sm)

(unselect-all sm)

(invert-selection sm)


(add-selection sm :rect 10 10 20 20)

(subtract-selection sm :rect 10 10 20 20)

(intersect-selection sm :rect 10 10 20 20)


(add-selection sm :ellipse 15 5 25 15)

(subtract-selection sm :ellipse 15 5 25 15)

(intersect-selection sm :ellipse 15 5 25 15)


(add-selection sm :polygon '(5 20) '(15 10) '(25 20))

(subtract-selection sm :polygon '(5 20) '(15 10) '(25 20))

(intersect-selection sm :polygon '(5 20) '(15 10) '(25 20))

|#
