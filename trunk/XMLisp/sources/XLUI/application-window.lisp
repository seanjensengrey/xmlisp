;;; Application-Window
;;; 11/13/08 Alexander Repenning
;;; 02/17/10 AI: fixed spelling of do-show-immediately (used to be do-show-immediatly)

(in-package :XLUI)

(defvar *Layout-Border* 12 "used for margins")


(defclass APPLICATION-WINDOW (window xml-serializer)
  ((named-views :accessor named-views :initarg :named-views :initform (make-hash-table :test #'equal) :documentation "hashtable of all the named views contained in window")
   (margin :accessor margin :initarg :margin :initform *Layout-Border* :type integer :documentation "top, right, left, bottom"))
  (:documentation "Main windows of applications that are not document-based.")
  (:default-initargs 
      :do-show-immediately nil))


(defmethod REGISTER-NAMED-VIEWS ((Self application-window))
  (recursive-map-subviews 
   Self 
   #'(lambda (View)
       (when (and (not (eq View Self)) (not (string= (name View) "")))
         (setf (gethash (string-upcase (name View)) (named-views Self)) View)))))


(defmethod VIEW-NAMED ((Self application-window) (Name string) )
  (gethash (string-upcase Name) (named-views Self)))


(defmethod VIEW-NAMED ((Self application-window) (Name symbol))
  (gethash (symbol-name Name) (named-views Self)))


(defmethod INITIALIZE-INSTANCE :after ((Self application-window) &rest Args)
  (declare (ignore Args))
  (do-subviews (Subview Self)
               (set-size Subview (- (width Self) (margin Self) (margin Self)) (- (height Self) (margin Self) (margin Self)))
    (set-position Subview (margin Self) (margin Self)))
  (register-named-views Self)
  (show Self))


(defmethod PRINT-SLOTS ((Self application-window))
  '(x y width height zoomable minimizable resizable closeable subviews))


(defmethod ADD-SUBOBJECT ((Window application-window) (View xml-layout-interface))
  (add-subviews Window View))


(defmethod SIZE-CHANGED-EVENT-HANDLER ((Self application-window) Width Height)
  (do-subviews (Subview Self)
    (set-size Subview (- Width (margin Self) (margin Self)) (- Height (margin Self) (margin Self)))
    (set-position Subview (margin Self) (margin Self))))


(defmethod FORCE-REDRAW ((Self Application-Window))
  (set-size Self (width Self) (height Self))
  (display Self))


;***************************************
;* CODECs                              *
;***************************************

(deftype layout-value () "attribute value used in layouts that could be symbol or number" t)


(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'layout-value)) Stream)
  (format Stream "\"~A\"" (string-upcase (write-to-string Value))))


(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'layout-value)))
  (read-from-string Value))


