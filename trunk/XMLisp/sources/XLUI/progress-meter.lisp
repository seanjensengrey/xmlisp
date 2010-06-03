(in-package :xlui)


(defclass PROGRESS-METER ()
  ((progress-window :accessor progress-window :initarg :progress-window :initform (load-object "lui:resources;windows;progress.window" :package (find-package :xlui))))
  (:documentation " "))


(defclass PROGRESS-WINDOW (application-window xml-serializer)
  ((value :accessor value :initform nil :documentation "value returned"))
  (:default-initargs 
    :do-show-immediately nil
    :resizable nil
    :minimizable nil
    :closeable nil
    :zoomable nil
    :title "")
  (:documentation " "))

#|
(defmethod INITIALIZE-INSTANCE :after ((Self progress-meter) &rest Args)
  (declare (ignore Args))
  (call-next-method)
  )

|#

(defmethod UPDATE-STATUS-BAR ((self progress-meter) text)
  (setf (value (view-named (progress-window self) "status-bar")) (format nil "Loading: ~A" text)))

 
(defmethod START-ANIMATION ((self progress-meter))
  (start-animation (view-named (progress-window self) "progress")))


(defmethod STOP-ANIMATION ((self progress-meter))
  (stop-animation (view-named (progress-window self) "progress")))


(defmethod HIDE-PROGRESS-WINDOW ((Self progress-meter) Return-Value)
  (declare (ignore return-value))
  (hide (progress-window self)))


(defmethod SHOW ((Self progress-window))
  (lui::in-main-thread ()        
    (#/orderFront: (lui::native-window self) nil)
    (#/makeKeyWindow (lui::native-window self))))


#|
(defparameter *window* (load-object "lui:resources;windows;progress.window" :package (find-package :xlui)))
(inspect (load-object "lui:resources;windows;progress.window" :package (find-package :xlui)))
(inspect *window*)

|#