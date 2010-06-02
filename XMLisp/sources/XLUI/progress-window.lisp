(in-package :xlui)

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


(defmethod UPDATE-STATUS-BAR ((self progress-window) text)
  (setf (value (view-named self "status-bar")) (format nil "Loading: ~A" text)))

 
(defmethod START-ANIMATION ((self progress-window))
  (start-animation (view-named self "progress")))


(defmethod STOP-ANIMATION ((self progress-window))
  (stop-animation (view-named self "progress")))


(defmethod HIDE-PROGRESS-WINDOW ((Self progress-window) Return-Value)
  (declare (ignore return-value))
  (hide self))

#|
(defparameter *window* (load-object "lui:resources;windows;progress.window" :package (find-package :xlui)))
(inspect (load-object "lui:resources;windows;progress.window" :package (find-package :xlui)))
(inspect *window*)

|#