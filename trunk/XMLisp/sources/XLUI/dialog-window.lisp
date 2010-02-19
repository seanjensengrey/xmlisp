;;; Application-Window
;;; 11/13/08 Alexander Repenning
;;; 02/17/10 AI: fixed spelling of do-show-immediately (used to be do-show-immediatly)

(in-package :XLUI)



(defclass DIALOG-WINDOW (application-window xml-serializer)
  ((value :accessor value :initform nil :documentation "value returned"))
  (:default-initargs 
    :do-show-immediately nil
    :resizable nil
    :minimizable nil
    :closeable nil
    :zoomable nil
    :title "")
  (:documentation "A modal dialog/alert window that has a value that is 
returned when the modal interaction is finished, e.g., when user hits an OK button."))


(defmethod SHOW ((Self dialog-window))
  ;; overwrite regular show: need to run this window modal and return values
  (show-and-run-modal Self))


(defmethod READ-RETURN-VALUE ((Self dialog-window))
  (value Self))


(defmethod STOP-MODAL :before ((Self dialog-window) Return-Value)
  (setf (value Self) Return-Value))

