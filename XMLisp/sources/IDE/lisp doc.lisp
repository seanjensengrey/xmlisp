

(in-package :ccl)


(defparameter *Classes-Documented* (make-hash-table))


(defun RENDER-CLASS-DOC (Classes &optional (Level 0))
  (when (not (listp Classes)) 
    (render-class-doc (list Classes) Level)
    (return-from render-class-doc))
  (when (= Level 0) (setf *Classes-Documented* (make-hash-table)))
  (dolist (Class Classes)
    (when (symbolp Class) (setf Class (find-class Class)))
    (unless (gethash (slot-value Class 'name) *Classes-Documented*)
      (setf (gethash (slot-value Class 'name) *Classes-Documented*) Class)
      (dotimes (I (* 2 Level)) (princ #\space))
      (princ (slot-value Class 'name))
      (let ((Documentation (documentation (slot-value Class 'name) 'type)))
        (when Documentation
          (format t ": ~A" Documentation)))
      (terpri)
      (let ((Slot-Names (mapcar #'slot-definition-name (class-direct-slots Class))))
        (when Slot-Names
          (dotimes (I (* 2 (+ Level 2))) (princ #\space))
          (format t "slots: ")
          (dolist (Slot-Name (butlast Slot-Names))
            (format t "~:(~A~), " Slot-Name))
          (format t "~:(~A~)" (first (last Slot-Names)))
          (terpri)))
      (render-class-doc (slot-value Class 'direct-subclasses) (1+ Level)))))



#| Examples:


(render-class-doc 'number)

(render-class-doc '(xlui::agent-3d))


(render-class-doc 't)



|#