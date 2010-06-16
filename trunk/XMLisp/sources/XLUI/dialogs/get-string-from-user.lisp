;;; Get-String-From-User-window
;;; 10/29/09 Alexander Repenning

(in-package :xlui)


(defclass GET-STRING-FROM-USER-WINDOW (dialog-window)
  ()
  (:documentation "Used to get a string from user"))


(defmethod OK-ACTION ((Window get-string-from-user-window) (Button button))
  (stop-modal Window (value (view-named Window "text"))))


(defmethod CANCEL-ACTION ((Window get-string-from-user-window) (Button button))
  (cancel-modal Window))


(defmethod SHOW ((Self get-string-from-user-window))
  ;; do not actually show
  )


(defmethod READ-RETURN-VALUE ((Self get-string-from-user-window))
  Self)


;; calling this function over and over appears to corrupt memory
(defun GET-STRING-FROM-USER (Message &key Initial-String)
  (let ((Window (load-object "lui:resources;windows;get-string-from-user.window" :package (find-package :xlui))))
    (when Initial-String
      (setf (value (view-named Window "text")) Initial-String))
    (ns:with-ns-point (Point (NS:NS-POINT-X (#/mouseLocation ns:ns-event)) (NS:NS-POINT-Y (#/mouseLocation ns:ns-event)))
      (#/setFrameOrigin: (lui::native-window window ) Point ))
    (setf (title Window) Message)
    (show-and-run-modal Window)))


;; HACK: alternative implementation based on alerts
(defun GET-STRING-FROM-USER (msg  &key (Initial-String ""))
  (let ((alert (make-instance 'ns:ns-alert))
        (tf (#/initWithFrame: (make-instance 'ns:ns-text-field)
                              (ns:make-ns-rect 0 0 400 21))))
    (#/setStringValue: tf (ccl::%make-nsstring Initial-String))
    (#/setEditable: tf #$YES)
    (#/setMessageText: alert (ccl::%make-nsstring msg))
    (#/addButtonWithTitle: alert #@"OK")
    (#/setAccessoryView: alert tf)
    (#/layout alert)
    (#/makeFirstResponder: (#/window alert) tf) 
    (#/runModal alert) 
    (ccl::lisp-string-from-nsstring (#/stringValue tf))))



#| Examples:

(get-string-from-user "Who is your favorite actor?" :initial-string "Betty Boo")

|#
