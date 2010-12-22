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


(defun GET-STRING-FROM-USER (msg  &key (Initial-String "")
                                  (Yes-Text "OK")
                                  (Cancel-Text "Cancel"))
  (let ((Alert (make-instance 'ns:ns-alert))
        (tf (#/initWithFrame: (lui::native-view (make-instance 'editable-text-control))
                              (ns:make-ns-rect 0 0 400 21))))
    (#/setStringValue: tf (ccl::%make-nsstring Initial-String))
    (#/setEditable: tf #$YES)
    (#/setMessageText: alert (ccl::%make-nsstring msg))
    (#/addButtonWithTitle: alert (native-string Yes-Text))
    (when Cancel-Text (#/addButtonWithTitle: alert (native-string Cancel-Text)))
    (#/setAccessoryView: alert tf)
    (when (mac-os-x-10.6-and-later)
      (#/layout alert))
    (#/makeFirstResponder: (#/window alert) tf) 
    (when (equal (#/runModal alert) #$NSAlertSecondButtonReturn)
      (return-from get-string-from-user nil))
    (ccl::lisp-string-from-nsstring (#/stringValue tf))))



#| Examples:

(get-string-from-user "Who is your favorite actor?" :initial-string "Betty Boo")

|#
