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


(defun GET-STRING-FROM-USER (Message &key Initial-String)
  (let ((Window (load-object "lui:resources;windows;get-string-from-user.window" :package (find-package :xlui))))
    (when Initial-String
      (setf (value (view-named Window "text")) Initial-String))
    (setf (title Window) Message)
    (show-and-run-modal Window)))



#| Examples:

(get-string-from-user "Who is your favorite actor?" :initial-string "Betty Boo")

|#
