(in-package :xlui)

;(export '(ok-action cancel-action show))

(defclass GET-NEW-AGENT-INFO-FROM-USER-WINDOW (dialog-window)
  ()
  (:documentation "Used to get a string from user"))


(defmethod OK-ACTION ((Window get-new-agent-info-from-user-window) (Button button))
  (inspect (value (list (value (view-named Window "image-selection"))(value (view-named Window "text")))))
  (print "FROM OK")
  (stop-modal Window (value (list (value (view-named Window "image-selection"))(value (view-named Window "text"))))))


(defmethod CANCEL-ACTION ((Window  get-new-agent-info-from-user-window) (Button button))
  (cancel-modal Window))


(defmethod SHOW ((Self get-new-agent-info-from-user-window))
  ;; do not actually show
  )

(defmethod READ-RETURN-VALUE ((Self get-new-agent-info-from-user-window))
  (print "FROM READ RETURN")
  Self)


(defun GET-NEW-AGENT-INFO-FROM-USER (Message &key Initial-String)
  (let ((Window (load-object "lui:resources;windows;agent-input.window" :package (find-package :xlui))))
    (when Initial-String
      (setf (value (view-named Window "text")) Initial-String))
    (setf (title Window) Message)
    (show-and-run-modal Window)))
