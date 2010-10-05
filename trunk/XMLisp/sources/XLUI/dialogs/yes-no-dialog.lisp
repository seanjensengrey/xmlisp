;;; 10/29/09 Alexander Repenning

(in-package :xlui)


(defclass YES-NO-DIALOG-WINDOW (dialog-window)
  ()
  (:documentation "Used to get a string from user"))


(defmethod SHOW-AND-RUN-MODAL ((Self yes-no-dialog-window))
  (declare (special *Run-Modal-Return-Value*))
  (setq *Run-Modal-Return-Value* nil)
  (when (#/isVisible (lui::native-window Self))
    (error "cannot run modal a window that is already visible"))
  (lui::in-main-thread () (#/makeKeyAndOrderFront: (lui::native-window self) (lui::native-window self)))
  (let ((Code (lui::in-main-thread () 
                              (#/runModalForWindow: (#/sharedApplication ns:ns-application)
                                      (lui::native-window Self)))))
    (declare (ignore Code))
    ;; ignore Code for now
    (lui::in-main-thread () (#/close (lui::native-window Self)))
    (print *Run-Modal-Return-Value*)
    lui::*Run-Modal-Return-Value*))


(defmethod YES-ACTION ((Window yes-no-dialog-window) (Button button))
  (stop-modal Window :yes))

(defmethod NO-ACTION ((Window yes-no-dialog-window) (Button button))
  (stop-modal Window :no))

(defmethod CANCEL-ACTION ((Window yes-no-dialog-window) (Button button))
  (cancel-modal Window))

(defmethod SHOW ((Self yes-no-dialog-window))
  ;; do not actually show
  )


(defmethod READ-RETURN-VALUE ((Self yes-no-dialog-window))
  Self)


;; calling this function over and over appears to corrupt memory
(defun SHOW-YES-NO-DIALOG (Title &key explanation-string
                                     (Yes-Text "Yes")
                                     (No-Text "No")
                                     (Cancel-Text "Cancel"))
  ;Do we need these?
  (declare (ignore yes-text no-text cancel-text))
 
  (let ((Window (load-object "lui:resources;windows;yes-no-dialog.window" :package (find-package :xlui))))
    ; (when Yes-Text (#/addButtonWithTitle: (lui::native-window Window) (native-string Yes-Text)))
    ;(when No-Text (#/addButtonWithTitle: (lui::native-window Window) (native-string No-Text)))
    ;(when Cancel-Text (#/addButtonWithTitle: (lui::native-window Window) (native-string Cancel-Text)))
    (#/setString: (lui::native-view (view-named Window "text")) (native-string explanation-string))
    #|
    (when Initial-String
      (setf (value (view-named Window "text")) Initial-String))
    |#
    (ns:with-ns-point (Point (NS:NS-POINT-X (#/mouseLocation ns:ns-event)) (NS:NS-POINT-Y (#/mouseLocation ns:ns-event)))
      (#/setFrameOrigin: (lui::native-window window ) Point ))
    (setf (title Window) Title)
    (show-and-run-modal Window)))


#| Examples:

(show-yes-no-dialog "Should we do this?" :explanation-string "Place your explanation here:")

|#