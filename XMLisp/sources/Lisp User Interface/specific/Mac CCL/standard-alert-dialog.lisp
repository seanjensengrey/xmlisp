;; Standard Alert Dialog
;; 12/11/08 Alexander Repenning

(in-package :lui)


(defmethod STANDARD-ALERT-DIALOG ((Message string) &key 
                                  (Yes-Text "Yes")
                                  (No-Text "No")
                                  (Cancel-Text "Cancel")
                                  (Explanation-Text)
                                  (Is-Critical nil))
  (let ((Alert (#/init (#/alloc ns:ns-alert))))
    (#/setMessageText: Alert Message)
    (#/addButtonWithTitle: Alert Yes-Text)
    (#/addButtonWithTitle: Alert No-Text)
    (#/addButtonWithTitle: Alert Cancel-Text)
    (when Explanation-Text (#/setInformativeText: Alert Explanation-Text))
    (#/setAlertStyle: Alert (if Is-Critical #$NSCriticalAlertStyle #$NSWarningAlertStyle))
    (case (#/runModal Alert)
      (#.#$NSAlertFirstButtonReturn t)
      (#.#$NSAlertSecondButtonReturn nil)
      (#.#$NSAlertThirdButtonReturn (throw :cancel nil)))))


#| Why does this require NSstring parameters and does not automatically convert Lisp strings or deal with nil??

(defun STANDARD-ALERT-DIALOG2 (Message &key 
                                  (yes-text #@"Yes")
                                  (no-text #@"No")
                                  (cancel-text #@"Cancel")
                                  (explanation-text #@"because..."))
  (#/runModal
   (#/alertWithMessageText:defaultButton:alternateButton:otherButton:informativeTextWithFormat:
    ns:ns-alert 
    Message
    yes-text
    no-text
    cancel-text
    explanation-text)))

(standard-alert-dialog2 #@"this works")
(standard-alert-dialog2 "but not this")

|#





#| Examples: 

(standard-alert-dialog "Too good to be true?")
(standard-alert-dialog "Too good to be true?" :is-critical t)

(standard-alert-dialog "No agent or shape selected in project window" 
                       :explanation-text "Select at least one agent in world"
                             :yes-text "OK" :no-text nil :cancel-text nil)

|#