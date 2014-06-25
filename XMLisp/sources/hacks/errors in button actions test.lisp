(in-package :ccl)


;; misc functions needed


(defun NATIVE-STRING (String) "
  Return a native string"
  (#/autorelease (ccl::%make-nsstring String)))


(defmethod STANDARD-ALERT-DIALOG ((Message string) &key 
                                  (Yes-Text "OK")
                                  (No-Text nil)
                                  (Cancel-Text nil)
                                  (Explanation-Text)
                                  (Is-Critical nil))
  (let ((Alert (#/init (#/alloc ns:ns-alert))))
    (#/setMessageText: Alert (native-string Message))
    (when Yes-Text (#/addButtonWithTitle: Alert (native-string Yes-Text)))
    (when No-Text (#/addButtonWithTitle: Alert (native-string No-Text)))
    (when Cancel-Text (#/addButtonWithTitle: Alert (native-string Cancel-Text)))
    (when Explanation-Text (#/setInformativeText: Alert (native-string Explanation-Text)))
    (#/setAlertStyle: Alert (if Is-Critical #$NSCriticalAlertStyle #$NSWarningAlertStyle))
    (case (#/runModal Alert)
      (#.#$NSAlertFirstButtonReturn t)
      (#.#$NSAlertSecondButtonReturn nil)
      (#.#$NSAlertThirdButtonReturn (throw :cancel nil)))))


(defun PRINT-CONDITION-UNDERSTANDABLY (Condition &optional (Message "") (Stream t))
  (format Stream "~%~A " Message)
  (ccl::report-condition Condition Stream))


;; error handler

(eval-when (:execute :load-toplevel :compile-toplevel)
(defmacro CATCH-ERRORS-NICELY (Situation &body Forms) "Catch errors at high level. Also works in gui threads and Cocoa call backs"
  `(catch :wicked-error
     (handler-bind
         ((warning #'(lambda (Condition) 
                       (print-condition-understandably Condition (format nil "~A warning, " ,Situation))
                       (muffle-warning)))
          (condition #'(lambda (Condition)
                         ;; no way to continue
                         (print-condition-understandably Condition (format nil "~A error, " ,Situation))
                         ;; produce a basic stack trace
                         (format t "~% ______________Exception in thread \"~A\"___(backtrace)___" (slot-value *Current-Process* 'ccl::name))
                         (standard-alert-dialog 
                          (with-output-to-string (Out)
                            (print-condition-understandably Condition "Error: " Out))
                          :is-critical t
                          :explanation-text (format nil "While ~A (in process: \"~A\")" ,Situation (slot-value *Current-Process* 'ccl::name)))
                         (throw :wicked-error Condition))))
       ,@Forms))))




;; Test

(defun MAKE-WINDOW-WITH-BUTTON ()
  (let ((window (make-instance ns:ns-window
                   :with-content-rect (ns:make-ns-rect 20 20 300 300)          
                   :backing #$NSBackingStoreBuffered
                   :defer t))
        (content-view (#/initWithFrame: (#/alloc ns:ns-view) (ns:make-ns-rect 0 0 300 300)))
        (button (#/initWithFrame: (#/alloc ns:ns-button) (ns:make-ns-rect 20 20 260 260))))
    (#/setAction: button (objc::@selector #/buttonAction))
    (#/setTarget: button 
                button)
    (#/setContentView: Window content-view)
    (#/addSubview: content-view button)
    (#/orderFront: Window nil)))


(objc:defmethod (#/buttonAction :void) ((self ns:ns-button))
  (catch-errors-nicely 
   "button click"
   (/ (sin 3.0) 0.0))   ;; division by zero
  (print "This is my action!!"))

(make-window-with-button)

(defparameter *Button* (make-instance 'ns::ns-button))
