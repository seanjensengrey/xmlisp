;;-*- Mode: Lisp; Package: :LUI -*- 
;*********************************************************************
;*                                                                   *
;*           E R R O R    H A N D L I N G                            *
;*                                                                   *
;*********************************************************************
;* Author       : Alexander Repenning, alexander@agentsheets.com     *
;*                http://www.agentsheets.com                         *
;* Copyright    : (c) 1996-2010, AgentSheets Inc.                    *
;* Filename     : error-handling.lisp                                *
;* Last Update  : 06/24/10                                           *
;* Version      :                                                    *
;*    1.0       : 06/24/10                                           *
;* Systems      : OS X 10.6.4                                        *
;* Lisps        : CLozure CL 1.4                                     *
;* Licence      : LGPL                                               *
;* Abstract     : Catch errors and warnings in user friendly way.    *
;*********************************************************************


(in-package :lui)

(export '(catch-errors-nicely))


(defun PRINT-CONDITION-UNDERSTANDABLY (Condition &optional (Message "") (Stream t))
  (format Stream "~%~A " Message)
  (ccl::report-condition Condition Stream))


(eval-when (:execute :load-toplevel :compile-toplevel)
(defmacro CATCH-ERRORS-NICELY ((Situation &key Before-Message After-Message) &body Forms) "Catch errors at high level. Also works in gui threads and Cocoa call backs"
  `(catch :wicked-error
     (handler-bind
         ((warning #'(lambda (Condition) 
                       (print-condition-understandably Condition (format nil "~A warning, " ,Situation))
                       (muffle-warning)))
          (condition #'(lambda (Condition)
                         (let ((*XMLisp-Print-Synoptic* t))
                           ;; no way to continue
                           ,Before-Message
                           ;; What is the error
                           (print-condition-understandably Condition (format nil "~%~%~A error, " ,Situation))
                           ;; produce a basic stack trace
                           (format t "~% ______________Exception in thread \"~A\"___(backtrace)___" (slot-value *Current-Process* 'ccl::name))
                           (ccl:print-call-history :start-frame-number 1 :detailed-p nil)
                           (format t "~%~%~%")
                           ;; show minmalist message to end-user
                           (ccl::with-autorelease-pool
                               (standard-alert-dialog 
                                (with-output-to-string (Out)
                                  (print-condition-understandably Condition "Error: " Out))
                                :is-critical t
                                :explanation-text (format nil "While ~A" ,Situation)))
                           ,After-Message
                           (throw :wicked-error Condition)))))
       ,@Forms))))




#| Examples:

(print (/ 3.4 (sin 0.0)))

(catch-errors-nicely
  ("trying to divide")
  (print (/ 3.4 (sin 0.0))))


(catch-errors-nicely ("trying to divide" :before-message (print :sandman))
  (print (/ 3.4 (sin 0.0))))


(process-run-function 
  "doing crazy stuff" 
   #'(lambda () (catch-errors-nicely 
                  ("trying to divide") 
                  (print (/ 3.4 (sin 0.0))))))

(in-main-thread () 
  (catch-errors-nicely
    ("trying to divide")
    (/ 3.4 (sin 0.0))))

(hemlock::time-to-run (catch-errors-nicely ("no problem") nil))

(hemlock::time-to-run (+ 3 4))


(defmacro with-standard-fpu-mode (&body body)
 (let ((saved-mode (gensym)))
   `(let ((,saved-mode (get-fpu-mode)))
      (set-fpu-mode :rounding-mode :nearest :overflow t
		     :underflow nil :division-by-zero t
		     :invalid t :inexact nil)
      (unwind-protect
	    (progn
	      ,@body)
	 (apply #'set-fpu-mode ,saved-mode)))))


(in-main-thread () 
  (catch-errors-nicely
    ("trying to divide")
    (with-standard-fpu-mode
      (/ 3.4 (sin 0.0)))))


|#
  