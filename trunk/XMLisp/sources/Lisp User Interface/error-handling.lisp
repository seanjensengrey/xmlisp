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
                         (ccl:print-call-history :start-frame-number 1 :detailed-p nil)
                         (standard-alert-dialog 
                          (with-output-to-string (Out)
                            (print-condition-understandably Condition "Error: " Out))
                          :is-critical t
                          :explanation-text (format nil "While ~A (in process: \"~A\")" ,Situation (slot-value *Current-Process* 'ccl::name)))
                         (throw :wicked-error Condition))))
       ,@Forms))))




#| Examples:

(print (/ 3.4 (sin 0.0)))

(catch-errors-nicely "trying to divide" (print (/ 3.4 (sin 0.0))))

(process-run-function 
  "doing crazy stuff" 
   #'(lambda () (catch-errors-nicely "trying to divide" (print (/ 3.4 (sin 0.0))))))

|#
  