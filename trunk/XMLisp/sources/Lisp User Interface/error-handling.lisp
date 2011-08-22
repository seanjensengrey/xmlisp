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


(defvar *Output-to-Alt-Console-p* t "If true output error info into Alt Console on Mac and Windows")


(defun PRINT-TO-LOG (Control-String &rest Format-Arguments) "
  Print to platform specific log file. On Mac access output via Console.app"
  (if *Output-To-Alt-Console-P*
    (apply #'format t Control-String Format-Arguments)
    (let ((NSString (#/retain (ccl::%make-nsstring (apply #'format nil Control-String Format-Arguments)))))
      (#_NSLog #@"%@" :address NSString)  ;; make sure NSString is not interpreted as format string
      (#/release NSString))))
    

(defun PRINT-CONDITION-UNDERSTANDABLY (Condition &optional (Message "") (Stream t))
  (format Stream "~A " Message)
  (ccl::report-condition Condition Stream))


(defun PRINT-DATE-AND-TIME-STAMP (&optional (Stream t))
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
                       (get-decoded-time)
    (declare (ignore DST-P))
    (format Stream "~2,'0d:~2,'0d:~2,'0d of ~a, ~d/~2,'0d/~d (GMT~@d)"
            hour
            minute
            second
            (nth day-of-week '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
            month
            date
            year
            (- tz))))
  

(eval-when (:execute :load-toplevel :compile-toplevel)
(defmacro CATCH-ERRORS-NICELY ((Situation &key Before-Message After-Message) &body Forms) "Catch errors at high level. Also works in gui threads and Cocoa call backs"
  `(catch :wicked-error
     (handler-bind
         ((warning #'(lambda (Condition)
                       (print-to-log 
                        "~A"
                        (with-output-to-string (Out)
                          (print-condition-understandably Condition (format nil "~A warning, " ,Situation) Out)))
                       (muffle-warning)))
          (condition #'(lambda (Condition)
                         (let ((*XMLisp-Print-Synoptic* t))
                           ;; no way to continue
                           ,Before-Message
                           ;; What is the error
                           (ccl::with-autorelease-pool
                               (print-to-log
                                "~A"
                                (with-output-to-string (Log)
                                  (format Log "##############################################~%")
                                  (print-condition-understandably Condition "Error: " Log)
                                  (format Log "~%While ~A~%~%" ,Situation)
                                  (print-date-and-time-stamp Log)
                                  (format Log "~%##############################################~%")
                                  ;; produce a basic stack trace
                                  (format Log "~% ______________Exception in thread \"~A\"___(backtrace)___" (slot-value *Current-Process* 'ccl::name))
                                  (ccl:print-call-history :start-frame-number 1 :detailed-p nil :stream Log)
                                  (format Log "~%~%~%")))
                             ;; show minmalist message to end-user
                             (in-main-thread () ;; OS X 10.7 is really insisting on using NSAlerts only in the mmain thread
                               (standard-alert-dialog 
                                (with-output-to-string (Out)
                                  (print-condition-understandably Condition "Error: " Out))
                                :is-critical t
                                :explanation-text (format nil "While ~A" ,Situation)))
                             ,After-Message
                             (throw :wicked-error Condition))))))
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


(catch-errors-nicely
  ("running simulation")
  (warn "bad agent action"))


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


(print-to-log "this and that")

(print-to-log "only ~A bottles of beer" 69)


|#
  