;;; 7/18/2001 Alexander Repenning
;;; 12/07/08 Clozure CL
;;; Micro Second Timer
;;; works for intel macs
;;; Hemlock Editor Extension: Ctrl-x Ctrl-t will show the time is takes to compute the 
;;; current form selected in an editor buffer. Time is in micro seconds
;;; unlike a previous version of this code called hires timer, code will be executed
;;; only once.

(in-package :hemlock)

(export '(time-to-run))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ccl:use-interface-dir :carbon)
  (ccl:open-shared-library "/System/Library/Frameworks/Carbon.framework/Carbon"))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro TIME-TO-RUN (&body Form) "
  in: &body Form {t}.
  Measure the time is takes to run <Form> in Microseconds.
  Form will run only ONCE."
  `(locally (declare (optimize (speed 3) (safety 0)))
     (ccl::without-interrupts
      (ccl:rlet ((t1 :unsigned-long)
                 (t2 :unsigned-long)
                 (t3 :unsigned-long)
                 (t4 :unsigned-long))
        (#_Microseconds t1)             ;; Cache the trap call
        (#_Microseconds t1)
        ,@Form
        (#_Microseconds t2)
        (#_WideSubtract t2 t1)
        ;; compensate overhead time
        (#_Microseconds t3)
        (#_Microseconds t4)
        (#_WideSubtract t4 t3)
        (- (+ (ccl:rref t2 :wide.lo) (* #.(expt 2 32) (ccl:rref t2 :wide.hi)))
           (+ (ccl:rref t4 :wide.lo) (* #.(expt 2 32) (ccl:rref t4 :wide.hi)))))))))


(defun PRINT-TIME (Time &optional (S t))
  "
  in:  Time {float} time in seconds,
       &optional S {stream} default t.
  Print <Time> using s, ms, us, or ns representation."
  (if (<= Time 0.0) 
    (format S "close to timer resolution => repeat test")
    (let ((E (/ (log (abs Time)) #.(log 10))))
      (cond
       ((> E 0)  (format S "~6,2F seconds" Time))
       ((> E -3) (format S "~6,2F ms" (* Time 1e3)))
       ((> E -6) (format S "~A us" (truncate (* Time 1e6))))
       ((> E -9) (format S "~6,2F ns" (* Time 1e9)))
       (t (format S "~E seconds" Time))))))


;;; Hemlock Binding

(defun benchmark-region (region)
  (message 
   (let ((*Package* (buffer-package (current-buffer))))  ;; not implemented yet
     (with-output-to-string (string)
       (let ((Time (/ (eval `(time-to-run ,(read-from-string (region-to-string region))))
		      1000000)))
	 (format String "  Time: ")
	 (print-time Time String))))))


(defcommand "Editor Benchmark Region" (p)
  "Benchmark lisp forms between the point and the mark in the editor Lisp."
  "Benchmark lisp forms between the point and the mark in the editor Lisp."
  (declare (ignore p))
  (if (region-active-p)
    (benchmark-region (current-region))
    (let* ((point (current-point)))
      (pre-command-parse-check point)
      (when (valid-spot point nil)      ; not in the middle of a comment
        (cond ((eql (next-character point) #\()
               (with-mark ((m point))
                 (if (list-offset m 1)
                   (benchmark-region (region point m)))))
              ((eql (previous-character point) #\))
               (with-mark ((m point))
                 (if (list-offset m -1)
                   (benchmark-region (region m point))))))))))


(bind-key "Editor Benchmark Region" #k"control-x control-t")


#| Examples:

;; use the time-to-run macro or do a ctrl-x ctrl-t after selecting expression

(time-to-run (sin 3.3))

(tan 3.3)

(sleep 1.0)

(time-to-run)

(read-from-string "44534535345")

(defvar *Array* (make-array 100))

(dotimes (i 1000) (aref *Array* 3))

|#
