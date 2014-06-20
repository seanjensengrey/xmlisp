;;; 7/18/2001 Alexander Repenning
;;; 12/12/08 Clozure CL
;;; Nano Second Timer
;;; works for intel macs
;;; Hemlock Editor Extension: Ctrl-x Ctrl-t will show the time is takes to compute the 
;;; current form selected in an editor buffer. Time is in nano seconds
;;; unlike a previous version of this code called hires timer, code will be executed
;;; only once.

(in-package :hemlock)

(export '(time-to-run time-to-run-milli))
#-cocotron
(defvar *mach-time-unit-ratio*
 (ccl::rlet ((info #>mach_timebase_info))
   (#_mach_timebase_info info)
   (/ (ccl::pref info #>mach_timebase_info.numer)
      (ccl::pref info #>mach_timebase_info.denom))))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro TIME-TO-RUN (&body Form) "
  in: &body Form {t}.
  Measure the time is takes to run <Form> in Nano seconds.
  Form will run only ONCE."
  #-cocotron
  `(locally (declare (optimize (speed 3) (safety 0)))
     (ccl::without-interrupts
       (let (t2 t3 t4)
         (setq t2 (#_mach_absolute_time))  ;; prime/cache the call: otherwise delta time will be significantly larger between first two calls
         (setq t2 (#_mach_absolute_time))
         ,@Form
         (setq t3 (#_mach_absolute_time))
         (setq t4 (#_mach_absolute_time))
         (values (round (* (max (- t3 t2 (- t4 t3)) 0)  *mach-time-unit-ratio*))))))
  #+cocotron
  `(locally (declare (optimize (speed 3) (safety 0)))
     (ccl::without-interrupts
      (let ((t1 (delta-time2))
            (t2 nil))
        ,@Form
        (setf t2 (delta-time2))
        (* 1000000000 (max t2 0))
        )
      ))))

(defmacro TIME-TO-RUN-MILLI (&body Form)
  (/ (TIME-TO-RUN Form) 1000000.0))
#-cocotron
(defun PRINT-TIME (Time &optional (S t))
  "
  in:  Time {float} time in seconds,
       &optional S {stream} default t.
  Print <Time> using s, ms, us, or ns representation."
  (if (<= Time 0.0) 
    (format S "close to timer resolution => repeat test")
    (let ((E (/ (log (abs Time)) #.(log 10))))
      (cond
       ((> E 0)  (format S "~6,3F seconds" Time))
       ((> E -3) (format S "~6,2F ms" (* Time 1e3)))
       ((> E -6) (format S "~6,2F us"  (* Time 1e6)))
       ((> E -9) (format S "~6,2F ns" (* Time 1e9)))
       (t (format S "~E seconds" Time))))))

(defparameter *animation-time* nil)
(defun GET-ANIMATION-TIME ()
  (or *animation-time*
      (setf *animation-time* 0.0)))

(defun DELTA-TIME2 () "
  Return time in seconds passed since last animation."
  #+:X8632-target (declare (optimize (safety 2))) ;; avoid 2^32 nano second time warps in CCL 32bit
  #-windows-target
  (let ((Time (#_mach_absolute_time)))
    (prog1
        (if (zerop (get-animation-time))
          0.0                           ;First time through
          (float (* 0.000000001
                    (- Time (get-animation-time))
                    #.(ccl::rlet ((info #>mach_timebase_info))
                        (#_mach_timebase_info info)
                        (/ (ccl::pref info #>mach_timebase_info.numer)
                           (ccl::pref info #>mach_timebase_info.denom))))))
      (setf *animation-time* Time)
      ))
  #+windows-target
  (let ((Time (ccl::rlet ((now #>FILETIME))
                ;; this Win32 timer function is no good: typically 10ms or worse resolution!
                ;; consider using QueryPerformanceCounterrad http://www.devsource.com/c/a/Techniques/High-Performance-Timing-under-Windows/1/
		(#_GetSystemTimeAsFileTime now)
		(dpb (ccl::pref now #>FILETIME.dwHighDateTime)
		     (byte 32 32)
		     (ccl::pref now #>FILETIME.dwLowDateTime)))))
    (prog1
	(if (zerop (get-animation-time))
          0.0                           ;First time through
          (float (* 0.0000001 (- Time (get-animation-time)))))
      (setf *animation-time* Time)
      )))

(defmacro TIME-TO-RUN2 (&body Form)
;;; Hemlock Bindings
  `(locally (declare (optimize (speed 3) (safety 0)))
     (ccl::without-interrupts
      (let ((t1 (delta-time2))
            (t2 nil))
         ,@Form
        (setf t2 (delta-time2))
        (* 1000000000 (max t2 0))
        )
      )))
#-cocotron     
(defun benchmark-region (region)
  (message 
   (let ((*Package* (buffer-package (current-buffer))))  ;; not implemented yet
     (with-output-to-string (string)
       (let ((Time (eval `(time-to-run ,(read-from-string (region-to-string region))))))
	 (format String "  Time: ")
	 (print-time (/ Time 1000000000.0) String))))))

#-cocotron
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

#-cocotron
(bind-key "Editor Benchmark Region" #k"control-x control-t")


#| Examples:

;; use the time-to-run macro or do a ctrl-x ctrl-t after selecting expression

(time-to-run (sin 3.3))

(tan 3.3)

(sleep 1.0)

(time-to-run)

(read-from-string "44534535345")
(read-from-string "4")

(member 'z '(a h k g r i f l i j g k l b l g z))


(defparameter *HT* (make-hash-table :test #'eq))

(setf (gethash 99 *ht*) 'bla)

(defparameter test (* 67 34))

(setq test (* 67 34))

(defvar *Array* (make-array 100))

(dotimes (i 1000) (aref *Array* 3))

|#
