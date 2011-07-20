;;-*- Mode: Lisp; Package: :LUI -*- 
;*********************************************************************
;*                                                                   *
;*           O P E N G L   V I E W    C O C O A                      *
;*                                                                   *
;*********************************************************************
;* Author       : Alexander Repenning, alexander@agentsheets.com     *
;*                http://www.agentsheets.com                         *
;* Copyright    : (c) 1996-2009, AgentSheets Inc.                    *
;* Filename     : opengl-view cocoa.lisp                             *
;* Last Update  : 07/16/09                                           *
;* Version      :                                                    *
;*    1.0       : 04/18/09                                           *
;*    1.0.1     : 05/15/09 avoid time travel                         *
;*    1.1       : 07/16/09 implement shared glcontexts               *
;* Systems      : G4, OS X 10.5.6                                    *
;* Lisps        : CLozure CL 1.3                                     *
;* Licence      : LGPL                                               *
;* Abstract     : OpenGL-view base class                             *
;*********************************************************************

(in-package :LUI)


(defclass NATIVE-OPENGL-VIEW (ns:ns-opengl-view)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro WITH-GLCONTEXT (View &rest Forms)
  (let ((GLContext (gensym "glcontext"))
        (CGLContext (gensym "cglcontext")))
    `(let* ((,GLContext (#/openGLContext (native-view ,View)))
            (,CGLContext (#/CGLContextObj ,GLContext)))
       (unwind-protect
           (progn
             (#_CGLLockContext ,CGLContext)
             (#/makeCurrentContext ,GLContext)
             (progn ,@Forms))
         (#/flushBuffer ,GLContext)
         (#/clearCurrentContext ns:ns-opengl-context)
         (#_CGLUnlockContext  ,CGLContext))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro WITH-GLCONTEXT-NO-FLUSH (View &rest Forms)
  (let ((GLContext (gensym "glcontext"))
        (CGLContext (gensym "cglcontext")))
    `(let* ((,GLContext (#/openGLContext (native-view ,View)))
            (,CGLContext (#/CGLContextObj ,GLContext)))
       (unwind-protect
           (progn
             (#_CGLLockContext ,CGLContext)
             (#/makeCurrentContext ,GLContext)
             (progn ,@Forms))
         (#/clearCurrentContext ns:ns-opengl-context)
         (#_CGLUnlockContext  ,CGLContext))))))


(objc:defmethod (#/drawRect: :void) ((Self native-opengl-view) (rect :<NSR>ect))
  (with-simple-restart (abandon-drawing "Stop trying OpenGL to draw in ~s" Self)
    (let ((Opengl-View (lui-view Self)))
      (with-glcontext Opengl-View
        (clear-background Opengl-View)
        (draw Opengl-View)))))


(objc:defmethod (#/prepareOpenGL :void) ((Self native-opengl-view))
  (prepare-opengl (lui-view Self)))


;; BIG MISTERY: not clear why this method needs to be here. 
;; Without it the window view does not appear to receive mouse events such as the mouse moved event

(objc:defmethod (#/mouseDown: :void) ((self native-opengl-view) event)
  (let ((mouse-loc (#/locationInWindow event)))
    (view-event-handler (window (lui-view Self)) 
                        (make-instance 'mouse-event
                          :x (truncate (pref mouse-loc :<NSP>oint.x))
                          :y (truncate (- (height (window (lui-view Self))) (pref mouse-loc :<NSP>oint.y)))
                          :event-type (native-to-lui-event-type (#/type event))
                          :native-event Event))))


(objc:defmethod (#/rightMouseDown: :void) ((self native-opengl-view) event)
  (let ((mouse-loc (#/locationInWindow event)))
    (view-event-handler (window (lui-view Self)) 
                        (make-instance 'mouse-event
                          :x (truncate (pref mouse-loc :<NSP>oint.x))
                          :y (truncate (- (height (window (lui-view Self))) (pref mouse-loc :<NSP>oint.y)))
                          :event-type (native-to-lui-event-type (#/type event))
                          :native-event Event))))



(objc:defmethod (#/viewDidMoveToWindow :void) ((self native-opengl-view))
  (call-next-method)
  (view-did-move-to-window (lui-view self)))
  

(defmethod MAKE-NATIVE-OBJECT ((Self opengl-view))
  (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
    (let ((Pixel-Format (#/initWithAttributes: 
                          (#/alloc ns:NS-OpenGL-Pixel-Format) 
                          (if (full-scene-anti-aliasing Self)
			    ;;--- Can't use conditionals inside {} (Sigh)
                            #+cocotron {#$NSOpenGLPFAColorSize 32 
                                        #$NSOpenGLPFADoubleBuffer 
                                        #$NSOpenGLPFADepthSize 32
                                        0}
                            #-cocotron {#$NSOpenGLPFAColorSize 32 
                                        #$NSOpenGLPFADoubleBuffer 
                                        #$NSOpenGLPFADepthSize 32
                                        #$NSOpenGLPFASampleBuffers 1
                                        #$NSOpenGLPFASamples 4
                                        #$NSOpenGLPFANoRecovery
                                        0}
                            ;;--- Can't use conditionals inside {} (Sigh)
                            #+cocotron {#$NSOpenGLPFAColorSize 32 
                                        #$NSOpenGLPFADoubleBuffer 
                                        #$NSOpenGLPFADepthSize 32
                                        0}
                            #-cocotron {#$NSOpenGLPFAColorSize 32 
                                        #$NSOpenGLPFADoubleBuffer 
                                        #$NSOpenGLPFADepthSize 32
                                        #$NSOpenGLPFANoRecovery
                                        0}))))
      (unless Pixel-Format (error "Bad OpenGL pixelformat"))
      (let ((Native-Control (make-instance 'native-opengl-view
                              :with-frame Frame
                              :pixel-format Pixel-Format
                              :lui-view Self)))
        ;; sharing  OpenGL context?
        (let ((View-to-Share (or (and (use-global-glcontext Self) (shared-opengl-view))
                                 (share-glcontext-of Self))))
          (when View-to-Share
            (let ((glContext 
                   (#/initWithFormat:shareContext: 
                    (#/alloc ns:ns-opengl-context);(#/openGLContext Native-Control)
                    (#/pixelFormat native-control) ;; redundant but should be OK
                    (#/openGLContext (native-view View-to-Share)))))
              (unless glContext (error "cannot share OpenGLContext of view ~A" View-to-Share))
              (#/setOpenGLContext: native-control glContext)
              )))
        (#/release Pixel-Format)
        Native-Control))))


(defmethod DISPLAY ((Self opengl-view))  
  (with-glcontext Self
    (clear-background Self)
    (draw Self)))


(defmethod DISPLAY-WITH-FORCE ((Self opengl-view))  
  #+cocotron
  (#/setNeedsDisplay: (native-view self) #$YES)
  (display self))


(defmethod FRAME-RATE ((Self opengl-view))
  (with-glcontext Self
    (let ((Frame-Count 0)
          (Stop-Time (+ (get-internal-real-time) internal-time-units-per-second)))
      (loop
        (clear-background Self)
        (draw Self)
        (#/flushBuffer (#/openGLContext (native-view Self)))
        (incf Frame-Count)
        (when (>= (get-internal-real-time) Stop-Time)
          (return (values Frame-Count (- (get-internal-real-time)
                                         (- Stop-Time internal-time-units-per-second)))))))))


(defmethod SET-SIZE :after ((Self opengl-view) Width Height)
  (in-main-thread ()           
    ;; Avoid drawing under Cocotron if the window isn't visible as that tends
    ;; to fix the positions of the window's views.  (This behavior is noted in
    ;; Cocotron issue 405.  It's not clear if this is the same issue or not.)
    (if #+cocotron (#/isVisible (#/window (native-view Self)))
        #-cocotron t
      (with-glcontext Self
        (glflush)
        (glViewport 0 0 Width Height)
        (when (and (not (equal Height 0)) (camera Self))
          (aim-camera (camera Self) :aspect (float (/ Width Height)))))
      (progn
        (when (camera Self)
          (aim-camera (camera Self) :aspect (float (/ Width Height))))
        (#/setNeedsDisplay: (native-view Self) #$YES)))))

;------------------------------
; Animation                    |
;______________________________


(defparameter *Animation-Process* nil "the process running all OpenGL animations")


(defmethod DELTA-TIME ((Self opengl-view)) "
  Return time in seconds passed since last animation."
  #+:X8632-target (declare (optimize (safety 2))) ;; avoid 2^32 nano second time warps in CCL 32bit
  #-windows-target
  (let ((Time (#_mach_absolute_time)))
    (prog1
        (if (zerop (animation-time Self))
          0.0                           ;First time through
          (float (* 0.000000001
                    (- Time (animation-time Self))
                    #.(ccl::rlet ((info #>mach_timebase_info))
                        (#_mach_timebase_info info)
                        (/ (ccl::pref info #>mach_timebase_info.numer)
                           (ccl::pref info #>mach_timebase_info.denom))))))
      (setf (animation-time Self) Time)))
  #+windows-target
  (let ((Time (rlet ((now #>FILETIME))
                ;; this Win32 timer function is no good: typically 10ms or worse resolution!
                ;; consider using QueryPerformanceCounterrad http://www.devsource.com/c/a/Techniques/High-Performance-Timing-under-Windows/1/
		(#_GetSystemTimeAsFileTime now)
		(dpb (pref now #>FILETIME.dwHighDateTime)
		     (byte 32 32)
		     (pref now #>FILETIME.dwLowDateTime)))))
    (prog1
	(if (zerop (animation-time Self))
          0.0                           ;First time through
          (float (* 0.0000001 (- Time (animation-time Self)))))
      (setf (animation-time Self) Time))))


;(defparameter *Animation-Time* nil)
(defmethod DELTA-TIME2 () "
  Return time in seconds passed since last animation."
  #+:X8632-target (declare (optimize (safety 2))) ;; avoid 2^32 nano second time warps in CCL 32bit
  #-windows-target
  (let ((Time (#_mach_absolute_time)))
    (prog1
        (if (zerop (animation-time Self))
          0.0                           ;First time through
          (float (* 0.000000001
                    (- Time (animation-time Self))
                    #.(ccl::rlet ((info #>mach_timebase_info))
                        (#_mach_timebase_info info)
                        (/ (ccl::pref info #>mach_timebase_info.numer)
                           (ccl::pref info #>mach_timebase_info.denom))))))
      (setf (animation-time Self) Time)))
  #+windows-target
  (let ((Time (rlet ((now #>FILETIME))
                ;; this Win32 timer function is no good: typically 10ms or worse resolution!
                ;; consider using QueryPerformanceCounterrad http://www.devsource.com/c/a/Techniques/High-Performance-Timing-under-Windows/1/
		(#_GetSystemTimeAsFileTime now)
		(dpb (pref now #>FILETIME.dwHighDateTime)
		     (byte 32 32)
		     (pref now #>FILETIME.dwLowDateTime)))))
    (prog1
	(if (zerop (animation-time Self))
          0.0                           ;First time through
          (float (* 0.0000001 (- Time (animation-time Self)))))
      (setf (animation-time Self) Time))))


(defmethod ANIMATE ((Self opengl-view) Time)
  (declare (ignore Time))
  ;; nada
  )


(defvar *Animation-Lock* nil "Lock used to protect animations initiated in different threads")


(defun ANIMATION-LOCK () "Return animation lock. Create lock if needed"
  (or *Animation-Lock* 
      (setq *Animation-Lock* (ccl::make-lock))))


(defmacro WITH-ANIMATION-LOCKED (&rest Body) "Make sure only one animation thread is active at a time"
  `(progn
     (with-lock-grabbed ((animation-lock))
       ,@Body)))


(defun POSSIBLE-RECURSIVE-LOCK-OWNER (lock)
  ;; written by Gary Byers <gb@clozure.com>
  (let* ((tcr (ccl::%get-object (ccl::recursive-lock-ptr lock) target::lockptr.owner)))
    (unless (eql 0 tcr) (ccl::tcr->process tcr))))


(defmacro WITH-ANIMATION-UNLOCKED (&rest Body) "Make sure to release animation lock if running in animation thread"
  `(progn
     (let* ((Lock-Owner (possible-recursive-lock-owner (animation-lock)))
            (Releasable (and Lock-Owner
                             (string-equal (slot-value ccl::*current-process* 'ccl::name) (slot-value Lock-Owner 'ccl::name)))))
       (when Releasable
         (release-animation-lock))
       ,@Body
       (when Releasable
         (grab-animation-lock)))))
       


(defun GRAB-ANIMATION-LOCK ()
  "Grab animation lock. Wait if necesary"
  (ccl::grab-lock (animation-lock)))


(defun RELEASE-ANIMATION-LOCK ()
  "Release animation lock. Wait if necesary"
  (ccl::release-lock (animation-lock)))
       

(defvar *Framerate-Ceilling* 60 "max frame rate; simulation not exceed to save CPU")


#-cocotron       
(defmethod ANIMATE-OPENGL-VIEW-ONCE ((Self opengl-view))
  ;; version with performance tracking
  (let* ((Animation-Time (with-animation-locked
                             (hemlock::time-to-run (animate Self (delta-time Self)))))
         (Rendering-Time (hemlock::time-to-run (display Self)))
         (Total-Time (+ Animation-Time Rendering-Time)))
    ;(declare (ignore total-time))
    ;;remember to remove the above declare if you are going to use the following frame rate test.
    (when (shift-key-p)
      (format t "~%animate: ~4,1F ms, ~4D %   render: ~4,1,F ms, ~4D %   Framerate: ~4D fps"
              (* 1.0e-6 Animation-Time)
              (truncate (/ (* 100 Animation-Time) Total-Time))
              (* 1.0e-6 Rendering-Time)
              (- 100 (truncate (/ (* 100 Animation-Time) Total-Time)))
              (truncate (/ 1.0e9 Total-Time))))
    ;; Cap framerate 
    (let ((Sleep-Time (- #.(/ 1.0 *Framerate-Ceilling*) (* Total-Time 1.0e-9))))
      (when (> Sleep-Time 0.0) 
        (sleep Sleep-Time)
        ;(format t "~%sleep time: ~A" Sleep-Time)
        ))))


#+cocotron
(defmethod ANIMATE-OPENGL-VIEW-ONCE ((Self opengl-view))
  (with-animation-locked
      (animate Self (delta-time Self)))
  (display Self)
  (sleep 0.01)  ;; ease off CPU time, need to compute this time 
  )


(defmethod ANIMATE-OPENGL-VIEWS-ONCE ((Self opengl-view))"
  Cycle once through all the OpenGL views that need to be animated"
  (dolist (View (animated-views Self))
    (animate-opengl-view-once View)))


(defmethod START-ANIMATION ((Self opengl-view))
  ;; add myself to list
  (pushnew Self (animated-views Self))
  ;; Create an animation processs if needed
  (when (or (null *Animation-Process*) (ccl::process-exhausted-p *Animation-Process*))
    (setq *Animation-Process* 
          (process-run-function
           '(:name "OpenGL Animations" :priority 0)
           #'(lambda ()
               (ccl::with-autorelease-pool
                   (case (catch :animation-abort
                           (loop
                             ;; allow the current animation cycle to be aborted but keep the thread running
                             (case (catch :animation-abort-cycle
                                     (catch-errors-nicely 
                                      ("running simulation")
                                      (cond
                                       ;; at least one view to be animated
                                       ((animated-views Self)
                                        (animate-opengl-views-once Self))
                                       ;; nothing to animate: keep process but use little CPU
                                       (t
                                        (sleep 0.5)))))
                               (:animation-abort-cycle-cleanup (animation-cycle-aborted Self))
                               (t nil))))
                     ;; call animation-aborted still in OpenGL Animations, good idea???
                     (:stop-animation (animation-aborted Self))
                     (t nil))))))))


(defmethod STOP-ANIMATION ((Self opengl-view))
  (setf (animated-views Self) (remove Self (animated-views Self))))


;------------------------------
; Full Screen Support          |
;______________________________

(defmethod ENTER-FULL-SCREEN-MODE :before ((Self opengl-view))
  ;; multisample would be too costly
  (with-glcontext Self
    (glDisable GL_MULTISAMPLE)))


(defmethod EXIT-FULL-SCREEN-MODE :before ((Self opengl-view))
  (with-glcontext Self
    (glEnable GL_MULTISAMPLE)))


;------------------------------
; Query functions              |
;______________________________

(defun GET-CURRENT-OPENGL-VIEW ()
  "return the view of the currently active OpenGL context"
  (let ((Context (#/currentContext ns:ns-opengl-context)))
    (unless (%null-ptr-p Context)
      (let ((View (#/view Context)))
        (unless (%null-ptr-p View)
          (lui-view View))))))


(objc:defmethod (#/mouseEntered: :void) ((self native-opengl-view) Event)
  (call-next-method event)
  (mouse-entered (lui-view self)))


(objc:defmethod (#/mouseExited: :void) ((self native-opengl-view) Event)
  (call-next-method event)
  (mouse-exited (lui-view self)))

;______________________________
; Events                       |
;______________________________

(defmethod VIEW-LEFT-MOUSE-DRAGGED-EVENT-HANDLER ((Self opengl-view) X Y DX DY)
  (declare (ignore X Y))
  ;; this is pretty save: do not need to lock 
  (with-animation-unlocked
      (track-mouse-3d (camera Self) Self dx dy))
  (unless (is-animated Self) (display Self)))


;______________________________
; Shader Support               |
;______________________________

(defmethod SET-SHADER-SOURCE (Shader Name)
  (let ((Strings nil))
    (with-open-file (Source (native-path "lui:resources;shaders;" Name) :direction :input)
      (loop
        (let ((String (read-line Source nil nil)))
          (unless String (return))
          (setq Strings (append Strings (list String))))))
    (let ((Size 0))
      ;; compute size
      (dolist (String Strings)
        (incf Size (1+ (length String)))) ;; add space for #\null char
      (with-vector-of-size (Null-Terminated-Strings Size)
        (let ((I 0))
          (dolist (String Strings)
            ;; copy string
            (dotimes (J (length String))
              (set-byte Null-Terminated-Strings (char-code (char String j)) i)
              (incf i))
            ;; terminate
            (set-byte Null-Terminated-Strings (char-code #\newline) i)
            (incf i))
          ;; replace last linefeed with null
          (set-byte Null-Terminated-Strings (char-code #\null) (1- Size))
          ;; set shader source
          (ccl::rlet ((&Null-Terminated-Strings :address))
            ;; need a pointer to a pointer of chars for source string
            (setf (ccl::%get-ptr &Null-Terminated-Strings) Null-Terminated-Strings)
            (glShaderSource Shader 1 &Null-Terminated-Strings (ccl::%null-ptr)))
          Strings)))))


(defmethod GET-SHADER-INFO-LOG (Object)
  (let* ((Max-Size 2000))
    (with-vector-of-size (&chars Max-Size)
      (with-output-to-string (string)
        (ccl::rlet ((&size :long))
          (glGetShaderInfoLog Object Max-Size &size &Chars)
          (dotimes (I (ccl::%get-long &size))
            (princ (code-char (ccl::%get-byte &Chars i)) String)))))))


#| Examples:

(get-current-opengl-view)  ;; returns nil if not called within the confines of a legit context

(with-glcontext (shared-opengl-view)
  (get-current-opengl-view))


|#


