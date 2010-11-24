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
         ;;New Code....
         ;(#/makeCurrentContext ,GLContext)
         ;;Ask Alex....
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
            #-cocotron
            (let ((glContext 
                   (#/initWithFormat:shareContext: 
                    (#/openGLContext Native-Control)
                    Pixel-Format ;; redundant but should be OK
                    (#/openGLContext (native-view View-to-Share)))))
              
              ;; (format t "~%before ~A ~%after ~A" (#/openGLContext Native-Control) glContext)))
              (unless glContext (error "cannot share OpenGLContext of view ~A" View-to-Share)))
            ;(print "WGL SHARE LISTS")
            ;(print (#_wglShareLists (%get-ptr (#/CGLContextObj (#/openGLContext (native-view view-to-share)))36) (%get-ptr (#/CGLContextObj glContext) 36)))
            )
          
          )
        (#/release Pixel-Format)
        Native-Control))))


(defmethod SHARE-TEXTURE-FOR-WINDOWS ((Self opengl-view))
  (print "WGL SHARE LISTS")
  #+cocotron
  (print (#_wglShareLists (%get-ptr (#/CGLContextObj (#/openGLContext (native-view (shared-opengl-view))))36) (%get-ptr (#/CGLContextObj (#/openGLContext (native-view self))) 36))))


(defmethod DISPLAY ((Self opengl-view))  
  (with-glcontext Self
    (clear-background Self)
    (draw Self)))


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
        (when (camera Self)
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
  `(with-lock-grabbed ((animation-lock))
     ,@Body))


(defun GRAB-ANIMATION-LOCK ()
  "Grab animation lock. Wait if necesary"
  (ccl::grab-lock (animation-lock)))


(defun RELEASE-ANIMATION-LOCK ()
  "Release animation lock. Wait if necesary"
  (ccl::release-lock (animation-lock)))
       
       
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
               (loop
                 (catch-errors-nicely 
                  "OpenGL Animation"
                  (cond
                   ;; at least one view to be animated
                   ((animated-views Self)
                    (ccl::with-autorelease-pool
                        (animate-opengl-views-once Self)))
                   ;; nothing to animate: keep process but use little CPU
                   (t
                    (sleep 0.5))))))))))


(defmethod STOP-ANIMATION ((Self opengl-view))
  (setf (animated-views Self) (remove Self (animated-views Self))))

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


(objc:defmethod (#/mouseMoved: :void) ((self native-opengl-view) Event)
  (call-next-method event)
  ;(mouse-exited (lui-view self))
  )

#| Examples:

(get-current-opengl-view)  ;; returns nil if not called within the confines of a legit context

(with-glcontext (shared-opengl-view)
  (get-current-opengl-view))


|#


