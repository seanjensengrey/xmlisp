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
         #+cocotron (#/flushBuffer ,GLContext)
         #-cocotron (glFlush)  ;;; no double buffering required for OS X Cocoa
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

#-cocotron
(defmethod UNLOCK-VSYNC-ON-MAC ((self opengl-view))
  (when (native-view self)
    (with-glcontext self
      (ccl::rlet ((&intervalValue :int))
        (setf (ccl::%get-byte &intervalValue) 0)
        (#_CGLSetParameter (#_CGLGetCurrentContext) #$kCGLCPSwapInterval &intervalValue)))))


(defmethod INITIALIZE-INSTANCE :after ((Self opengl-view) &rest Args)
  (declare (ignore Args))
  (unless (camera Self)
    (setf (camera Self) (make-instance (camera-type self) :view Self)))
  #-cocotron
  (unlock-vsync-on-mac self))

(objc:defmethod (#/drawRect: :void) ((Self native-opengl-view) (rect :<NSR>ect))
  (with-simple-restart (abandon-drawing "Stop trying OpenGL to draw in ~s" Self)
    (let ((Opengl-View (lui-view Self)))
      (with-glcontext Opengl-View
        ;; HACK:: For some reason on some graphics cards (costco MAchine) we need to initialize the glViewport for it to show up correctly
        ;; I initially tried to add this code to an :after method of prepare-opengl but this caused a crash the first time the glyphs were created
        ;; in openGLviews so far now this is the best place I could find to put the method.  
        #+cocotorn (cocotron-just-in-time-viewport-initialization (lui-view self))
        (clear-background Opengl-View)
        (draw Opengl-View)))))


(objc:defmethod (#/dealloc :void) ((Self native-opengl-view))
  (#/release (#/openGLContext self))
  (objc:remove-lisp-slots self)
  (call-next-method))


#|
(objc:defmethod (#/finalize :void) ((Self native-opengl-view))
  (print "finalzie")
  (print (type-of (lui-view self)))
  (call-next-method))



(objc:defmethod (#/retain :id) ((Self native-opengl-view))
  (print "retain")
  (print (type-of (lui-view self)))
  (prog1
      (call-next-method)
    (print (#/retainCount self)))
  )

(objc:defmethod (#/autorelease :id) ((Self native-opengl-view))
  (print (type-of (lui-view self)))
  (print "autorelease_+_+_+_+_+_+_+_+_+_+_+_+__+_+_+_+_")
 (prog1
      (call-next-method)
    (print (#/retainCount self))))


(objc:defmethod (#/release :id) ((Self native-opengl-view))
  (print "release")
  (print (type-of (lui-view self)))
  (prog1
      (call-next-method)
    (print (#/retainCount self))))

|#
#+cocotron
(defmethod COCOTRON-JUST-IN-TIME-VIEWPORT-INITIALIZATION ((self opengl-view))
  (when (first-time-drawing self)
    (glViewport 0 0 (Width self) (Height self))
    (setf (first-time-drawing self) nil)))


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
  ;; If do-mak-native-object is not set this view will not have a native-object.  I.E. it will never be displayed.
  (unless (do-make-native-object self)
    (return-from make-native-object nil))
  (in-main-thread ()
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
                                        #$NSOpenGLPFADepthSize 32
                                        #$NSOpenGLPFANoRecovery
                                        0}))))
      (unless Pixel-Format (error "Bad OpenGL pixelformat"))
      (let ((Native-Control   
             (make-instance 'native-opengl-view
               :with-frame Frame
               :pixel-format Pixel-Format
               :lui-view Self)))
        ;; sharing  OpenGL context?
       (let ((View-to-Share (or (and (lui::use-global-glcontext Self) (lui::shared-opengl-view))
                                     (lui::share-glcontext-of Self))))
              (when View-to-Share
                (let ((glContext 
                       (#/initWithFormat:shareContext: 
                        (#/alloc ns:ns-opengl-context)
                        (#/pixelFormat native-control) ;; redundant but should be OK
                        (#/openGLContext (lui::native-view View-to-Share)))))
                  (unless glContext (error "cannot share OpenGLContext of view ~A" View-to-Share))
                  (#/setOpenGLContext: Native-Control glContext))))
        (#/release Pixel-Format)
        Native-Control)))))


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
        #-:cocotron (glFlush)
        #+:cocotron  (#/flushBuffer (#/openGLContext (native-view Self)))
        (incf Frame-Count)
        (when (>= (get-internal-real-time) Stop-Time)
          (return Frame-Count))))))


(defmethod SET-SIZE :after ((Self opengl-view) Width Height)
  (in-main-thread ()           
    ;; Avoid drawing under Cocotron if the window isn't visible as that tends
    ;; to fix the positions of the window's views.  (This behavior is noted in
    ;; Cocotron issue 405.  It's not clear if this is the same issue or not.)
                  
    ;; I believe the above comment and the below switch or no longer need, Cocotron issue 405 has been resolved.
    ;; Also, not doing the glViewport if the window is not visible causes the "black screen" issue on Intel HD 
    ;; Graphics cards, I.E. Examples such AgentSheetsGlobe would load with a pure black context and would not
    ;; display its contents till the window was resized, removed the conditional fixes that problem.  I'll leave
    ;; it commented out like this for a little while so we have a chance to test it out before making the change 
    ;; permement.  
    (if t;#+cocotron (#/isVisible (#/window (native-view Self)))
        ;#-cocotron t
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
       (unwind-protect
         (progn
           (when Releasable
             (release-animation-lock))
           ,@Body)
         (when Releasable
           (grab-animation-lock))))))
       


(defun GRAB-ANIMATION-LOCK ()
  "Grab animation lock. Wait if necesary"
  (ccl::grab-lock (animation-lock)))


(defun RELEASE-ANIMATION-LOCK ()
  "Release animation lock. Wait if necesary"
  (ccl::release-lock (animation-lock)))
       

(defvar *Framerate-Ceilling* 
  #-cocotron 60
  #+cocotron nil  ;; the windows timer cannot handle small time
  "max frame rate; simulation not exceed to save CPU. Set to nil to turn off.")


(defvar *Print-Frame-Rate-Info* nil "if true print run time information including frame rate to console")


(defmethod ANIMATE-OPENGL-VIEW-ONCE ((Self opengl-view))
  ;; version with performance tracking
  (let* ((Animation-Time (with-animation-locked
                             (hemlock::time-to-run (animate Self (delta-time Self)))))
         (Rendering-Time (hemlock::time-to-run (display Self)))
         (Total-Time (+ Animation-Time Rendering-Time)))
    ;;remember to remove the above declare if you are going to use the following frame rate test.
    (when (equal total-time 0)
      (setf total-time 0.01))
    (display-frame-rate self animation-time total-time rendering-time)
    ;; Cap framerate 
    (when *Framerate-Ceilling*
      (let ((Sleep-Time (- (/ 1.0 *Framerate-Ceilling*) (* Total-Time 1.0e-9))))
        (when (> Sleep-Time 0.0) 
          (sleep Sleep-Time))))))


(defmethod DISPLAY-FRAME-RATE ((self opengl-view) animation-time total-time rendering-time)
  (when *Print-Frame-Rate-Info*
    (format t "~%animate: ~4,1F ms, ~4D %   render: ~4,1,F ms, ~4D %   Framerate: ~4D fps"
            (* 1.0e-6 Animation-Time)
            (truncate (/ (* 100 Animation-Time) Total-Time))
            (* 1.0e-6 Rendering-Time)
            (- 100 (truncate (/ (* 100 Animation-Time) Total-Time)))
            (truncate (/ 1.0e9 Total-Time)))))


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
                                      ("running simulation"
                                       :after-message (stop-animation self))
                                      (cond
                                       ;; at least one view to be animated
                                       ((animated-views Self)
                                        (animate-opengl-views-once Self)
                                        ;; call the stop function if there is one
                                        (dolist (animated-view  *deactivated-views*)
                                          (when (animation-stop-function animated-view)
                                            (funcall (animation-stop-function animated-view) animated-view)
                                            (setf (animation-stop-function animated-view) nil)))
                                        )
                                       ;; nothing to animate: keep process but use little CPU
                                       (t
                                        (return)))))
                               (:animation-abort-cycle-cleanup (animation-cycle-aborted Self))
                               (t nil))))
                     ;; call animation-aborted still in OpenGL Animations, good idea???
                     
                     (:stop-animation (animation-aborted Self))
                     (t nil))))))))


(defmethod STOP-ANIMATION ((Self opengl-view) &key (Stop-Function #'identity))
  (cond
   ;; if view is animated set its stop function and remove it from the animated views
   ((member Self (animated-views Self))
    (setf (animation-stop-function Self) Stop-Function)
    (setf (animated-views Self) (remove Self (animated-views Self)))
    (setf *deactivated-views* (append *deactivated-views* (list self))))
   ;; view is not animated: just call the stop function immediatly
   (t 
    (funcall Stop-Function Self))))
   

;------------------------------
; Full Screen Support          |
;______________________________

#| No real benefit: disabling multi sample does not appear to save memory nor rendering time

(defmethod ENTER-FULL-SCREEN-MODE :before ((Self opengl-view))
  ;; multisample would be too costly
  (with-glcontext Self
    (glDisable GL_MULTISAMPLE)))


(defmethod EXIT-FULL-SCREEN-MODE :before ((Self opengl-view))
  (with-glcontext Self
    (glEnable GL_MULTISAMPLE)))

|#


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

;------------------------------
; Extensions                  |
;______________________________

(defmethod EXTENSIONS ((Self opengl-view))  
  ;; Will return the opengl extensions for the environment this opengl-view is running in
  (with-glcontext Self
    (let ((extension-pointer (glGetString GL_EXTENSIONS))
          (extension-string (make-array 0
                                    :element-type 'character
                                    :fill-pointer 0
                                    :adjustable t))
          (i 0))
      (loop while (not (equal (lui::%get-byte extension-pointer i) 0))  do
        ;; read and push one character at a time into the extension-string
        (vector-push-extend (code-char (lui::%get-byte extension-pointer i))   extension-string)
        (incf i))
      extension-string)))

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


;; OpenGL Helper functions

(defun GET-OPENGL-INFO (Type)
  (with-glcontext (shared-opengl-view)
    (let ((&bytes (glGetString Type)))
      (with-output-to-string (Info)
        (dotimes (i 10000)
          (let ((Byte (ccl::%get-byte &bytes i)))
            (when (zerop Byte) (return))
            (princ (code-char Byte) Info)))))))



#| Examples:

(get-current-opengl-view)  ;; returns nil if not called within the confines of a legit context

(with-glcontext (shared-opengl-view)
  (get-current-opengl-view))


|#


