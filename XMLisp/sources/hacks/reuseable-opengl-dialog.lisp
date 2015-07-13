(in-package :xlui)


(defclass REUSABLE-OPENGL-DIALOG (opengl-dialog)
  ((reusable-glcontext :accessor reusable-glcontext :initform (make-hash-table) :allocation :class :documentation "A one-at-a-time reusable opengl contexts usable only in modal windows with a single opengl view of one class at a time"))
  (:documentation "Can only use ONE instance of any particulate subclass at a time in one window."))


(defmethod lui::MAKE-NATIVE-OBJECT ((Self reusable-opengl-dialog))
  (lui::in-main-thread ()
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
        (let ((Native-Control (make-instance 'lui::native-opengl-view
                                :with-frame Frame
                                :pixel-format Pixel-Format
                                :lui-view Self)))
          (cond 
           ;; REUSE!
           ((gethash (type-of Self) (reusable-glcontext Self))
            (print :reuse)
            ;; should we release the current glcontent before we assign a new one??
            ;; reuse
            (#/setOpenGLContext: Native-Control (gethash (type-of Self) (reusable-glcontext Self)))
            ;; perhaps some reinitialization needed here...
            )
           ;; NEW
           (t
            ;; sharing  OpenGL context?
            (print :new)
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
            ;; use a reusable
            (setf (gethash (type-of Self) (reusable-glcontext Self)) (#/openGLContext Native-Control))))
          (#/release Pixel-Format)
          Native-Control)))))


#| Examples:

;; make only one at a time: notice opengl context count does NOT go up after first one
;; if you open more than one: Mac mouse will control wrong window content; PC blank window (looks surprisgnly similar to "running out of 32 opengl Window 7 problem")

<application-window margin="0">
  <reusable-opengl-dialog/>
</application-window>

;; need to patch:

  (defclass IMAGE-EDITOR (reusable-opengl-dialog)
  (defclass INFLATED-ICON-EDITOR (reusable-opengl-dialog)



;; make only one at a time: notice opengl context count does NOT go up after first one

<application-window margin="0">
  <opengl-dialog/>
</application-window>


|#
