;;;-*- Mode: Lisp; Package: lui -*-
;*********************************************************************
;*                                                                   *
;*        T R A N S P A R E N T   O P E N   G L   W I N D O W        *
;*                                                                   *
;*********************************************************************
   ;* Author    : Alexander Repenning (alexander@agentsheets.com)    *
   ;*             http://www.agentsheets.com                         *
   ;* Copyright : (c) 2004-2009, AgentSheets Inc.                    *
   ;* Filename  : Transparent-Opengl-Window.lisp                     *
   ;* Updated   : 05/07/05                                           *
   ;* Version   :                                                    *
   ;*   1.0     : 11/04/04                                           *
   ;*   1.0.1   : 04/05/05 clear depth buffer                        *
   ;*   1.0.2   : 05/07/05 Tiger workaround for AGL_SURFACE_OPACITY  *
   ;*   2.0     : 07/09/09 Cocoa                                     *
   ;*   2.0.1   : 07/29/09 draw-rect method                          *
   ;* SW/HW     : PowerPC G4, OS X 10.5.7, CCL 1.3                   *
   ;* Abstract  : Window used for Drag and Drop Feedback             *
   ;*                                                                *
   ;******************************************************************


(in-package :lui)

(export '(transparent-opengl-window))


;____________________________________________
; NATIVE-TRANSPARENT-OPENGL-VIEW             |
;____________________________________________

(defclass NATIVE-TRANSPARENT-OPENGL-VIEW (ns:ns-opengl-view)
  ((lui-window :accessor lui-window :initarg :lui-window))
  (:metaclass ns:+ns-object))


(objc:defmethod (#/prepareOpenGL :void) ((Self native-transparent-opengl-view))
  ;; NSOpenGLView appears to default to some opaque background: need to clear that
  (#/set (#/clearColor ns:ns-color))
  (#_NSRectFill (#/bounds Self))
  (prepare-opengl (lui-window Self)))


(objc:defmethod (#/drawRect: :void) ((Self native-transparent-opengl-view) (rect :<NSR>ect)) 
  (draw-rect (lui-window Self)))

;____________________________________________
; TRANSPARENT-OPENGL-WINDOW                  |
;____________________________________________

(defclass TRANSPARENT-OPENGL-WINDOW ()
  ((display-function :accessor display-function :initform nil :initarg :display-function :documentation "0 parameter function using OpenGL calls to display content")
   (init-function :accessor init-function :initform nil :initarg :init-function :documentation "0 parameter function using OpenGL calls to initialize window")
   (tracking-function  :accessor tracking-function :initform nil :initarg :tracking-function :documentation "called with x and y when mouse position changed")
   (native-window :accessor native-window :initform nil :documentation "native OS window object")
   (native-view :accessor native-view :initform nil :documentation "native OS view object")
   (share-context-of :accessor share-context-of :initform nil :initarg :share-context-of)
   (use-global-glcontext :accessor use-global-glcontext :initform t :initarg :use-global-glcontext :type boolean :documentation "set to t if you want to share the global glcontext")
   (x :accessor x :initform 0 :initarg :x :documentation "screen position, pixels")
   (y :accessor y :initform 0 :initarg :y :documentation "screen position, pixels")
   (width :accessor width :initform 170 :initarg :width :documentation "width in pixels")
   (height :accessor height :initform 90 :initarg :height :documentation "height in pixels")
   (alpha :accessor alpha :initform 0.4d0 :initarg :alpha :documentation "alpha value: 0=transparent..1=opaque"))
  (:documentation "A transparent window is used as feedback for drag and drop "))


(defmethod INITIALIZE-INSTANCE ((Self transparent-opengl-window) &rest Initargs)
  (declare (ignore Initargs))
  (call-next-method)
  (ns:with-ns-rect (Frame 
                    (x Self)
                    ;; replace with more general code
                    (- (pref (#/frame (#/mainScreen ns:ns-screen)) <NSR>ect.size.height)
                       (y Self) (height Self))
                    (width Self)
                    (height Self))
    (setf (native-window Self) (make-instance 'ns:ns-window
                                 :with-content-rect Frame
                                 :style-mask #$NSBorderlessWindowMask
                                 :backing #$NSBackingStoreBuffered
                                 :defer #$NO))
    (#/setOpaque: (native-window Self) #$NO)
    (#/setAlphaValue: (native-window Self) (gui::cgfloat 0.5))
    (#/setBackgroundColor: (native-window Self) (#/clearColor ns:ns-color))
    (#/setHasShadow: (native-window self) #$NO)
    (let ((Pixel-Format (#/initWithAttributes: 
                         (#/alloc ns:NS-OpenGL-Pixel-Format)
			 ;;--- I'd like to simply #-cocotron the #$NSOpenGLPFANoRecovery
			 ;;--- but the {} reader barfs when I try that.
                         #-cocotron {#$NSOpenGLPFAColorSize 32 
                                     #$NSOpenGLPFADoubleBuffer 
                                     #$NSOpenGLPFADepthSize 32
			             #$NSOpenGLPFANoRecovery
                                     0}
                         #+cocotron {#$NSOpenGLPFAColorSize 32 
                                     #$NSOpenGLPFADoubleBuffer 
                                     #$NSOpenGLPFADepthSize 32
                                     0})))
      (unless Pixel-Format (error "Bad OpenGL pixelformat"))
      (setf (native-view Self) (make-instance 'native-transparent-opengl-view
                                 :with-frame (#/frame (#/contentView (native-window Self)))
                                 :pixel-format Pixel-Format
                                 :lui-window Self))
      ;; sharing  OpenGL context?
      (let ((View-to-Share (or (and (use-global-glcontext Self) (shared-opengl-view))
                                 (share-glcontext-of Self))))
          (when View-to-Share
            (let ((glContext 
                   (#/initWithFormat:shareContext: 
                    (#/openGLContext (native-view Self))
                    Pixel-Format ;; redundant but should be OK
                    (#/openGLContext (native-view View-to-Share)))))
              ;; (format t "~%before ~A ~%after ~A" (#/openGLContext Native-Control) glContext)))
              (unless glContext (error "cannot share OpenGLContext of view ~A" View-to-Share)))))
      (#/release Pixel-Format)
      ;; make surface of OpenGL view transparent
      (#/setValues:forParameter: (#/openGLContext (native-view Self)) {0} #$NSOpenGLCPSurfaceOpacity)
      (#/setContentView: (native-window Self) (native-view Self))
      ;; UGLY: the disable/enable is prevents a flicker showing the background briefly. Not clear why needed
      (unwind-protect 
          (progn
            (#/disableFlushWindow (native-window Self))
            ;#- cocotron 
            (#/makeKeyAndOrderFront: (native-window Self) nil)
            (#/flushWindow (native-window Self))
            )
        (#/enableFlushWindow (native-window Self))))))


(defmethod DRAW-RECT ((Self transparent-opengl-window))
  ;; may need to specialized
  (with-glcontext Self
    (clear-background Self)
    (draw Self)))


(defmethod SET-POSITION ((Self transparent-opengl-window) X Y)
  (setf (x Self) X)
  (setf (y Self) Y)
  (ns:with-ns-size (Position x (- (truncate (pref (#/frame (or #| (#/screen (native-window Self)) |#
                                                               (#/mainScreen ns:ns-screen))) 
                                                  <NSR>ect.size.height))  y))
    (#/setFrameTopLeftPoint: (native-window Self) Position)))



(defmethod DRAW ((Self transparent-opengl-window))
  (when (display-function Self)
    (funcall (display-function Self))))


(defmethod PREPARE-OPENGL ((Self transparent-opengl-window))
  )


(defmethod CLEAR-BACKGROUND ((Self transparent-opengl-window))
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClear #.(logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT)))


(defmethod DISPLAY ((Self transparent-opengl-window))
  (with-glcontext Self
    (clear-background Self)
    (draw Self)))


(defmethod WINDOW-CLOSE ((Self transparent-opengl-window))
  (#/close (native-window Self)))


(defmethod WINDOW-WILL-CLOSE ((Self transparent-opengl-window) Notification)
  (declare (ignore Notification))
  ;;do nothing
)


#| Examples:


(defun EXAMPLE-DISPLAY-FUNCTION ()
  ;; what is to be drawn into the window and the drag and drop proxy
  ;; the obligatory OpenGL RGB triangle
  (glBegin GL_TRIANGLES)
  (glColor3f 1.0 0.0 0.0)
  (glVertex3f 0.0 0.6 0.0)
  (glColor3f 0.0 1.0 0.0)
  (glVertex3f -0.2 -0.3 0.0) 
  (glColor3f 0.0 0.0 1.0)
  (glVertex3f 0.2 -0.3 0.0)
  (glEnd))


;; Drag and Drop


(defparameter *DND-Window* nil)


(defclass OPENGL-DRAG-and-DROP-VIEW (opengl-view)
  ((display-function :accessor display-function :initform nil :initarg :display-function :documentation "0 parameter function using OpenGL calls to display content")))


(defmethod DRAW ((Self opengl-drag-and-drop-view))
  (funcall (display-function Self)))


(defmethod VIEW-LEFT-MOUSE-DRAGGED-EVENT-HANDLER ((Self opengl-drag-and-drop-view) X Y DX DY)
  (declare (ignore DX DY))
  (unless *DND-WINDOW*
    ;; same size as view containing opengl object to be dragged
    (setq *Dnd-Window* (make-instance 'transparent-opengl-window :width 340 :height 180 :display-function #'example-display-function)))
  (let ((Screen-x (+ x (x Self) (x (window Self))))
        (Screen-y (+ y (y Self) (y (window Self)))))
    (set-position
     *Dnd-Window*
     (- Screen-x (truncate (width *Dnd-Window*) 2))
     (- Screen-y (truncate (height *Dnd-Window*) 2)))
    (let ((Target-View (find-view-at-screen-position Screen-x Screen-y)))
      (setf (text *text*) (or (write-to-string Target-View)
                              "-")))))


(defmethod VIEW-LEFT-MOUSE-UP-EVENT-HANDLER ((Self opengl-drag-and-drop-view) X Y)
  (declare (ignore x y))
  (when *DND-WINDOW*
    (window-close *DND-WINDOW*)
    (setf *DND-WINDOW* nil)))


(defparameter *WI* (make-instance 'window :width 340 :height 360))

(add-subview *WI* (make-instance 'opengl-drag-and-drop-view :width 340 :height 180 :display-function #'example-display-function))

(defparameter *Text* (make-instance 'editable-text-control :y 180 :width 340 :height 180))

(add-subview *WI* *text*)

(find-view-containing-point *WI* 5 5)
(find-view-containing-point *WI* 5 181)
(find-view-containing-point *WI* 5 2000)

|#

