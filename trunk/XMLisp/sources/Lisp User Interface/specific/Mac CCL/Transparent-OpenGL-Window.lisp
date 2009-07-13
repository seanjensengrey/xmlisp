;;;-*- Mode: Lisp; Package: ccl -*-
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
   ;* SW/HW     : PowerPC G4, OS X 10.5.6, CCL 1.2                   *
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
  (#_NSRectFill (#/bounds Self)))


(objc:defmethod (#/drawRect: :void) ((Self native-transparent-opengl-view) (rect :<NSR>ect))
  (let ((Window (lui-window Self)))
    (with-glcontext Window
      (clear-background Window)
      (draw Window))))

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
   (x :accessor x :initform 0 :initarg :x :documentation "screen position, pixels")
   (y :accessor y :initform 0 :initarg :y :documentation "screen position, pixels")
   (width :accessor width :initform 170 :initarg :width :documentation "width in pixels")
   (height :accessor height :initform 90 :initarg :height :documentation "height in pixels")
   (alpha :accessor alpha :initform 0.4d0 :initarg :alpha :documentation "alpha value: 0=transparent..1=opaque"))
  (:documentation "A transparent window is used as feedback for drag and drop "))


(defmethod INITIALIZE-INSTANCE ((Self transparent-opengl-window) &rest Initargs)
  (declare (ignore Initargs))
  (call-next-method)
  (ns:with-ns-rect (Frame (x Self) (y Self) (width Self) (height Self))
    (setf (native-window Self) (make-instance 'ns:ns-window
                                 :with-content-rect Frame
                                 :style-mask #$NSBorderlessWindowMask
                                 :backing #$NSBackingStoreBuffered
                                 :defer #$NO))
      (#/setAlphaValue: (native-window Self) (gui::cgfloat 0.5))
      (#/setBackgroundColor: (native-window Self) (#/clearColor ns:ns-color))
      (#/setOpaque: (native-window Self) #$NO)
    (let ((Pixel-Format (#/initWithAttributes: 
                         (#/alloc ns:NS-OpenGL-Pixel-Format)
                         {#$NSOpenGLPFAColorSize 32 
                         #$NSOpenGLPFADoubleBuffer 
                         #$NSOpenGLPFADepthSize 32
                         #$NSOpenGLPFANoRecovery
                         0})))
      (unless Pixel-Format (error "Bad OpenGL pixelformat"))
      (setf (native-view Self) (make-instance 'native-transparent-opengl-view
                                 :with-frame (#/frame (#/contentView (native-window Self)))
                                 :pixel-format Pixel-Format
                                 :lui-window Self))
      (#/release Pixel-Format)
      ;; make surface of OpenGL view transparent
      (#/setValues:forParameter: (#/openGLContext (native-view Self)) {0} #$NSOpenGLCPSurfaceOpacity)
      ;; (#/setContentView: (native-window Self) (native-view Self))
      ;; UGLY: the disable/enable is prevents a flicker showing the background briefly. Not clear why needed
      (unwind-protect 
          (progn
            (#/disableFlushWindow (native-window Self))
            (#/makeKeyAndOrderFront: (native-window Self) nil)
            (#/flushWindow (native-window Self))
            )
        (#/enableFlushWindow (native-window Self))))))


(defmethod DRAW ((Self transparent-opengl-window))
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  ;; the obligatory OpenGL RGB triangle
  (glBegin GL_TRIANGLES)
  (glColor3f 1.0 0.0 0.0)
  (glVertex3f 0.0 0.6 0.0)
  (glColor3f 0.0 1.0 0.0)
  (glVertex3f -0.2 -0.3 0.0) 
  (glColor3f 0.0 0.0 1.0)
  (glVertex3f 0.2 -0.3 0.0)
  (glEnd))


(defmethod INIT ((Self transparent-opengl-window))
  (print "init"))


(defmethod CLEAR-BACKGROUND ((Self transparent-opengl-window))
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClear #.(logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT)))


(defmethod DISPLAY ((Self transparent-opengl-window))
  (with-glcontext Self
    (clear-background Self)
    (draw Self)))

#| Examples:


(defparameter *TOGL-Window* (make-instance 'transparent-opengl-window :x 300))

(defparameter *TOGL-Window* (make-instance 'transparent-opengl-window :x 10 :width 500 :height 500))

(display *TOGL-Window*)


(#/orderFront: (native-window *TOGL-Window*) nil)

(#/orderOut: (native-window *TOGL-Window*) nil)

|#

