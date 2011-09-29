;;-*- Mode: Lisp; Package: :LUI -*- 
;*********************************************************************
;*                                                                   *
;*           O P E N G L   V I E W                                   *
;*                                                                   *
;*********************************************************************
;* Author       : Alexander Repenning, alexander@agentsheets.com     *
;*                http://www.agentsheets.com                         *
;* Copyright    : (c) 1996-2009, AgentSheets Inc.                    *
;* Filename     : opengl-view.lisp                                   *
;* Last Update  : 04/18/09                                           *
;* Version      :                                                    *
;*    1.0       : 04/18/09                                           *
;*    1.1       : 07/15/09 shared opengl contexts                    *
;* Systems      : G4, OS X 10.5.6                                    *
;* Lisps        : CLozure CL 1.3                                     *
;* Licence      : LGPL                                               *
;* Abstract     : OpenGL-view base class                             *
;*********************************************************************

(in-package :LUI)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (ccl:use-interface-dir :GL)
  #-windows-target (open-shared-library "/System/Library/Frameworks/OpenGL.framework/OpenGL")
  #+windows-target (open-shared-library "opengl32.dll"))


(defclass OPENGL-VIEW (view)
  ((share-glcontext-of :accessor share-glcontext-of :initform nil :initarg :share-glcontext-of :documentation "share the glcontext of this view")
   (use-global-glcontext :accessor use-global-glcontext :initform t :initarg :use-global-glcontext :type boolean :documentation "set to t if you want to share the global glcontext")
   (current-font :accessor current-font :initform nil)
   (full-scene-anti-aliasing :accessor full-scene-anti-aliasing :initform t :type boolean :documentation "if true anti alias")
   (camera :accessor camera :initform nil :documentation "camera used")
   (textures :accessor textures :initform (make-hash-table :test #'equal) :allocation  :class  :documentation "table of loaded texture ids")
   (animation-time :accessor animation-time :initform 0 :documentation "Time when animation was run last")
   (animated-views :allocation :class :accessor animated-views :initform nil :documentation "class list of animated views")
   (render-mode :accessor render-mode :initform :render :type keyword :documentation "value: :render :select or :feedback")
   (camera-type :reader camera-type :initarg :camera-type :initform 'camera)
   (animation-stop-function :accessor animation-stop-function :initform nil :documentation "If there is a stop-function then the animation of this view will be finished, and the stop-function called with the view (in the animation thread)" ))
  (:documentation "OpenGL View"))


(defvar *Shared-OpenGL-View* nil "the GL context of this opengl-view can be share by all windows to share textures and display lists")


(defun SHARED-OPENGL-VIEW () "
  out: OpenGL-View.
  Return an shared opengl view which can be used to share textures, display list and other objects.
  This view is usually never visible and not attached to any window.
  An OpenGL-View will be automtically be created if needed."
  (or *Shared-OpenGL-View*
      (prog1
          (setf *Shared-OpenGL-View* (make-instance 'opengl-view :use-global-glcontext nil))
        ;; add the shared-opengl-view to an inivisible window, I am not quite sure why this is nescisary but if we do not do this on windows we will crash when we try to perform the wgl_share_list
        #+cocotron
        (let ((window (make-instance 'window :do-show-immediately nil)))
          (add-subview window *shared-opengl-view*)
          ))))


;**************************
;* interface              *
;**************************

(defgeneric CLEAR-BACKGROUND (opengl-view)
  (:documentation "Set background color and clear all necessary buffers (e.g., color, alpha, stencil, depth)"))

(defgeneric PREPARE-OPENGL (opengl-view)
  (:documentation "Called before drawing for the first time. No need to use with-glcontext"))

(defgeneric ANIMATE (opengl-view Time)
  (:documentation "Update content but do not draw/display. Time is in micro seconds. Use time to implement frame rate independent animation"))

(defgeneric IS-ANIMATED (opengl-view)
  (:documentation "True if view is animated"))

(defgeneric START-ANIMATION (opengl-view)
  (:documentation "Start the animation of the view"))

(defgeneric STOP-ANIMATION (opengl-view &key Stop-Function)
  (:documentation "Stop the animation of the view. If there is a <Stop-Function> call it AFTER the animation thread serviced the view for the last time, or, if the view is not animated call immediatly."))

(defgeneric ANIMATION-ABORTED (opengl-view)
  (:documentation "Called when animation code got aborted via (throw :animation-abort)"))

(defgeneric ANIMATION-CYCLE-ABORTED (opengl-view)
  (:documentation "Called when animation code got aborted via (throw :animation-cycle-abort). No other agent will be dispatched in same cycle"))

(defgeneric FRAME-RATE (opengl-view)
  (:documentation "How many frames per second"))

(defgeneric OBJECT-COORDINATE-VIEW-WIDTH (opengl-view)
  (:documentation "How wide does a flat object at z=0 need to be to fill the view horizonally based on current camera setting."))

(defgeneric OBJECT-COORDINATE-VIEW-HEIGHT (opengl-view)
  (:documentation "How high does a flat object at z=0 need to be to fill the view vertically based on current camera setting."))

(defgeneric MOUSE-ENTERED (opengl-view)
  (:documentation "Method that is called when the mouse enters this view."))

(defgeneric MOUSE-EXITED (opengl-view)
  (:documentation "Method that is called when the mouse exits this view."))

(defgeneric INITIALIZE-CAMERA (opengl-view)
  (:documentation "Initialize camera. Typically called as part of prepare-opengl"))

;**************************
;* default implementation *
;**************************

(defmethod INITIALIZE-INSTANCE :after ((Self opengl-view) &rest Args)
  (declare (ignore Args))
  (unless (camera Self)
    (setf (camera Self) (make-instance (camera-type self) :view Self))))


(defmethod CLEAR-BACKGROUND ((Self opengl-view))
  ;; don't do this for transparent windows
  (glClearColor 0.5 0.5 0.5 0.0)
  (glClear #.(logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT)))


(defmethod DRAW :before ((Self opengl-view))
  (when (camera Self) (draw (camera Self))))
 

(defmethod DRAW ((Self opengl-view))
  ;; the obligatory OpenGL RGB triangle
  (glBegin GL_TRIANGLES)
  (glColor3f 1.0 0.0 0.0)
  (glVertex3f 0.0 0.6 0.0)
  (glColor3f 0.0 1.0 0.0)
  (glVertex3f -0.2 -0.3 0.0) 
  (glColor3f 0.0 0.0 1.0)
  (glVertex3f 0.2 -0.3 0.0)
  (glEnd))


(defmethod PREPARE-OPENGL ((Self opengl-view))
  ;; nothing
  )


(defmethod IS-ANIMATED ((Self opengl-view))
  (member Self (animated-views Self)))


(defmethod ANIMATION-ABORTED ((Self opengl-view))
  ;; do nothing
  )


(defmethod ANIMATION-CYCLE-ABORTED ((Self opengl-view))
  ;; do nothing
  )


(defmethod VIEW ((Self opengl-view))
  ;; I am my own view
  Self)


(defmethod OBJECT-COORDINATE-VIEW-WIDTH ((Self opengl-view))
  (float (* (aspect (camera self)) (object-coordinate-view-height self)) 0.0))

      
(defmethod OBJECT-COORDINATE-VIEW-HEIGHT ((Self opengl-view))
  (float (* (tan (/ (* (/  (fovy (camera self)) 2) pi) 180)) (float (eye-z (camera self))) 2) 0.0))


            
(defmethod MOUSE-ENTERED ((Self opengl-view))
  ; do nothing
  )

(defmethod MOUSE-EXITED ((Self opengl-view))
  ; do nothing
  )


(defmethod INITIALIZE-CAMERA ((Self opengl-view))
  ; do nothing
  )


(defmethod VIEW-DID-END-RESIZE ((Self opengl-view))
  (enable-tooltips self)
  (map-subviews self #'(lambda (View) (view-did-end-resize View ))))


;_______________________________
; Events                        |
;_______________________________

(defmethod VIEW-LEFT-MOUSE-DOWN-EVENT-HANDLER ((Self opengl-view) X Y)
  (declare (ignore X Y))
  ;;(format t "~%down: ~A, ~A" x y)
  )


(defmethod VIEW-RIGHT-MOUSE-DOWN-EVENT-HANDLER ((Self opengl-view) X Y)
  (declare (ignore X Y))
  ;;(format t "~%down: ~A, ~A" x y)
  )


;_______________________________
; Textures                      |
;_______________________________


(defvar *Default-OpenGL-Texture-Magnification-Filter* GL_LINEAR "Used then loading textures from files")


(defun CREATE-TEXTURE-FROM-FILE (Filename &key Verbose (Build-Mipmaps t) Repeat (Mag-Filter *Default-OpenGL-Texture-Magnification-Filter*) Forced-Depth) "
  in:  Filename {string}, 
    &key Verbose {boolean}, Repeat
  out: OpenGL-Texture-Name {int}, width, height, depth
  Load the <Filename> texture inside the texture directory.
  - Ideal file should be 32 bit ARGB compatible, e.g., .png with mask or 24 bit RGB
 - 8 bit and 16 bit image will work too 
  - Image size must be 2^n x 2^m, at least 64 x 64
  - This function must be called with active AGL context, e.g., inside OpenGL-Window INIT method."
  (declare (ftype function create-image-from-file))
  (rlet ((&texName :long))
    (multiple-value-bind (&Image Width Height Depth) (create-image-from-file Filename :verbose Verbose :flip-vertical t :forced-depth Forced-Depth)
      (unless &Image (return-from create-texture-from-file nil))
      (glPixelStorei GL_UNPACK_ROW_LENGTH Width)  ; Set proper unpacking row length for image
      (glPixelStorei GL_UNPACK_ALIGNMENT 1)       ; Set byte aligned unpacking (needed for 3-byte-per-pixel image)
      (glGenTextures 1 &texName)
      ; Specify the texture's properties.
      (glBindTexture GL_TEXTURE_2D (%get-long &texName))
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (if Repeat GL_REPEAT GL_CLAMP_TO_EDGE))
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (if Repeat GL_REPEAT GL_CLAMP_TO_EDGE))
      (glTexParameteri gl_texture_2d gl_texture_mag_filter Mag-Filter)   ;; make textures look smooth (can be costly)
      ;; Mipmaps: make texture look good at different sizes
      (if Build-Mipmaps
        (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_NEAREST)
        (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR))
      ;; Experimental: anisotropic filtering to make textures less blurry at angle
      #-:cocotron  (glTexParameterf GL_TEXTURE_2D GL_TEXTURE_MAX_ANISOTROPY_EXT 16.0)
      (let ((PixelFormat (ecase Depth (32 GL_RGBA) (24 GL_RGB)))
            (InternalFormat (ecase Depth (32 GL_RGBA8) (24 GL_RGB8))))
        (glTexImage2D GL_TEXTURE_2D 0 InternalFormat width height 0 PixelFormat GL_UNSIGNED_BYTE &Image)
        (when Build-Mipmaps
          (when Verbose (format t "~%Building Mipmaps~%"))
          (unless 
              (zerop (gluBuild2DMipmaps GL_TEXTURE_2D InternalFormat width height PixelFormat GL_UNSIGNED_BYTE &Image))
            (error "could not create mipmaps"))
          (when Verbose (format t "Completed Mipmaps~%"))))
      ;; OpenGL should have copied now the image data into texture memory: release
      (dispose-vector &Image)
      ;; return the texture handle and image dimensions
      (values
       (%get-long &texName)
       Width 
       Height
       Depth))))


(defmethod TEXTURE-FILE ((Self opengl-view) Texture-Name)     
  (native-path "lui:resources;textures;" Texture-Name))


(defmethod USE-TEXTURE ((Self opengl-view) Texture-Name)
  (let ((Texture-Id (gethash Texture-Name (textures Self))))
    ;; load texture from file if necessary
    (unless Texture-Id
      (setq Texture-Id (create-texture-from-file (texture-file Self Texture-Name)))
      (setf (gethash Texture-Name (textures Self)) Texture-Id))
    ;; make the texture active (for now assume a 2D texture
    (when Texture-Id
      (glbindtexture gl_texture_2d Texture-Id))))


(defmethod USE-TEXTURE-AT-PATH ((Self opengl-view) Texture-Name Texture-Path)
  (let ((Texture-Id (gethash Texture-Name (textures Self))))
    ;; load texture from file if necessary
    (unless Texture-Id
      (setq Texture-Id (create-texture-from-file (native-path Texture-path Texture-Name)))
      (setf (gethash Texture-Name (textures Self)) Texture-Id))
    ;; make the texture active (for now assume a 2D texture
    (when Texture-Id
      (glbindtexture gl_texture_2d Texture-Id))))


(defmethod WINDOW-OF-VIEW-WILL-CLOSE ((Self opengl-view))
  (stop-animation self))

#| Examples:

(defparameter *WI* (make-instance 'window :width 340 :height 180))

(add-subview *WI* (make-instance 'opengl-view :width 340 :height 180))

|#