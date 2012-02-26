;;-*- Mode: Lisp; Package: xlui -*-
;*********************************************************************
;*                                                                   *
;*                           S H A P E S                             *
;*                                                                   *
;*********************************************************************
;* Author       : Alexander Repenning, alexander@agentsheets.com     *
;*                Andri Ioannidou (andri@agentsheets.com)            *
;*                http://www.agentsheets.com                         *
;* Copyright    : (c) 1996-2010, AgentSheets Inc.                    *
;* Filename     : Shapes.lisp                                        * 
;* Last Update  : 03/09/10                                           *
;* Version      :                                                    *
;*    1.0       : 03/09/10                                           *
;* Systems      : Intel-Mac, OS X 10.6.2, CCL 1.4                    *
;* Abstract     : Introducing shape class                            *
;*                                                                   *
;*********************************************************************

(in-package :xlui)


(defparameter *FLAT-SHAPE-Z-OFFSET* 0.02 "Default dept of tile")


(defclass SHAPE (agent-3d)
  ((depth :accessor depth :initform 1.0 :type float :initarg :depth)
   (turn-height :accessor turn-height :initform 0.0 :initarg :turn-height :type single-float :documentation "height around which shapes will be turned. Default to floor")
   (textures :accessor textures :initform (make-hash-table :test #'equal))
   (texture-path :accessor texture-path :initform nil :initarg :texture-path :documentation "if this value is set the openGLView will look for texture here instead of the default location")
   (texture :accessor texture :initform nil :initarg :texture :documentation "texture file name")))


;_______________________________________
;        Specification                  |
;_______________________________________

(defgeneric ICON (Shape)
  (:documentation "Name of image used as icon in menus to represent shape"))


(defgeneric DRAW-AS-PROXY-ICON (Shape)
  (:documentation "display a  proxy icon at <0, 0, 0>. Used typically by project manger and visual programming language to refer to shape. A proxy icon can be a flat rendering of the shape or a miniature/simplified version of the shape itsel."))


(defgeneric PROXY-ICON-SCALE (Shape)
  (:documentation "the scale of the shape proxy icon"))

;_______________________________________
;        Implementation                 |
;_______________________________________

(defmethod PRINT-SLOTS ((Self shape))
  '())


(defmethod USE-TEXTURE ((Self shape) Texture-Name)
  (unless Texture-Name (return-from use-texture)) 
  (let ((Texture-Id (gethash Texture-Name (textures Self))))
    ;; load texture from file if necessary
    (unless Texture-Id
      (setq Texture-Id (create-texture-from-file (texture-file Self Texture-Name) :verbose nil))
      (setf (gethash Texture-Name (textures Self)) Texture-Id))
    ;; make the texture active (for now assume a 2D texture
    (glBindTexture GL_TEXTURE_2D Texture-Id)))


(defmethod TEXTURE-FILE ((Self t) Texture-Name)
  (declare (ignore texture-name))
  nil)


(defmethod TEXTURE-FILE ((Self Shape) Texture-Name)
  ;; if part of a shape-manager, get texture-file of that
  (if (texture-path self)
    (native-path (texture-path self) texture-name)
    (or (texture-file (part-of Self) Texture-Name)
        (native-path "lui:resources;textures;" Texture-Name))))


;; Proxy Icon Methods

(defmethod DRAW-PROXY-ICON-BACKGROUND ((Self shape))
  (glBegin GL_QUADS)
  (glColor3f 1.0 1.0 1.0)
  (glVertex2i 0 0)
  (glVertex2i 0 1)
  (glVertex2i 1 1)
  (glVertex2i 1 0)
  (glEnd))
  

(defmethod PROXY-ICON-SIZE ((Self shape))
  0.15)


(defmethod DRAW-AS-PROXY-ICON ((Self shape)) 
  ;; draw shape as a square with flat texture
  (let ((Height (proxy-icon-size Self))
        (Width (proxy-icon-size Self)))
    (glEnable GL_TEXTURE_2D) 
    (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
    (use-texture Self (icon self))
    (glPushMatrix)
    (glTranslatef 0.0 0.0 0.01)  ;; sligthly towards the viewer
    (glBegin GL_QUADS)
    (glnormal3f 0.0 0.0 1.0)
    (glTexCoord2i 0 0) (glVertex2f 0.0 0.0)
    (glTexCoord2i 0 1) (glVertex2f 0.0 Height)
    (glTexCoord2i 1 1) (glVertex2f Width Height)
    (glTexCoord2i 1 0) (glVertex2f Width 0.0)
    (glEnd)
    (glPopMatrix))
  )


(defmethod PROXY-ICON-SCALE ((Self shape))
  0.15)


(defmethod (SETF texture) :before (value (self shape))
  "Setting a texture of a shape will purge currently loaded textures"
  (declare (ignore value))
  (when (view self)
    (maphash 
     #'(lambda (key value) 
         (declare (ignore key))
         (with-glcontext (view self) 
           (ccl::rlet ((&texName :long))
             (setf (ccl::%get-long &texName) value)
             (glDeleteTextures 1 &texName))))
     (textures self)) 
    (clrhash (textures self))))

;;Should already have an glContext before calling this method
(defmethod CLEAN-UP-TEXTURES ((self shape))
  (maphash 
   #'(lambda (key value) 
       (declare (ignore key))
       (ccl::rlet ((&texName :long))
         (setf (ccl::%get-long &texName) value)
         (glDeleteTextures 1 &texName)))
   (textures self))
  (clrhash (textures self)))


;****************************************
;  Main shape subclasses                *
;****************************************

;_______________________________________
;  Sphere                               |
;_______________________________________

(defclass SPHERE (shape)
  ((size :accessor size :initform 0.5d0 :initarg :size :type double-float :documentation "radius")
   (quadric :accessor quadric :initform nil)))


(defmethod INITIALIZE-QUADRIC ((Self sphere))
  ;; not a good idea to make a quadric if there is no window
  (setf (quadric Self) (gluNewQuadric))
  (gluQuadricDrawstyle (quadric Self) GLU_FILL)
  (gluQuadricOrientation (quadric Self) GLU_OUTSIDE)
  (gluQuadricNormals (quadric Self) GLU_SMOOTH)
  (gluQuadricTexture (quadric Self) GL_TRUE))


(defmethod PRINT-SLOTS ((Self sphere))
  `(x y z roll pitch heading size texture))


(defmethod DRAW ((Self sphere))
  (unless (is-visible Self) (return-from draw))
  (glTranslatef 0.5 0.5 (float (size Self) 0.0))
  ;; (glEnable GL_LIGHTING)
  (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
  (initialize-quadric Self)
  (cond
   ((texture Self) 
    (glEnable GL_TEXTURE_2D)
    (use-texture Self (texture Self)))
   (t 
    (glDisable gl_texture_2d)))
  (gluSphere (quadric Self) (size Self) 20 20)
  ;; to make the code sharable for drag and drop we need to do the less efficial way
  ;; reinitializing the quadric every time we draw. Overhead is <= 10%
  (gluDeleteQuadric (quadric Self))
  (setf (quadric Self) nil)
  (call-next-method))


(defmethod DRAW-BOUNDING-BOX ((Self sphere) Red Green Blue &optional Alpha)
  (declare (ignore Red Green Blue Alpha))
  (glPushMatrix)
  (let ((Offset (float (- (size Self)) 0.0)))
    (glTranslatef Offset Offset Offset))
  (call-next-method)
  (glPopMatrix))


(defmethod DEPTH ((Self sphere))
  (float (* 2 (size Self)) 0.0))


(defmethod TURN-HEIGHT ((Self sphere))
  ;; if not specified use radius
  (float (size Self) 0.0))


(defmethod ICON ((Self Sphere))
  (texture Self))


;_______________________________________
;  Sky Dome                             |
;_______________________________________

(defclass SKY-DOME (sphere)
  ()
  (:default-initargs 
    :texture "skyIsland.png")
  (:documentation "A huge sphere containing the world"))


(defmethod PRINT-SLOTS ((Self sky-dome))
  `(x y z roll pitch heading size texture))


(defmethod INITIALIZE-INSTANCE ((Self sky-dome) &rest Initargs)
  (declare (ignore Initargs))
  ;; sky domes must be very LARGE
  (setf (size Self) 100d0)
  (call-next-method))


(defmethod SELECTION-PRIORITY ((Self sky-dome))
  -2)


(defmethod DRAW ((Self sky-dome))
  ;; no light, decal texture
  (unless (is-visible Self) (return-from draw))
  (glDisable GL_LIGHTING)
  (initialize-quadric Self)
  (glColor4f 1.0 1.0 1.0 1.0)
  (cond
   ((texture Self) 
    (glEnable GL_TEXTURE_2D)
    (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_DECAL)
    (use-texture Self (texture Self)))
   (t 
    (glDisable gl_texture_2d)))
  (gluSphere (quadric Self) (size Self) 20 20)
  ;; to make the code sharable for drag and drop we need to do the less efficial way
  ;; reinitializing the quadric every time we draw. Overhead is <= 10%
  (gluDeleteQuadric (quadric Self))
  (setf (quadric Self) nil)
  (glEnable GL_LIGHTING))

;_______________________________________
;  Cube                                 |
;_______________________________________

(defclass CUBE (shape)
  ((size :accessor size :initform 1.0 :type float))
  (:documentation "Cube agent"))


(defmethod PRINT-SLOTS ((Self cube))
  `(x y z roll pitch heading size texture))


(defmethod BOUNDING-BOX-WIDTH ((Self cube))
  (size Self))


(defmethod BOUNDING-BOX-HEIGHT ((Self cube))
  (size Self))


(defmethod BOUNDING-BOX-DEPTH ((Self cube))
  (size Self))


(defmethod DRAW ((Self cube))
  (call-next-method)
  (when (is-visible Self)
    (cond
     ((texture Self)
      (glenable gl_texture_2d)
      (gltexenvi gl_texture_env gl_texture_env_mode gl_modulate)
      (use-texture Self (texture Self)))
     (t
      (glDisable gl_texture_2d)))
    ;; slow immediate mode to render
    (glbegin gl_quads)
    (let ((s (size Self)))
      ;; front 
      (glnormal3f 0.0 0.0 1.0)
      (gltexcoord2f 0.0 0.0) (glvertex3f 0.0 0.0 s)
      (gltexcoord2f 0.0 1.0) (glvertex3f 0.0  s s)
      (gltexcoord2f 1.0 1.0) (glvertex3f  s  s s)
      (gltexcoord2f 1.0 0.0) (glvertex3f  s 0.0 s)
      ;; back
      (glnormal3f 0.0 0.0 -1.0)
      (gltexcoord2f 0.0 0.0) (glvertex3f 0.0 0.0 0.0)
      (gltexcoord2f 0.0 1.0) (glvertex3f 0.0  s 0.0)
      (gltexcoord2f 1.0 1.0) (glvertex3f  s  s 0.0)
      (gltexcoord2f 1.0 0.0) (glvertex3f  s 0.0 0.0)
      ;; right
      (glnormal3f  1.0 0.0 0.0)
      (gltexcoord2f 0.0 0.0) (glvertex3f s 0.0 0.0)
      (gltexcoord2f 0.0 1.0) (glvertex3f s 0.0  s)
      (gltexcoord2f 1.0 1.0) (glvertex3f s  s  s)
      (gltexcoord2f 1.0 0.0) (glvertex3f s  s 0.0)
      ;; left
      (glnormal3f -1.0 0.0 0.0)
      (gltexcoord2f 0.0 0.0) (glvertex3f 0.0 0.0 0.0)
      (gltexcoord2f 0.0 1.0) (glvertex3f 0.0 0.0  s)
      (gltexcoord2f 1.0 1.0) (glvertex3f 0.0  s  s)
      (gltexcoord2f 1.0 0.0) (glvertex3f 0.0  s 0.0)
      ;; top
      (glnormal3f 0.0 1.0 0.0)
      (gltexcoord2f 0.0 0.0) (glvertex3f 0.0  s 0.0)
      (gltexcoord2f 0.0 1.0) (glvertex3f 0.0  s s)
      (gltexcoord2f 1.0 1.0) (glvertex3f  s  s s)
      (gltexcoord2f 1.0 0.0) (glvertex3f  s  s 0.0)
      ;; bottom
      (glnormal3f 0.0 -1.0 0.0)
      (gltexcoord2f 0.0 0.0) (glvertex3f 0.0 0.0 0.0)
      (gltexcoord2f 0.0 1.0) (glvertex3f 0.0 0.0 s)
      (gltexcoord2f 1.0 1.0) (glvertex3f  s 0.0 s)
      (gltexcoord2f 1.0 0.0) (glvertex3f  s 0.0 0.0)
      (glend))))


(defmethod ICON ((Self Cube))
  (texture Self))

;_______________________________________
;  Box                                  |
;_______________________________________

(defclass BOX (shape)
  ((front-texture :accessor front-texture :initform nil :documentation "texture file name")
   (back-texture :accessor back-texture :initform nil :documentation "texture file name")
   (left-texture :accessor left-texture :initform nil :documentation "texture file name")
   (right-texture :accessor right-texture :initform nil :documentation "texture file name")
   (top-texture :accessor top-texture :initform nil :documentation "texture file name")
   (bottom-texture :accessor bottom-texture :initform nil :documentation "texture file name")
   (dx :accessor dx :initform 1.0 :initarg :dx)
   (dy :accessor dy :initform 1.0 :initarg :dy))
  (:default-initargs 
      :depth 1.0
    :turn-height 0.5)
  (:documentation "Box has six different textures"))


(defmethod PRINT-SLOTS ((Self box))
  '(x y z roll pitch heading dx dy texture front-texture back-texture left-texture right-texture top-texture bottom-texture))


(defmethod ICON ((Self box))
  (or (texture Self)
      (front-texture Self)))


(defmethod DRAW ((Self Box))
  (call-next-method)
  (when (is-visible Self)
    (cond
     ;; either a single texture or all of them 
     ((or (texture Self)
          (and (front-texture Self)
               (back-texture Self) 
               (left-texture Self) 
               (right-texture Self) 
               (top-texture Self)
               (bottom-texture Self)))
      (glenable gl_texture_2d)
      (gltexenvi gl_texture_env gl_texture_env_mode gl_modulate))
     (t
      (glDisable gl_texture_2d)))
    ;; slow immediate mode to render
    (let ((X (dx Self))
          (Y (dy Self))
          (Z (depth Self)))
      ;; front
      (when (or (front-texture Self) (texture Self))
        (use-texture Self (or (front-texture Self) (texture Self)))
        (glbegin gl_quads)
        (glnormal3f 0.0 1.0 0.0)
        (gltexcoord2f 0.0 0.0) (glvertex3f x y 0.0)
        (gltexcoord2f 0.0 1.0) (glvertex3f x y z)
        (gltexcoord2f 1.0 1.0) (glvertex3f 0.0 y z)
        (gltexcoord2f 1.0 0.0) (glvertex3f 0.0 y 0.0)
        (glEnd))
      ;; back
      (when (or (front-texture Self) (texture Self))
        (use-texture Self (or (back-texture Self) (texture Self)))
        (glbegin gl_quads)
        (glnormal3f 0.0 -1.0 0.0)
        (gltexcoord2f 0.0 0.0) (glvertex3f 0.0 0.0 0.0)
        (gltexcoord2f 0.0 1.0) (glvertex3f 0.0 0.0 z)
        (gltexcoord2f 1.0 1.0) (glvertex3f x 0.0 z)
        (gltexcoord2f 1.0 0.0) (glvertex3f x 0.0 0.0)
        (glEnd))
      ;; right
      (when (or (right-texture Self) (texture Self))
        (use-texture Self (or (right-texture Self) (texture Self)))
        (glbegin gl_quads)
        (glnormal3f  1.0 0.0 0.0)
        (gltexcoord2f 0.0 0.0) (glvertex3f x 0.0 0.0)
        (gltexcoord2f 0.0 1.0) (glvertex3f x 0.0  z)
        (gltexcoord2f 1.0 1.0) (glvertex3f x y z)
        (gltexcoord2f 1.0 0.0) (glvertex3f x y 0.0)
        (glEnd))
      ;; left
      (when (or (left-texture Self) (texture Self))
        (use-texture Self (or (left-texture Self) (texture Self)))
        (glbegin gl_quads)
        (glnormal3f -1.0 0.0 0.0)
        (gltexcoord2f 0.0 0.0) (glvertex3f 0.0 0.0 0.0)
        (gltexcoord2f 0.0 1.0) (glvertex3f 0.0 0.0  z)
        (gltexcoord2f 1.0 1.0) (glvertex3f 0.0 y z)
        (gltexcoord2f 1.0 0.0) (glvertex3f 0.0 y 0.0)
        (glEnd))
      ;; top
      (when (or (top-texture Self) (texture Self))
        (use-texture Self (or (top-texture Self) (texture Self)))
        (glbegin gl_quads)
        (glnormal3f 0.0 0.0 1.0)
        (gltexcoord2f 0.0 0.0) (glvertex3f 0.0 0.0 z)
        (gltexcoord2f 0.0 1.0) (glvertex3f 0.0 y z)
        (gltexcoord2f 1.0 1.0) (glvertex3f x y z)
        (gltexcoord2f 1.0 0.0) (glvertex3f x 0.0 z)
        (glEnd))
      ;; bottom
      (when (or (bottom-texture Self) (texture Self))
        (use-texture Self (or (bottom-texture Self) (texture Self)))
        (glbegin gl_quads)
        (glnormal3f 0.0 0.0 -1.0)
        (gltexcoord2f 0.0 0.0) (glvertex3f 0.0 0.0 0.0)
        (gltexcoord2f 0.0 1.0) (glvertex3f 0.0 y 0.0)
        (gltexcoord2f 1.0 1.0) (glvertex3f x y 0.0)
        (gltexcoord2f 1.0 0.0) (glvertex3f x 0.0 0.0)
        (glEnd)))))



;_______________________________________
;  Cylinder                             |
;_______________________________________

(defclass CYLINDER (shape)
  ((base-radius :accessor base-radius :initform 1.0d0 :type double-float :documentation "The radius of the cylinder at z = 0")
   (top-radius :accessor top-radius :initform 0.0d0 :type double-float :initarg :top-radius :documentation "The radius of the cylinder at z = depth")
   (quadric :accessor quadric :initform nil))
  (:default-initargs 
      :depth 1.0)
  (:documentation "Cylinder"))


(defmethod PRINT-SLOTS ((Self cylinder))
  `(x y z roll pitch heading base-radius top-radius depth texture))


(defmethod BOUNDING-BOX-WIDTH ((Self cylinder))
  (float (* 2 (base-radius Self)) 0.0))


(defmethod BOUNDING-BOX-HEIGHT ((Self cylinder))
  (float (* 2 (base-radius Self)) 0.0))


(defmethod BOUNDING-BOX-DEPTH ((Self cylinder))
  (depth Self))


(defmethod DRAW-BOUNDING-BOX ((Self cylinder) Red Green Blue &optional Alpha)
  (declare (ignore Red Green Blue Alpha))
  (glPushMatrix)
  (let ((Offset (float (- (base-radius Self)) 0.0)))
    (glTranslatef Offset Offset 0.0))
  (call-next-method)
  (glPopMatrix))


(defmethod DRAW ((Self cylinder))
  (unless (is-visible Self) (return-from draw))
  ;; setup
  (unless (quadric Self) 
    (setf (quadric Self) (gluNewQuadric))
    (gluQuadricOrientation (quadric Self) glu_outside)
    (gluQuadricNormals (quadric Self) glu_smooth)
    (gluQuadricTexture (quadric Self) gl_true)
    (gluQuadricDrawstyle (quadric Self) glu_fill))
  (call-next-method)
  ;; texture
  (cond
   ((texture Self)
    (glenable gl_texture_2d)
    (gltexenvi gl_texture_env gl_texture_env_mode gl_modulate)
    (use-texture Self (texture Self)))
   (t
    (glDisable gl_texture_2d)))
  ;; render cylinder
  (gluCylinder (quadric Self) (base-radius Self) (top-radius Self) (coerce (depth Self) 'double-float) 40 4)
  (gluDeleteQuadric (quadric Self))
  (setf (quadric Self) nil))

;_______________________________________
; Capped Cylinder                       |
;_______________________________________

(defclass CAPPED-CYLINDER (shape)
  ((base-radius :accessor base-radius :initform 0.5d0 :type double-float :documentation "The radius of the cylinder at z = 0")
   (top-radius :accessor top-radius :initform 0.5d0 :type double-float :initarg :top-radius :documentation "The radius of the cylinder at z = depth")
   (quadric :accessor quadric :initform nil))
  (:default-initargs 
      :depth 1.0)
  (:documentation "Cylinder"))


(defmethod PRINT-SLOTS ((Self capped-cylinder))
  `(x y z roll pitch heading base-radius top-radius depth texture))


(defmethod DRAW ((Self capped-cylinder))
  (unless (is-visible Self) (return-from draw))
  ;; setup
  (unless (quadric Self) 
    (setf (quadric Self) (gluNewQuadric))
    (gluQuadricOrientation (quadric Self) glu_outside)
    (gluQuadricNormals (quadric Self) glu_smooth)
    (gluQuadricTexture (quadric Self) gl_true)
    (gluQuadricDrawstyle (quadric Self) glu_fill))
  (call-next-method)
  ;; texture
  (cond
   ((texture Self)
    (glenable gl_texture_2d)
    (gltexenvi gl_texture_env gl_texture_env_mode gl_modulate)
    (use-texture Self (texture Self)))
   (t
    (glDisable gl_texture_2d)))
  (glPushMatrix)
  (glTranslatef 0.5 0.5 0.0)
  ;; render cylinder
  (gluCylinder (quadric Self) (base-radius Self) (top-radius Self) (coerce (depth Self) 'double-float) 40 4)
  (gluDisk (quadric Self) 0.0d0 (top-radius self) 40 2)
  
  (glTranslatef 0.0 0.0 (depth self))
  (gluDisk (quadric Self) 0.0d0 (top-radius self) 40 2)
  (glPopMatrix)
  (gluDeleteQuadric (quadric Self))
  (setf (quadric Self) nil))


(defmethod ICON ((Self capped-cylinder))
  (texture Self))
;_______________________________________
;  Disk                                 |
;_______________________________________

(defclass DISK (shape)
  ((inner-radius :accessor inner-radius :initform 0.0d0 :type double-float :documentation "The inner radius of the disk (defaults to zero)")
   (outer-radius :accessor outer-radius :initform 1.0d0 :type double-float :documentation "The outer radius of the disk")
   (quadric :accessor quadric :initform nil))
  (:documentation "Disk"))


(defmethod PRINT-SLOTS ((Self disk))
  `(x y z roll pitch heading inner-radius outer-radius texture))


(defmethod BOUNDING-BOX-WIDTH ((Self disk))
  (float (* 2 (outer-radius Self)) 0.0))


(defmethod BOUNDING-BOX-HEIGHT ((Self disk))
  (float (* 2 (outer-radius Self)) 0.0))


(defmethod BOUNDING-BOX-DEPTH ((Self disk))
  0.0)


(defmethod DRAW-BOUNDING-BOX ((Self disk) Red Green Blue &optional Alpha)
  (declare (ignore Red Green Blue Alpha))
  (glPushMatrix)
  (let ((Offset (float (- (outer-radius Self)) 0.0)))
    (glTranslatef Offset Offset 0.0))
  (call-next-method)
  (glPopMatrix))


(defmethod DRAW ((Self disk))
  (unless (is-visible Self) (return-from draw))
  ;; setup
  (unless (quadric Self) 
    (setf (quadric Self) (gluNewQuadric))
    (gluQuadricOrientation (quadric Self) glu_outside)
    (gluQuadricNormals (quadric Self) glu_smooth)
    (gluQuadricTexture (quadric Self) gl_true)
    (gluQuadricDrawstyle (quadric Self) glu_fill))
  (call-next-method)
  ;; texture
  (cond
   ((texture Self)
    (glenable gl_texture_2d)
    (gltexenvi gl_texture_env gl_texture_env_mode gl_modulate)
    (use-texture Self (texture Self)))
   (t
    (glDisable gl_texture_2d)))
  ;; render cylinder
  (gluDisk (quadric Self) (inner-radius Self) (outer-radius Self) 40 2)
  (gluDeleteQuadric (quadric Self))
  (setf (quadric Self) nil))


;_______________________________________
;  Tile                                 |
;_______________________________________

(defclass TILE (shape)
  ((width :accessor width :initform 1.0 :type float :initarg :width :documentation "width")
   (height :accessor height :initform 1.0 :type float :initarg :height :documentation "height"))
  (:default-initargs 
      :depth *FLAT-SHAPE-Z-OFFSET*)
  (:documentation "Tile"))


(defmethod BOUNDING-BOX-WIDTH ((Self tile))
  (width Self))


(defmethod BOUNDING-BOX-HEIGHT ((Self tile))
  (height Self))


(defmethod BOUNDING-BOX-DEPTH ((Self tile))
  (depth Self)) ;; was 0.2


(defmethod DRAW ((Self tile))
  (unless (is-visible Self) (return-from draw))
  (call-next-method)
  (cond
   ((texture Self)
    (glEnable GL_TEXTURE_2D)
    (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
    (use-texture Self (texture Self)))
   (t
    (glDisable GL_TEXTURE_2D)))
  ;; slow immediate mode to render
  (glPushMatrix)
  (glTranslatef 0.0 0.0 (depth self))
  (glBegin GL_QUADS)
  (glNormal3f 0.0 0.0 1.0)
  (glTexCoord2f 0.0 0.0) (glVertex2f 0.0 0.0)
  (glTexCoord2f 0.0 1.0) (glVertex2f 0.0 (height Self))
  (glTexCoord2f 1.0 1.0) (glVertex2f (width Self) (height Self))
  (glTexCoord2f 1.0 0.0) (glVertex2f (width Self) 0.0)
  (glEnd)
  (glPopMatrix))


(defmethod PRINT-SLOTS ((Self tile))
  `(texture))


(defmethod ICON ((Self Tile))
  (texture Self))

;__________________________
; Missing Shape Shape       |
;__________________________/


(defclass MISSING-SHAPE-PLACEHOLDER (tile)
  ()
  (:default-initargs :texture "missing_image.png")
  (:documentation "Used as placeholder in cases were a shape is missing. Use as singleton via the-missing-shape-placeholder"))


(defmethod RESOURCE-PATHNAME ((Self missing-shape-placeholder) File-Name &key Check Extension)
  (declare (ignore Check Extension))
  (pathname (format nil "lui:resources;textures;~A" File-Name)))


(defvar *The-Missing-Shape-Placeholder* nil "singleton of MISSING-SHAPE-PLACEHOLDER")


(defun THE-MISSING-SHAPE-PLACEHOLDER ()
  (or *The-Missing-Shape-Placeholder* 
      (setq *The-Missing-Shape-Placeholder* (make-instance 'Missing-Shape-Placeholder))))


(defvar *THE-MISSING-SHAPE-PLACEHOLDER-MANAGER* nil "singleton shape-manager for the MISSING-SHAPE-PLACEHOLDER")

(defun THE-MISSING-SHAPE-PLACEHOLDER-MANAGER ()
  (or *THE-MISSING-SHAPE-PLACEHOLDER-MANAGER*
      (setq *the-missing-shape-placeholder-Manager* (make-instance 'shape-manager :shape (the-missing-shape-placeholder)))))
