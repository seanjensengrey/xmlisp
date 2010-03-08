;;-*- Mode: Lisp; Package: AD3D -*-
;*********************************************************************
;*                                                                   *
;*            I N F L A T A B L E   I C O N                          *
;*                                                                   *
;*********************************************************************
;* Author       : Alexander Repenning, alexander@agentsheets.com     *
;*                http://www.agentsheets.com                         *
;* Copyright    : (c) 1996-2010, AgentSheets Inc.                    *
;* License:     : Protected by U.S. Patent No. 7,630,580             *
;* Filename     : inflatable-icon.lisp                               * 
;* Last Update  : 02/10/10                                           *
;* Version      :                                                    *
;*    1.0       : 02/17/04                                           *
;*    1.1       : 10/14/04 resource / xml based                      *
;*    1.1.1     : 05/10/05 print-slots                               *
;*    1.2       : 08/26/05 auto-compile                              *
;*    1.2.1     : 09/07/05 make-inflatable-icon-from-image-file      *
;*    1.2.2     : 09/08/05 check for random > 0                      *
;*    1.2.3     : 11/30/05 do not attempt to load image when no file *
;*    1.3       : 05/30/07 Inflate-Pixel-P-Fn parameter to inflate   *
;*    1.3.1     : 06/26/07 surfaces slot                             *
;*    1.3.2     : 08/16/07 preserve connectors                       *
;*    1.4       : 09/27/07 flat icons can be superfast               *
;*    1.4.1     : 10/12/07 *Minimal-Inflatable-Icon-Height*          *
;*    2.0       : 02/09/10 CCL Cocoa                                 *
;* Systems      : Intel-Mac, OS X 10.6.2, CCL 1.4                    *
;* Abstract     : 3D shapes derived from 2D icons/images             *
;*                                                                   *
;*********************************************************************

(in-package :xlui)


(defclass INFLATABLE-ICON (agent-3d)
  ((height :accessor height :initform 0s0 :initarg :height)
   (rows :accessor rows :initform 32 :initarg :rows)
   (columns :accessor columns :initform 32 :initarg :columns)
   (steps :accessor steps :initform 10)
   (pressure :accessor pressure :initform 0.0 :type short-float)
   (noise :accessor noise :initform 0s0)
   (max-value :accessor max-value :initform 1.0)
   (dx :accessor dx :initform 1.0 :initarg :dx :type short-float)
   (dy :accessor dy :initform 1.0 :initarg :dy :type short-float)
   (dz :accessor dz :initform 0.0 :initarg :dz :type short-float :documentation "z offset, can be used to antialias through edge snipping")
   (display-list :accessor display-list :initform nil)
   (is-compiled :accessor is-compiled :initform nil :type boolean)
   (auto-compile :accessor auto-compile :initform t :type boolean :initarg :auto-compile)
   (is-upright :accessor is-upright :initform nil :type boolean)
   (surfaces :accessor surfaces :initform 'front :type symbol :documentation "FRONT, FRONT AND BACK, ..")
   (icon :accessor icon :initform nil :initarg :icon :documentation "name of icon image")
   (image :accessor image :initform nil :initarg image :documentation "RGBA image")
   (noise-map :accessor noise-map :initform nil :documentation "a 2d map with noise values")
   (altitudes :accessor altitudes :type array)
   (distance :accessor distance :initform 0.0 :documentation "distance between mirrored surfaces")
   (connectors :accessor connectors :initform nil :documentation "connectors are polygons connecting inner and outer edges of the symetric sides")
   (visible-alpha-threshold :accessor visible-alpha-threshold :initform 64 :allocation :class :documentation "8 bit alpha value used as visible/invisible threshold")
   (texture-id :accessor texture-id :initform nil :documentation "OpenGL texture name")
   (is-flat :accessor is-flat :initform nil :type boolean :documentation "speed optimized rendering if flat")
   (update-texture-from-image-p :accessor update-texture-from-image-p :initform nil :documentation "set this flag to t to trigger texture update"))
  (:documentation "High polygon count 3D object made from inflated icon"))



;********************************************************
;* Specification                                        *
;********************************************************

(defgeneric PIXEL-VISIBLE-P (Inflatable-Icon Row Column)
  (:documentation "Return if pixel is visible and needs to be reprented with face. Take visible-alpha-threshold into account. Out of range row, columns return nil"))

(defgeneric COPY-CONTENT-INTO (Source-Inflatable-Icon Destination-Inflatable-Icon)
  (:documentation "Copy the content of the source inflatable icon, image, pressure, etc. into the destination inflable icon"))

(defgeneric COMPUTE-HEIGHT (Inflatable-Icon)
  (:documentation "Compute the height of the inflatable icon based on its content, inflation, and faces"))

(defgeneric UPDATE-TEXTURE-FROM-IMAGE (Inflatable-Icon &optional Image)
  (:documentation "Update the texture from the current image"))


;********************************************************
;* Implementation                                       *
;********************************************************

(defmethod PRINT-SLOTS ((Self inflatable-icon))
  `(icon rows columns height pressure steps noise max-value is-upright surfaces altitudes distance dz is-flat))


(defmethod FINISHED-READING :after ((Self inflatable-icon) Stream)
  (declare (ignore Stream))

  (print "finished-reading :after")

  ;; no point in storing connectors: generate them in the right cases
  (case (surfaces Self)
    (front-and-back-connected (compute-connectors Self)))
  ;; flat icons should not be compiled: will just create overhead with display lists
  (when (is-flat Self)
    (setf (auto-compile Self) nil)))


(defmethod MAKE-IMAGE-FROM-ICON ((Self inflatable-icon))
  ;; convert the icon into an RGBA image and initialize the altitude array
  (format t "~%~%TEXTURE-FILE: ~A~%~%" (texture-file (part-of Self) (icon Self)))
  
  (when (part-of Self)
    (when (texture-file (part-of Self) (icon Self))
      (setf (auto-compile Self) t)
      (multiple-value-bind (Image Columns Rows)
                           (create-image-from-file (texture-file (part-of Self) (icon Self)))
      (setf (image Self) Image)
      (setf (columns Self) Columns)
      (setf (rows Self) Rows)
      ;; don't overwrite existing altitudes
      (unless (altitudes Self)
        (setf (altitudes Self) (make-array (list Rows Columns) 
                                           :element-type 'short-float
                                           :initial-element 0.0)))))
  Self))





(defmethod OPEN-RESOURCE ((Self inflatable-icon))
  (call-next-method)
  (make-image-from-icon Self)
  #| use only altitudes without additional inflation
  (when (image Self)
    (inflate 
     Self
     :steps (steps Self)
     :pressure (pressure Self)
     :noise (noise Self)
     :max (max-value Self))) |# )


(defmethod COPY-CONTENT-INTO ((Source inflatable-icon) (Destination inflatable-icon))

 ;(inspect (image source))
  ;; check for size compatibility to avoid disaster
  
  
  (unless (and (= (rows Source) (rows Destination)) 
               (= (columns Source) (columns Destination))
               (= (sizeof (image Source) ) (sizeof (image source) ))
               )  
    (error "cannot copy content of source into destination inflatable icon: incompatible sizes"))
  
 ; (ccl:external-call "malloc_size" :address (image Source) :size_t)
  ;; given that they are the same size only copy content
  (setf (is-upright Destination) (is-upright Source))
  (setf (height Destination) (height Source))
  (setf (dz Destination) (dz Source))
  (setf (surfaces Destination) (surfaces Source))
  (setf (distance Destination) (distance Source))
  ;; arrays
  (noise-map Source)  ;; accessor makes array if needed
  (noise-map Destination)  ;; ;; accessor makes array if needed
  (dotimes (Row (rows Source))
    (dotimes (Column (columns Source))
      (setf (aref (noise-map Destination) Row Column) (aref (noise-map Source) Row Column))
      (setf (aref (altitudes Destination) Row Column) (aref (altitudes Source) Row Column))))
  (setf (connectors Destination) (mapcar #'copy-instance (connectors Source)))
  (setf (visible-alpha-threshold Destination) (visible-alpha-threshold Source))
  ;; copy Image: slow byte copy
  ;(dotimes (I (ccl:external-call "malloc_size" :address (image Source) :size_t))
  (dotimes (I (sizeof (image Source) ))
    (set-byte (image Destination) (lui::%get-byte (image Source) i) i))
  
  ;; flat texture optimization: do not copy texture-id -> destination should get its own texture id from OpenGL
  (setf (is-flat Destination) (is-flat Source))
  ;; do not compile flat textures: the display list overhead slows things down by about 2x
  (setf (auto-compile Destination) (not (is-flat Source)))
  ;; to make change visible we have to reset the compiled flag
  (setf (is-compiled Destination) nil))




#| still Carbon

(defmethod COPY-CONTENT-INTO ((Source inflatable-icon) (Destination inflatable-icon))
  ;; check for size compatibility to avoid disaster
  (unless (and (= (rows Source) (rows Destination)) 
               (= (columns Source) (columns Destination))
               (= (#_getPtrSize (image Source)) (#_getPtrSize (image Destination))))
    (error "cannot copy content of source into destination inflatable icon: incompatible sizes"))
  ;; given that they are the same size only copy content
  (setf (is-upright Destination) (is-upright Source))
  (setf (height Destination) (height Source))
  (setf (dz Destination) (dz Source))
  (setf (surfaces Destination) (surfaces Source))
  (setf (distance Destination) (distance Source))
  ;; arrays
  (noise-map Source)  ;; accessor makes array if needed
  (noise-map Destination)  ;; ;; accessor makes array if needed
  (dotimes (Row (rows Source))
    (dotimes (Column (columns Source))
      (setf (aref (noise-map Destination) Row Column) (aref (noise-map Source) Row Column))
      (setf (aref (altitudes Destination) Row Column) (aref (altitudes Source) Row Column))))
  (setf (connectors Destination) (mapcar #'copy-instance (connectors Source)))
  (setf (visible-alpha-threshold Destination) (visible-alpha-threshold Source))
  ;; copy Image: slow byte copy
  (dotimes (I (#_getPtrSize (image Source)))
    (%put-byte (image Destination) (%get-byte (image Source) i) i))
  ;; flat texture optimization: do not copy texture-id -> destination should get its own texture id from OpenGL
  (setf (is-flat Destination) (is-flat Source))
  ;; do not compile flat textures: the display list overhead slows things down by about 2x
  (setf (auto-compile Destination) (not (is-flat Source)))
  ;; to make change visible we have to reset the compiled flag
  (setf (is-compiled Destination) nil))

|#


(defmethod MAXIMUM-ALTITUDE ((Self inflatable-icon))
  (let ((Max 0.0))
    (dotimes (Row (rows Self) Max)
      (dotimes (Column (columns Self))
        (when (pixel-visible-p Self Row Column)
          (setq Max (max Max (aref (altitudes Self) Row Column))))))))


(defmethod MAX-VISIBLE-PIXEL-ROW ((Self inflatable-icon))
  (dotimes (Row (rows Self) (rows Self))
    (dotimes (Column (columns Self))
      (when (pixel-visible-p Self (- (rows Self) Row) Column) (return-from max-visible-pixel-row (- (rows Self) Row))))))
      

(defvar *Minimal-Inflatable-Icon-Height* 0.01 "> 0 to avoid having flattened icons pile up in 0 height stack")


(defmethod COMPUTE-HEIGHT ((Self inflatable-icon))
  ;; this can just be a heuristic: maybe ability for user to change
  (setf (height Self)
        (max *Minimal-Inflatable-Icon-Height*
             (cond
              ((is-upright Self)
               (float (/ (max-visible-pixel-row Self) (rows Self)) 0.0))
              (t (maximum-altitude Self))))))
#| need to take these into account

  (case (surfaces Self)
    ;; front
    (front 
     ;; front and back
    ((front-and-back front-and-back-connected)
    ;; CUBE
    (cube 

|#


(defmethod UPDATE-TEXTURE-FROM-IMAGE ((Self inflatable-icon) &optional Image)
  (with-glcontext (view Self)  ;; could be called from outside of draw
    (unless Image (setq Image (image Self)))
    ;; replace entire texture with image
    (cond
     ((and (texture-id Self) Image)
      (glBindTexture GL_TEXTURE_2D (texture-id Self))
      (glTexSubImage2D GL_TEXTURE_2D 0 0 0 (columns Self) (rows Self) GL_RGBA GL_UNSIGNED_BYTE Image)
      ;; need to update also the mipmaps to avoid mix of new and old image
      (gluBuild2DMipmaps GL_TEXTURE_2D 4 (columns Self) (rows Self) GL_RGBA GL_UNSIGNED_BYTE Image))
     (t
      (warn "inflatable icon is not ready for update")))))

;___________________________________
; Connectors                        |
;___________________________________

(defclass CONNECTOR ()
  ((x1 :accessor x1 :initarg :x1)
   (y1 :accessor y1 :initarg :y1)
   (z1 :accessor z1 :initarg :z1)
   (x2 :accessor x2 :initarg :x2)
   (y2 :accessor y2 :initarg :y2)
   (z2 :accessor z2 :initarg :z2)
   (x3 :accessor x3 :initarg :x3)
   (y3 :accessor y3 :initarg :y3)
   (z3 :accessor z3 :initarg :z3)
   (x4 :accessor x4 :initarg :x4)
   (y4 :accessor y4 :initarg :y4)
   (z4 :accessor z4 :initarg :z4)
   (normal-x :accessor normal-x :initarg :normal-x)
   (normal-y :accessor normal-y :initarg :normal-y)
   (red :accessor red :initarg :red)
   (green :accessor green :initarg :green)
   (blue :accessor blue :initarg :blue)
   (alpha :accessor alpha :initarg :alpha))
  (:documentation "a quad connecting the left with the right hand side of a symetrical inflatable icon"))


(defmethod COPY-INSTANCE ((Self connector))
  (make-instance 'connector
    :x1 (x1 Self)
    :y1 (y1 Self)
    :z1 (z1 Self)
    :x2 (x2 Self)
    :y2 (y2 Self)
    :z2 (z2 Self)
    :x3 (x3 Self)
    :y3 (y3 Self)
    :z3 (z3 Self)
    :x4 (x4 Self)
    :y4 (y4 Self)
    :z4 (z4 Self)
    :normal-x (normal-x Self)
    :normal-y (normal-y Self)
    :red (red Self)
    :green (green Self)
    :blue (blue Self)
    :alpha (alpha Self)))


(defmethod PIXEL-VISIBLE-P ((Self inflatable-icon) Row Column)
  (unless (array-in-bounds-p (altitudes Self) Row Column) (return-from pixel-visible-p nil))
  (>= (rgba-image-alpha (image Self) Column Row (columns Self)) (visible-alpha-threshold Self)))


(defmethod ADD-CONNECTOR ((Self inflatable-icon) Row Column Direction)
  (let* ((Dx (/ (dx Self) (columns Self)))
         (Dy (/ (dy Self) (rows Self)))
         (X (* Dx Column))
         (Y (* Dy Row)))
    (case Direction
      (:up 
       (let ((Z1 (altitude-at Self Row Column))
             (Z2 (altitude-at Self Row (1+ Column))))
         (push 
          (make-instance 'connector
            ;; vertices
            :x1 x :y1 y :z1 z1
            :x2 (+ x dx) :y2 y :z2 z2
            :x3 (+ x dx) :y3 y :z3 (* 2 (- 0.0 (distance Self) z2))
            :x4 x :y4 y :z4 (* 2 (- 0.0 (distance Self) z1))
            ;; normal
            :normal-x 0.0 :normal-y 1.0
            :red (rgba-image-red (image Self) Column Row (columns Self))
            :green (rgba-image-green (image Self) Column Row (columns Self))
            :blue (rgba-image-blue (image Self) Column Row (columns Self))
            :alpha (rgba-image-alpha (image Self) Column Row (columns Self)))
          (connectors Self))))
      (:down
       (let ((Z1 (altitude-at Self (1+ Row) Column))
             (Z2 (altitude-at Self (1+ Row) (1+ Column))))
         (push 
          (make-instance 'connector
            ;; vertices
            :x1 x :y1 (+ y dy) :z1 z1
            :x2 (+ x dx) :y2 (+ y dy) :z2 z2
            :x3 (+ x dx) :y3 (+ y dy) :z3 (* 2 (- 0.0 (distance Self) z2))
            :x4 x :y4 (+ y dy) :z4 (* 2 (- 0.0 (distance Self) z1))
            ;; normal
            :normal-x 0.0 :normal-y 1.0
            :red (rgba-image-red (image Self) Column Row (columns Self))
            :green (rgba-image-green (image Self) Column Row (columns Self))
            :blue (rgba-image-blue (image Self) Column Row (columns Self))
            :alpha (rgba-image-alpha (image Self) Column Row (columns Self)))
          (connectors Self))))
      (:left 
       (let ((Z1 (altitude-at Self Row Column))
             (Z2 (altitude-at Self (1+ Row) Column)))
         (push 
          (make-instance 'connector
            ;; vertices
            :x1 x :y1 y :z1 z1
            :x2 x :y2 (+ y dy) :z2 z2
            :x3 x :y3 (+ y dy) :z3 (* 2 (- 0.0 (distance Self) z2))
            :x4 x :y4 y :z4 (* 2 (- 0.0 (distance Self) z1))
            ;; normal
            :normal-x -1.0 :normal-y 0.0
            :red (rgba-image-red (image Self) Column Row (columns Self))
            :green (rgba-image-green (image Self) Column Row (columns Self))
            :blue (rgba-image-blue (image Self) Column Row (columns Self))
            :alpha (rgba-image-alpha (image Self) Column Row (columns Self)))
          (connectors Self))))
      (:right 
       (let ((Z1 (altitude-at Self Row (1+ Column)))
             (Z2 (altitude-at Self (1+ Row) (1+ Column))))
         (push 
          (make-instance 'connector
            ;; vertices
            :x1 (+ x dx) :y1 y :z1 z1
            :x2 (+ x dx) :y2 (+ y dy) :z2 z2
            :x3 (+ x dx) :y3 (+ y dy) :z3 (* 2 (- 0.0 (distance Self) z2))
            :x4 (+ x dx) :y4 y :z4 (* 2 (- 0.0 (distance Self) z1))
            ;; normal
            :normal-x 1.0 :normal-y 0.0
            :red (rgba-image-red (image Self) Column Row (columns Self))
            :green (rgba-image-green (image Self) Column Row (columns Self))
            :blue (rgba-image-blue (image Self) Column Row (columns Self))
            :alpha (rgba-image-alpha (image Self) Column Row (columns Self)))
          (connectors Self)))))))


(defmethod COMPUTE-CONNECTORS ((Self inflatable-icon))
  (unless (image Self) (return-from compute-connectors))
  (setf (connectors Self) nil)
  ;; scan vertically 
  (dotimes (Column (columns Self))
    (dotimes (Row (rows Self))
      ;; I am visible
      (when (pixel-visible-p Self Row Column)
        (unless (pixel-visible-p Self (1- Row) Column) (add-connector Self Row Column :up))
        (unless (pixel-visible-p Self (1+ Row) Column) (add-connector Self Row Column :down))
        (unless (pixel-visible-p Self Row (1- Column)) (add-connector Self Row Column :left))
        (unless (pixel-visible-p Self Row (1+ Column)) (add-connector Self Row Column :right))))))


(defmethod (SETF DISTANCE) :after (Value (Self inflatable-icon))
  (declare (ignore Value))
  ;;(format t "~%distance=~A" Value)
  (compute-connectors Self)
  ;;(print (length (connectors Self)))
  )

;___________________________________
; DRAW                              |
;___________________________________
    

(defmethod COMPILE-SHAPE ((Self inflatable-icon))
  ;; if icon was compiled before free list
  (when (display-list Self) (gldeletelists (display-list Self) 1))
  ;; new list
  (setf (display-list Self) (glGenLists 1))
  (glNewList (display-list Self) gl_compile)
  (draw-uncompiled Self)
  (glEndList)
  (setf (is-compiled Self) t))


(defmethod UNCOMPILE-SHAPE ((Self inflatable-icon))
  (setf (is-compiled Self) nil))


(defmethod DRAW-CONNECTORS ((Self inflatable-icon))
  ;; current: random small quads -> super infefficient way to draw the connectors
  ;; better: sort and make quad strips
  ;; best: run-lenght join quads: same color, connected, planar quads into big quads
  (dolist (Connector (connectors Self))
    (glBegin gl_quads)
    (glcolor4ub (red Connector) (green Connector) (blue Connector) (alpha Connector))
    (glNormal3f (normal-x Connector) (normal-y Connector) 0.0)
    (glVertex3f (x1 Connector) (y1 Connector) (z1 Connector))
    (glVertex3f (x2 Connector) (y2 Connector) (z2 Connector))
    (glVertex3f (x3 Connector) (y3 Connector) (z3 Connector))
    (glVertex3f (x4 Connector) (y4 Connector) (z4 Connector))
    (glEnd))
  (glColor3f 1.0 1.0 1.0))
    


(defmethod DRAW ((Self inflatable-icon))
  ;; the texture update needs to happen in the right opengl context
  ;; just in time while display is not elegant but works
  (when (update-texture-from-image-p Self)
    (update-texture-from-image Self)
    (setf (update-texture-from-image-p Self) nil))
  (glpushmatrix)
  (glTranslatef 0s0 0s0 (dz Self))  ;; keep from interfering with background
  (when (is-upright Self)
    (glRotatef +90s0 1.0s0 0.0s0 0.0s0)
    (glTranslatef (/ (- 1s0 (dx Self)) 2s0) +0.0s0 -0.5s0))
  ;; autocompile
  (when (and (auto-compile Self) (not (is-compiled Self)))
    (compile-shape Self))
  ;; display surfaces
  (ecase (surfaces Self)
    ;; front
    (front 
     (glpushmatrix)
     (glTranslatef 0s0 0s0 (distance Self))
     (if (is-compiled Self) (draw-compiled Self) (draw-uncompiled Self))
     (glpopmatrix))
    ;; front and back
    (front-and-back
     (glpushmatrix)
     (glTranslatef 0s0 0s0 (distance Self))
     (if (is-compiled Self) (draw-compiled Self) (draw-uncompiled Self))
     (glpopmatrix)
     ;; back
     (glpushmatrix)
     (glscalef 1s0 1s0 -1s0)
     (glTranslatef 0s0 0s0 (distance Self))
     (if (is-compiled Self) (draw-compiled Self) (draw-uncompiled Self))
     (glpopmatrix))
    (front-and-back-connected
     (glpushmatrix)
     (glTranslatef 0s0 0s0 (distance Self))
     (if (is-compiled Self) (draw-compiled Self) (draw-uncompiled Self))
     ;; connectors
     (if (connectors Self) (draw-connectors Self))
     (glpopmatrix)
     ;; back
     (glpushmatrix)
     (glscalef 1s0 1s0 -1s0)
     (glTranslatef 0s0 0s0 (distance Self))
     (if (is-compiled Self) (draw-compiled Self) (draw-uncompiled Self))
     (glpopmatrix))
    ;; CUBE
    (cube 
     (glpushmatrix)
     (glTranslatef 0.0 0.0 (distance Self))
     (if (is-compiled Self) (draw-compiled Self) (draw-uncompiled Self))
     (glpopmatrix)
     
     (glpushmatrix)
     (glTranslatef (+ 0.5 (distance Self)) 0.0 0.5)
     (glRotatef 90.0 0.0 1.0 0.0)
     (if (is-compiled Self) (draw-compiled Self) (draw-uncompiled Self))
     (glpopmatrix)
     
     (glpushmatrix)
     (glTranslatef (- 0.5 (distance Self)) 0.0 0.5)
     (glscalef -1.0 1.0 1.0)
     (glRotatef 90.0 0.0 1.0 0.0)
     (if (is-compiled Self) (draw-compiled Self) (draw-uncompiled Self))
     (glpopmatrix)
     
     ;; top
     (glpushmatrix)
     (glTranslatef 0.0 (+ 0.5 (distance Self)) 0.5)
     (glRotatef -90.0 1.0 0.0 0.0)
     (if (is-compiled Self) (draw-compiled Self) (draw-uncompiled Self))
     (glpopmatrix)
     
     (glpushmatrix)
     (glscalef 1.0 1.0 -1.0)
     (glTranslatef 0s0 0s0 (distance Self))
     (if (is-compiled Self) (draw-compiled Self) (draw-uncompiled Self))
     (glpopmatrix)))
 (glpopmatrix))


(defmethod ALTITUDE-AT ((Self inflatable-icon) Row Column)
  (if (array-in-bounds-p (altitudes Self) Row Column)
    (or (aref (altitudes Self) Row Column) 0.0s0)
    0.0s0))


(defmethod NOISE-MAP :around ((Self inflatable-icon))
  (call-next-method)
  ;; JIT accessor
  (unless (slot-value Self 'noise-map)
    (setf (slot-value Self 'noise-map)
          (make-array (list (rows Self) (columns Self))))
    (dotimes (Row (rows Self))
      (dotimes (Column (columns Self))
        (setf (aref (slot-value Self 'noise-map) Row Column) (- (random 2.0) 1.0)))))
  (slot-value Self 'noise-map))


(defmethod NORMAL-AT ((Self inflatable-icon) Row Column 2M)
  (glnormal3f
   (- (altitude-at Self Row (1- Column)) (altitude-at Self Row (1+ Column)))
   (- (altitude-at Self (1- Row) Column) (altitude-at Self (1+ Row) Column))
   2m))


(defmethod DRAW-COMPILED ((Self inflatable-icon))
  (glCallList (display-list Self)))


;; HACK!! move this into LUI Cocoa
(defmethod DRAW-AS-FLAT-TEXTURE ((Self inflatable-icon))

  (print "drawing as flat texture")


  (unless (texture-id Self)
    ;; use image as texture
    (unless (image Self) (error "image of inflatable icon is undefined"))

    (print (sizeof (image self)))
    (print (image Self))

    (unless (= (sizeof (image Self)) (* (rows Self) (columns Self) 4)) (error "image size does not match row/column size"))
    (ccl::rlet ((&texName :long))
      (glGenTextures 1 &texName)
      (setf (texture-id Self) (ccl::%get-long &texName)))
    (glBindTexture GL_TEXTURE_2D (texture-id Self))
    ;; clam to edge to avoid gaps between adjacent textures
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
    ;; linear pixel magnification
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_NEAREST)
    (glTexImage2D GL_TEXTURE_2D 0 4 (columns Self) (rows Self) 0 GL_RGBA GL_UNSIGNED_BYTE (image Self))
    (unless (zerop (gluBuild2DMipmaps GL_TEXTURE_2D 4 (columns Self) (rows Self) GL_RGBA GL_UNSIGNED_BYTE (image Self)))
            (error "could not create mipmaps")))
  (glEnable GL_TEXTURE_2D)
  (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
  (glBindTexture GL_TEXTURE_2D (texture-id Self))
  (glBegin GL_QUADS)
  (glNormal3f 0.0 0.0 1.0)
  (glTexCoord2i 0 0) (glVertex2f 0.0 0.0)
  (glTexCoord2i 1 0) (glVertex2f 1.0 0.0)
  (glTexCoord2i 1 1) (glVertex2f 1.0 1.0)
  (glTexCoord2i 0 1) (glVertex2f 0.0 1.0)
  (glEnd))


(defmethod DRAW-UNCOMPILED ((Self inflatable-icon)) 
  (unless (image Self) (return-from draw-uncompiled))
  (when (is-flat Self)  ;; optimization
    (draw-as-flat-texture Self)
    (return-from draw-uncompiled))
  (let* ((X 0s0)
         (Dx (/ (dx Self) (columns Self)))
         (Y 0s0)
         (Dy (/ (dy Self) (rows Self)))
         (2M (+ dx dx))
         (Image (image Self))
         (Old-Color-Red nil)
         (Old-Color-Green nil)
         (Old-Color-Blue nil)
         (Old-Color-Alpha nil)
         (Face-Number 0))
    (glDisable gl_texture_2d)
    ;;(glEnable gl_lighting)
    (glEnable gl_color_material)
    (glenable gl_normalize)
    ;; scan vertically 
    (dotimes (Column (columns Self))
      (dotimes (Row (rows Self))
        (cond
         ;; visible pixel
         ((>= (rgba-image-alpha Image Column Row (columns Self)) (visible-alpha-threshold Self))
          (let ((Z00 (altitude-at Self Row Column))
                (Z10 (altitude-at Self (1+ Row) Column))
                (Z01 (altitude-at Self Row (1+ Column)))
                (Z11 (altitude-at Self (1+ Row) (1+ Column)))
                (X1 (+ x dx))
                (Y1 (+ y dy)))
            (when (= Face-Number 0) (glBegin gl_triangle_strip))
            ;; Set face color + alpha if necessary
            (let ((Color-Red (rgba-image-red Image Column Row (columns Self)))
                  (Color-Green (rgba-image-green Image Column Row (columns Self)))
                  (Color-Blue (rgba-image-blue Image Column Row (columns Self)))
                  (Color-Alpha (rgba-image-alpha Image Column Row (columns Self))))
              (unless (and (equal Color-Red Old-Color-Red)
                           (equal Color-Green Old-Color-Green)
                           (equal Color-Blue Old-Color-Blue)
                           (equal Color-Alpha Old-Color-Alpha))
                ;; terminate strip and start new one: WHY? This could be a RADEON bug
                ;; this wastes a lot of time since strips will be much shorter on average
                (when (> Face-Number 0)
                  (glEnd)
                  (glBegin gl_triangle_strip)
                  (setq Face-Number 0))
                (with-vector (V 
                              (/ Color-Red 256s0)
                              (/ Color-Green 256s0)
                              (/ Color-Blue 256s0)
                              (/ Color-Alpha 256s0))
                  (glColor4fv V)
                  ;; (glColor4f 0.5s0 0.5s0 0.5s0 0.5s0)
                  )
                (setq Old-Color-Red Color-Red)
                (setq Old-Color-Green Color-Green)
                (setq Old-Color-Blue Color-Blue)
                (setq Old-Color-Alpha Color-Alpha)))
            ;; Draw Face
            ;; vertex 0 and 1
            (cond
             ((= Face-Number 0)        ;; first face of new strip
              (normal-at Self Row (1+ Column) 2m)
              (glVertex3f x1 y z01)
              (normal-at Self Row Column 2m)
              (glVertex3f x y z00)
              (setq Face-Number 1))
             (t                        ;; contiguous face
              (incf Face-Number)))
            ;; vertex 2 and 3
            (normal-at Self (1+ Row) (1+ Column) 2m)
            (glVertex3f x1 y1 z11)
            (normal-at Self (1+ Row) Column 2m)
            (glVertex3f x y1 z10)))
          (t                      ;; pixel invisible
           ;; Terminate strip if needed
           (when (> Face-Number 0)
             (glEnd)
             (setq Face-Number 0))))
        (incf y dy))
      (setq y 0s0)
      (incf x dx)
      (when (> Face-Number 0)
        (glEnd)
        (setq Face-Number 0)))
    ;; Terminate strip if needed
    (when (> Face-Number 0)
      (glEnd)
      (setq Face-Number 0))
    ;; finish
    (glcolor4fv {1.0 1.0 1.0 1.0})))

;_________________________________________
; Altitude Operations                     |
;_________________________________________

(defmethod NEEDS-TO-BE-DIFFUSED-P ((Self inflatable-icon) Row Column)
  ;; because of the asymetry of the reference point only diffuse if there are neighbours to the right and below 
  (and
   (>= (rgba-image-alpha (image Self) Column Row (columns Self)) 64)
   (array-in-bounds-p (altitudes Self) (1- Row) Column)
   (>= (rgba-image-alpha (image Self) Column (1- Row) (columns Self)) 64)
   (array-in-bounds-p (altitudes Self) Row (1- Column))
   (>= (rgba-image-alpha (image Self) (1- Column) Row (columns Self)) 64)))


(defmethod INFLATE ((Self inflatable-icon) &key (Steps 100) (Pressure 0.01s0) Noise (Max 1.0s0) Inflate-Pixel-P-Fn)
  (dotimes (I Steps)
    (dotimes (Column (1- (columns Self)))
      (dotimes (Row (1- (rows Self)))
        (when (and 
               (or (not Inflate-Pixel-P-Fn) (funcall Inflate-Pixel-P-Fn Row Column))
               (needs-to-be-diffused-p Self Row Column))
          (diffuse Self Row Column Pressure (if (= i 0) Noise 0.0s0) Max))))))


(defmethod INFLATE-COLOR ((Self inflatable-icon) &key (Steps 100) (Pressure 0.01s0) Noise (Max 1.0s0))
  (dotimes (I Steps)
    (dotimes (Column (1- (columns Self)))
      (dotimes (Row (1- (rows Self)))
        (when (>= (rgba-image-alpha (image Self) Column Row (columns Self)) 64)
          ;; need to provide color value as paramter
          (case (rgba-image-red (image Self) Column Row (columns Self))
            (255 (diffuse Self Row Column -0.1 (if (= i 0) Noise 0.0s0) Max))
            (t   (diffuse Self Row Column Pressure (if (= i 0) Noise 0.0s0) Max))))))))


(defmethod DIFFUSE ((Self inflatable-icon) R C Pressure Noise Max)
  (setf (aref (altitudes Self) R C)
        (float
         (+ (if (and Noise (> Noise 0.0))
              (* Noise (aref (noise-map Self) R C))
              0.0)
            (min Max
                 (* 0.25s0 
                    (+ Pressure  
                       (altitude-at Self (+ R 1) C)
                       (altitude-at Self (- R 1) C)
                       (altitude-at Self R (+ C 1))
                       (altitude-at Self R (- C 1)))))) 
         0s0))
  ;; adjust height?
  (when (> (aref (altitudes Self) R C) (height Self))
    (setf (height Self) (aref (altitudes Self) R C))))


              
(defmethod FLATTEN ((Self inflatable-icon))
  ;; a flat inflatable icon could be displayed much faster as single quad poly
  (dotimes (Row (rows Self))
    (dotimes (Column (columns Self))
      (setf (aref (altitudes Self) Row Column) 0.0))))
  
;_________________________________________
; File I/O                                |
;_________________________________________

#| still Carbon

(defmethod SAVE-TO-PATHNAME ((Self inflatable-icon) Pathname)
  (with-open-file (File Pathname :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (dotimes (I (#_getPtrSize (image Self)))
      (write-byte (%get-byte (image Self) i) File))))

|#

;__________________________________
; Contructors                      |
;__________________________________

(defun MAKE-INFLATABLE-ICON-FROM-IMAGE-FILE (Pathname)
  (let ((Icon (make-instance 'inflatable-icon
                :icon (format nil "~A.~A" (pathname-name Pathname) (pathname-type Pathname))
                :auto-compile nil)))
    ;; convert the icon into an RGBA image and initialize the altitude array
    (multiple-value-bind (Image Columns Rows)
                         (create-image-from-file Pathname)
      (setf (image Icon) Image)
      (setf (rows Icon) Rows)
      (setf (image Icon) Image)
      (setf (altitudes Icon) (make-array (list Rows Columns) 
                                         :element-type 'short-float
                                         :initial-element 0.0)))
    Icon))

  
;********************************************************
;* Lobster Inflatable Icon                              *
;********************************************************

(defclass LOBSTER-INFLATABLE-ICON (inflatable-icon)
  ()
  (:documentation "freqently used inflatable icon used for benchmarking and defaulting"))


(defmethod INITIALIZE-INSTANCE :after ((Self lobster-inflatable-icon) &rest Args)
  (declare (ignore Args))
  (let ((Pathname  "lui:resources;shapes;redLobster;redLobster.png"))
    (setf (icon Self) (format nil "~A.~A" (pathname-name Pathname) (pathname-type Pathname)))
    (setf (auto-compile Self) t)
    ;; convert the icon into an RGBA image and initialize the altitude array
    (multiple-value-bind (Image Columns Rows)
                         (create-image-from-file Pathname)
      (setf (image Self) Image)
      (setf (rows Self) Rows)
      (setf (columns Self) Columns)
      (setf (altitudes Self) (altitudes (load-object "lui:resources;shapes;redLobster;index.shape" :package (find-package :xlui)))))))



#| Examples:


;;; Intel Mac 2.6 GHz, MacBook Pro, Nvidia GeForce 8600 GT, CCL 1.4, 64 bit, red lobster: 40us = 25k Lobster/Second


(inspect 
 (make-inflatable-icon-from-image-file "lui:resources;shapes;redLobster;redLobster.png")

<application-window>
  <agent-3d-view>
    <lobster-inflatable-icon/>
  </agent-3d-view>
</application-window>




(inspect 
(make-inflatable-icon-from-image-file "ad3d:resources;images;SEStopScriptImage.tiff"))


(inspect 
(load-object "ccl:projects;Sokoban;agents;pusher;shapes;red lobster;index.shape"))

|#