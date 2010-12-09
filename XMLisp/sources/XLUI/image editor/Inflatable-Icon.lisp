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
;*    1.4.1     : 10/12/07 *Minimal-Inflatable-Icon-Depth*           *
;*    2.0       : 02/09/10 CCL Cocoa                                 *
;*    2.1       : 05/05/10 AI: height --> depth                      *
;* Systems      : Intel-Mac, OS X 10.6.2, CCL 1.4                    *
;* Abstract     : 3D shapes derived from 2D icons/images             *
;*                                                                   *
;*********************************************************************

(in-package :xlui)



(defclass INFLATABLE-ICON (shape) ;; agent-3d
  ((rows :accessor rows :initform 32 :initarg :rows)
   (columns :accessor columns :initform 32 :initarg :columns)
   (steps :accessor steps :initform 10)
   (pressure :accessor pressure :initform 0.0 :type short-float)
   (ceiling-value :accessor ceiling-value :initarg :ceiling-value :initform 1.0 :type short-float)
   (noise :accessor noise :initform 0s0)
   (smooth :accessor smooth :initarg :smooth :initform 0 :type integer)
   (max-value :accessor max-value :initform 1.0)
   (dx :accessor dx :initform 1.0 :initarg :dx :type short-float)
   (dy :accessor dy :initform 1.0 :initarg :dy :type short-float)
   (dz :accessor dz :initform 0.0 :initarg :dz :type short-float :documentation "z offset, can be used to antialias through edge snipping")
   (display-list :accessor display-list :initform nil)
   (is-compiled :accessor is-compiled :initform nil :type boolean)
   (auto-compile :accessor auto-compile :initform t :type boolean :initarg :auto-compile)
   (is-upright :accessor is-upright :initform nil :initarg is-upright :type boolean)
   (surfaces :accessor surfaces :initform 'front :initarg :surfaces :type symbol :documentation "FRONT, FRONT AND BACK, ..")
   (icon :accessor icon :initform nil :initarg :icon :documentation "name of icon image")
   (image :accessor image :initform nil :initarg image :documentation "RGBA image")
   (noise-map :accessor noise-map :initform nil :documentation "a 2d map with noise values")
   (altitudes :accessor altitudes :type array)
   (distance :accessor distance :initform 0.0 :initarg :distance :documentation "distance between mirrored surfaces")
   (connectors :accessor connectors :initform nil :documentation "connectors are polygons connecting inner and outer edges of the symetric sides")
   (visible-alpha-threshold :accessor visible-alpha-threshold :initform 64 :allocation :class :documentation "8 bit alpha value used as visible/invisible threshold")
   (texture-id :accessor texture-id :initform nil :documentation "OpenGL texture name")
   (is-flat :accessor is-flat :initform nil :type boolean :documentation "speed optimized rendering if flat"))
  (:documentation "High polygon count 3D object made from inflated icon")
  (:default-initargs 
      :depth 0.0d0))


;********************************************************
;* Specification                                        *
;********************************************************

(defgeneric PIXEL-VISIBLE-P (Inflatable-Icon Row Column)
  (:documentation "Return if pixel is visible and needs to be reprented with face. Take visible-alpha-threshold into account. Out of range row, columns return nil"))

(defgeneric COPY-CONTENT-INTO (Source-Inflatable-Icon Destination-Inflatable-Icon)
  (:documentation "Copy the content of the source inflatable icon, image, pressure, etc. into the destination inflable icon"))

(defgeneric COMPUTE-DEPTH (Inflatable-Icon)
  (:documentation "Compute the depth of the inflatable icon based on its content, inflation, and faces"))

(defgeneric UPDATE-TEXTURE-FROM-IMAGE (Inflatable-Icon &optional Image)
  (:documentation "Update the texture from the current image"))


;********************************************************
;* Implementation                                       *
;********************************************************

(defmethod PRINT-SLOTS ((Self inflatable-icon))  
  `(icon rows columns depth pressure ceiling-value smooth steps noise max-value is-upright surfaces altitudes distance dz is-flat ))


(defmethod FINISHED-READING :after ((Self inflatable-icon) Stream)
  (declare (ignore Stream))
  ;; no point in storing connectors: generate them in the right cases
  (case (surfaces Self)
    (front-and-back-connected (compute-connectors Self)))
  ;; flat icons should not be compiled: will just create overhead with display lists
  (when (is-flat Self)
    (setf (auto-compile Self) nil)))



(defmethod MAKE-IMAGE-FROM-ICON ((Self inflatable-icon))
  ;; convert the icon into an RGBA image and initialize the altitude array
  (when (part-of Self)
    (when (texture-file (part-of Self) (icon Self))
      ;; Flat inflatable icons should not be auto-compiled
      (setf (auto-compile self) (not (is-flat self)))
      (multiple-value-bind (Image Columns Rows)
                           (create-image-from-file (texture-file (part-of Self) (icon Self)) :forced-depth 32)
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
  ;; check for size compatibility to avoid disaster
  (unless (and (= (rows Source) (rows Destination)) 
               (= (columns Source) (columns Destination)))
               ;; sizeof does not work for these images!! (= (print (sizeof (image Source) )) (print (sizeof (image Destination)))))
    (error "cannot copy content of source into destination inflatable icon: incompatible sizes"))
  ;; given that they are the same size only copy content
  (setf (is-upright Destination) (is-upright Source))
  (setf (depth Destination) (depth Source))
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
  ;; copy Image: slow byte-by-byte copy
  (dotimes (I (* 4 (rows Source) (columns Source)))
    (set-byte (image Destination) (get-byte (image Source) i) i))
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
  (setf (depth Destination) (depth Source))
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





(defmethod MAX-VISIBLE-PIXEL-ROW ((Self inflatable-icon))
  (dotimes (Row (rows Self) (rows Self))
    (dotimes (Column (columns Self))
      (when (pixel-visible-p Self (- (rows Self) Row) Column) (return-from max-visible-pixel-row (- (rows Self) Row))))))
      

(defvar *Minimal-Inflatable-Icon-depth* 0.01 "> 0 to avoid having flattened icons pile up in 0 depth stack")


(defmethod COMPUTE-DEPTH ((Self inflatable-icon))
  ;; this can just be a heuristic: maybe ability for user to change
  (setf (depth Self)
        (max *Minimal-Inflatable-Icon-depth*
             ;; this cond may not be needed, we need to take distance into account for both upright and non upright inflatable icons.
             (cond
              ((is-upright Self)
               (float (/ (max-visible-pixel-row Self) (rows Self)) 0.0))
              (t  
               (maximum-altitude Self))))))
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
      ;; clam to edge to avoid gaps between adjacent textures
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
      ;; linear pixel magnification
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_NEAREST)
      ;; need to update also the mipmaps to avoid mix of new and old image
      (gluBuild2DMipmaps GL_TEXTURE_2D GL_RGBA8 (columns Self) (rows Self) GL_RGBA GL_UNSIGNED_BYTE Image)
      )
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
  (when (equal  (image Self) nil) 
    (return-from compute-connectors))
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
  ;(print "draw connectors")
  ;(print (/ (hemlock::time-to-run 
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
     ;; back/bottom
     (glpushmatrix)
     (glscalef 1s0 1s0 -1s0)
     (when (< (distance self) 0.0)
       (glClipPlane GL_CLIP_PLANE0 {0.0d0 0.0d0 1.0d0 0.0d0})
       (glEnable GL_CLIP_PLANE0))
     (glTranslatef 0s0 0s0 (distance Self))     
     (if (is-compiled Self) (draw-compiled Self) (draw-uncompiled Self))
     (when (< (distance self) 0.0)
       (glDisable GL_CLIP_PLANE0))
     (glpopmatrix)
     (glpushmatrix)
     (when (< (distance self) 0.0)
       (glClipPlane GL_CLIP_PLANE1 {0.0d0 0.0d0 1.0d0 0.0d0})
       (glEnable GL_CLIP_PLANE1))
     (glTranslatef 0s0 0s0 (distance Self))
     (if (is-compiled Self) (draw-compiled Self) (draw-uncompiled Self))
     (when (< (distance self) 0.0)
       (glDisable GL_CLIP_PLANE1))
     (glpopmatrix))
    (front-and-back-connected
     (glpushmatrix)
     ;; back/bottom
     (glpushmatrix)
     (glscalef 1s0 1s0 -1s0)
     (glTranslatef 0s0 0s0 (distance Self))
     (if (is-compiled Self) (draw-compiled Self) (draw-uncompiled Self))
     (glpopmatrix)
     (glTranslatef 0s0 0s0 (distance Self))
     (if (is-compiled Self) (draw-compiled Self) (draw-uncompiled Self))
     ;; connectors
     (if (connectors Self)  (draw-connectors Self))
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

     ;; bottom
     (glpushmatrix)
     (glTranslatef 0.0 (- 0.5 (distance Self)) 0.5)
     (glscalef 1.0 -1.0 1.0)
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


(defmethod DRAW-AS-FLAT-TEXTURE ((Self inflatable-icon)) 
  ;; HACK!! move ccl:: parts into LUI Cocoa
  ;; JIT make texture and mipmaps
  (unless (texture-id Self) 
    ;; use image as texture
    (unless (image Self) (error "image of inflatable icon is undefined"))
    ;; HACK: sizeof always return zero so we need to find some other sort of error checking.
    ;; (unless (= (sizeof (image Self)) (* (rows Self) (columns Self) 4)) (error "image size does not match row/column size"))
    ;; generate texture
    (ccl::rlet ((&texName :long))
      (glGenTextures 1 &texName)
      (setf (texture-id Self) (ccl::%get-long &texName)))
    (glBindTexture GL_TEXTURE_2D (texture-id Self))
    (glTexSubImage2D GL_TEXTURE_2D 0 0 0 (columns Self) (rows Self) GL_RGBA GL_UNSIGNED_BYTE (image Self))
    ;; clam to edge to avoid gaps between adjacent textures
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
    ;; linear pixel magnification
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_NEAREST)
    ;;; (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA8 (columns Self) (rows Self) 0 GL_RGBA GL_UNSIGNED_BYTE (image Self))
    ;#-cocotron (unless (zerop (gluBuild2DMipmaps GL_TEXTURE_2D 4 (columns Self) (rows Self) GL_RGBA GL_UNSIGNED_BYTE (image Self)))
    (unless (zerop (gluBuild2DMipmaps GL_TEXTURE_2D GL_RGBA8 (columns Self) (rows Self) GL_RGBA GL_UNSIGNED_BYTE (image Self)))
      (error "could not create mipmaps")))
  ;; render as textured quad
  (glEnable GL_TEXTURE_2D)
  (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
  (glBindTexture GL_TEXTURE_2D (texture-id Self))
  (glBegin GL_QUADS)
  (glNormal3f 0.0 0.0 1.0)
  (glTexCoord2i 0 0) (glVertex2f 0.0 0.0)
  (glTexCoord2i 1 0) (glVertex2f 1.0 0.0)
  (glTexCoord2i 1 1) (glVertex2f 1.0 1.0)
  (glTexCoord2i 0 1) (glVertex2f 0.0 1.0)
  (glEnd)
  (glDisable GL_TEXTURE_2D))


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
         (Old-Color-Alpha nil))
    (glDisable gl_texture_2d)
    ;;(glEnable gl_lighting)
    (glEnable gl_color_material)
    (glenable gl_normalize)
    (let ((state :start)
          (last-vertex ())
          (vertex-before-last())
          (repeat-next-vertex nil))
    ;; scan vertically 
    (dotimes (Column (+ 1 (columns Self)))
      (dotimes (Row   (rows Self))
        (let ((Z00 (altitude-at Self Row Column))
                (Z10 (altitude-at Self (1+ Row) Column))
                (Z01 (altitude-at Self Row (1+ Column)))
                (Z11 (altitude-at Self (1+ Row) (1+ Column)))
                (X1 (+ x dx))
                (Y1 (+ y dy)))
          (case state
            (:start
             (when (>= (rgba-image-alpha Image Column Row (columns Self)) (visible-alpha-threshold Self))
                 (let ((Color-Red (rgba-image-red Image Column Row (columns Self)))
                       (Color-Green (rgba-image-green Image Column Row (columns Self)))
                       (Color-Blue (rgba-image-blue Image Column Row (columns Self)))
                       (Color-Alpha (rgba-image-alpha Image Column Row (columns Self))))
                   ;; only set the color if it has changed
                   (unless (and (equal Color-Red Old-Color-Red)
                                (equal Color-Green Old-Color-Green)
                                (equal Color-Blue Old-Color-Blue)
                                (equal Color-Alpha Old-Color-Alpha))
                     (with-vector (V 
                                   (/ Color-Red 256s0)
                                   (/ Color-Green 256s0)
                                   (/ Color-Blue 256s0)
                                   (/ Color-Alpha 256s0))
                       (glColor4fv V))
                     (setq Old-Color-Red Color-Red)
                     (setq Old-Color-Green Color-Green)
                     (setq Old-Color-Blue Color-Blue)
                     (setq Old-Color-Alpha Color-Alpha)))
                 ;; We have found the first pixel in the image, begin the triangle strip and draw the first pixel (4 vertices, 2 triangles)
                 (glBegin gl_triangle_strip)
                 (setf state :paint)
                 (normal-at Self Row (1+ Column) 2m)
                 (glVertex3f x1 y z01)
                 (normal-at Self Row Column 2m)
                 (glVertex3f x y z00)
                 (normal-at Self (1+ Row) (1+ Column) 2m)
                 (glVertex3f x1 y1 z11)
                 (normal-at Self (1+ Row) Column 2m)
                 (glVertex3f x y1 z10)
                 (when (equal row  (- (rows self) 1) ) 
                     (setf repeat-next-vertex t)
                     (glVertex3f x y1 z10))
                 (setf last-vertex `(,x ,y1 ,z10))
                 (setf vertex-before-last `(,x1 ,y1 ,z11))))
            (:paint
             (cond
              ( (equal column  (columns self))
               ;; we have reached the end of the image so end the triangle strip
               (setf state :finish)
               (glEnd))
              ((< (rgba-image-alpha Image Column Row (columns Self)) (visible-alpha-threshold Self))
               ;; We have found a transparent pixel while in the paint state, switch to the skip state and draw a degenerate triangle
               (setf state :skip)
               (if last-vertex
                 (glVertex3f (first last-vertex) (second last-vertex) (third last-vertex))))
              (t
               (let ((Color-Red (rgba-image-red Image Column Row (columns Self)))
                     (Color-Green (rgba-image-green Image Column Row (columns Self)))
                     (Color-Blue (rgba-image-blue Image Column Row (columns Self)))
                     (Color-Alpha (rgba-image-alpha Image Column Row (columns Self))))
                 ;; only set the color if it has changed
                 (unless (and (equal Color-Red Old-Color-Red)
                              (equal Color-Green Old-Color-Green)
                              (equal Color-Blue Old-Color-Blue)
                              (equal Color-Alpha Old-Color-Alpha))
                   (with-vector (V 
                                 (/ Color-Red 256s0)
                                 (/ Color-Green 256s0)
                                 (/ Color-Blue 256s0)
                                 (/ Color-Alpha 256s0))
                     (glColor4fv V))
                   (setq Old-Color-Red Color-Red)
                   (setq Old-Color-Green Color-Green)
                   (setq Old-Color-Blue Color-Blue)
                   (setq Old-Color-Alpha Color-Alpha)
                   ;; we need to draw a degenerate triangle after changing color so that it will not blend between the two colors
                   ;; I.E. we draw vertices 0 1 2 3 2 3 4 5 6 if we are making a color shift between the pixel 
                   ;; that contains the triangles: 0 1 2 and 1 2 3 and the pixel that contains the triangles : 3 4 5 and 4 5 6    
                   (unless repeat-next-vertex
                     (glVertex3f (first vertex-before-last) (second vertex-before-last) (third vertex-before-last))
                     (glVertex3f (first last-vertex) (second last-vertex) (third last-vertex))
                     )))
               (when repeat-next-vertex

                   #|
                   (glVertex3f x1 y1 z11)
                   |#
                   (normal-at Self Row (1+ Column) 2m)
                   ;; repeat first pixel to form a degenerate triangle
                   (glVertex3f x1 y z01)
                   (glVertex3f x1 y z01)
                   (normal-at Self Row Column 2m)
                   (glVertex3f x y z00)
                   (setf repeat-next-vertex nil))
               (normal-at Self (1+ Row) (1+ Column) 2m)
               (glVertex3f x1 y1 z11)
               (normal-at Self (1+ Row) Column 2m)
               (glVertex3f x y1 z10)
               (when (equal row  (- (rows self) 1) )
                   (setf repeat-next-vertex t)
                   (glVertex3f x y1 z10))
               (setf last-vertex `(,x ,y1 ,z10))
               (setf vertex-before-last `(,x1 ,y1 ,z11)))))
            (:skip
             (cond
              ( (equal column  (columns self) )
               ;; we have reached the end of the image so end the triangle strip
               (setf state :finish)
               (glEnd))
              ;;is the new pixel opaque?  If so switch paint state, change color if needed and draw the pixel (4 vertices) + degenerate triangle
              
              ((>= (rgba-image-alpha Image Column Row (columns Self)) (visible-alpha-threshold Self))
               (setf state :paint)
                           
               (let ((Color-Red (rgba-image-red Image Column Row (columns Self)))
                     (Color-Green (rgba-image-green Image Column Row (columns Self)))
                     (Color-Blue (rgba-image-blue Image Column Row (columns Self)))
                     (Color-Alpha (rgba-image-alpha Image Column Row (columns Self))))
                 ;; only set the color if it has changed
                 (unless (and (equal Color-Red Old-Color-Red)
                              (equal Color-Green Old-Color-Green)
                              (equal Color-Blue Old-Color-Blue)
                              (equal Color-Alpha Old-Color-Alpha))
                   (with-vector (V 
                                 (/ Color-Red 256s0)
                                 (/ Color-Green 256s0)
                                 (/ Color-Blue 256s0)
                                 (/ Color-Alpha 256s0))
                     (glColor4fv V))
                   (setq Old-Color-Red Color-Red)
                   (setq Old-Color-Green Color-Green)
                   (setq Old-Color-Blue Color-Blue)
                   (setq Old-Color-Alpha Color-Alpha)))
               (normal-at Self Row (1+ Column) 2m)
               ;; repeat first pixel to form a degenerate triangle
               (glVertex3f x1 y z01)
               (glVertex3f x1 y z01)
               (normal-at Self Row Column 2m)
               (glVertex3f x y z00)
               (normal-at Self (1+ Row) (1+ Column) 2m)
               (glVertex3f x1 y1 z11)
               (normal-at Self (1+ Row) Column 2m)
               (glVertex3f x y1 z10)
               (setf repeat-next-vertex nil)
               (when (equal row  (- (rows self) 1) )
                   (setf repeat-next-vertex t)
                   (glVertex3f x y1 z10))
               (setf last-vertex `(,x ,y1 ,z10))
               (setf vertex-before-last `(,x1 ,y1 ,z11)))))
            (:finish
             ;;do nothing, we are done.
             )))
        (incf y dy))
      (setq y 0s0)
      (incf x dx)
      ))
    ;; Terminate strip if needed
    
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


(defmethod AVERAGE-ALTITUDE ((Self inflatable-icon) &key Inflate-Pixel-P-Fn)
  ;; return the average only of all the legitimate pixels
  (let ((Sum 0.0) 
        (Count 0))
  (dotimes (Column (1- (columns Self)))
    (dotimes (Row (1- (rows Self)))
      (when (and 
             (or (not Inflate-Pixel-P-Fn) (funcall Inflate-Pixel-P-Fn Row Column))
             (needs-to-be-diffused-p Self Row Column))
        (incf Sum (altitude-at Self Row Column))
        (incf Count))))
    (if (> Count 0)
      (/ Sum Count)
      0.0)))


(defmethod MAXIMUM-ALTITUDE ((Self inflatable-icon) &key Inflate-Pixel-P-Fn)
  (let ((Max 0.0))
    (dotimes (Row (rows Self) Max)
      (dotimes (Column (columns Self))
        (when (and 
               (or (not Inflate-Pixel-P-Fn) (funcall Inflate-Pixel-P-Fn Row Column))
               (needs-to-be-diffused-p Self Row Column))
          (setq Max (max Max (aref (altitudes Self) Row Column))))))))



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
                 (* 0.25
                    (+ Pressure
                       (altitude-at Self (+ R 1) C)
                       (altitude-at Self (- R 1) C)
                       (altitude-at Self R (+ C 1))
                       (altitude-at Self R (- C 1)))))) 
         0s0))
  ;; adjust depth?
  (when (> (aref (altitudes Self) R C) (depth Self))
    (setf (depth Self) (aref (altitudes Self) R C))))


              
(defmethod FLATTEN ((Self inflatable-icon))
  ;; a flat inflatable icon could be displayed much faster as single quad poly
  (dotimes (Row (rows Self))
    (dotimes (Column (columns Self))
      (setf (aref (altitudes Self) Row Column) 0.0)))
  ;; need to make a new flat texture
  (when (texture-id Self)
    (ccl::rlet ((&Tex-Id :long (texture-id Self)))
      (glDeleteTextures 1 &Tex-Id))
    (setf (texture-id Self) nil)))
  
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
  (let ((Pathname  "lui:resources;templates;shapes;redLobster;redLobster.png"))
    (setf (icon Self) (format nil "~A.~A" (pathname-name Pathname) (pathname-type Pathname)))
    (setf (auto-compile Self) t)
    ;; convert the icon into an RGBA image and initialize the altitude array
    (multiple-value-bind (Image Columns Rows)
                         (create-image-from-file Pathname)
      (setf (image Self) Image)
      (setf (rows Self) Rows)
      (setf (columns Self) Columns)
      (setf (altitudes Self) (altitudes (load-object "lui:resources;templates;shapes;redLobster;index.shape" :package (find-package :xlui)))))))



#| Examples:


;;; Intel Mac 2.6 GHz, MacBook Pro, Nvidia GeForce 8600 GT, CCL 1.4, 64 bit, red lobster: 40us = 25k Lobster/Second


(inspect 
 (make-inflatable-icon-from-image-file "lui:resources;templates;shapes;redLobster;redLobster.png")

(defparameter *window*
<application-window title="window">
  <agent-3d-view name="losbter-view">
    <lobster-inflatable-icon  name="lobster2"/>
  </agent-3d-view>
</application-window>)


(defparameter *window2*
<application-window title="window2">
  <agent-3d-view name="losbter-view">
    <lobster-inflatable-icon distance=".5" surfaces="front" name="lobster2"/>
  </agent-3d-view>
</application-window>)


(with-glcontext (view-named *window2* "losbter-view")
  (format t " ~% ~,1F klops" 
         (/ 1e10 (hemlock::time-to-run
          (dotimes (i 10000)
            (draw (first (agents (view-named *window2* "losbter-view")))))))
               ))

;;; OS X 10.6.2 3.06GHz Intel Core 2 Duo 256MB ATI Radio HD 4670 ~26klops 
;;; OS X 10.6.5 2.66 Intel Core i7, NVIDIA GeForce GT 330M        54 klops



(defparameter *window3*
<application-window title="window3">
  <agent-3d-view name="losbter-view">
    <lobster-inflatable-icon distance=".5" surfaces="front-and-back-connected" name="lobster2"/>
  </agent-3d-view>
</application-window>)


(inspect (view-named *window* "losbter-view"))

(inspect 
(make-inflatable-icon-from-image-file "ad3d:resources;images;SEStopScriptImage.tiff"))


(inspect 
(load-object "ccl:projects;Sokoban;agents;pusher;shapes;red lobster;index.shape"))

|#