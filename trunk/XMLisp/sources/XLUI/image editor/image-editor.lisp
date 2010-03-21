;*****************************************************************************
;*                                                                           *
;*                       I M A G E   E D I T O R                             *
;*                                                                           *
;*****************************************************************************
;* Author       : Ronald Sudomo (ronald@agentsheets.com)                     *
;*                Alexander Repenning (alex@agentsheets.com)                 *
;*                Travis Offtermatt (Travis.Offtermatt@colorado.edu)         *
;*                http://www.agentsheets.com                                 *
;* Copyright    : (c) 2009, AgentSheets Inc.                                 *
;* Version      :                                                            *
;*    1.0       : 06/14/06 initial implementation                            *
;*    1.0.1     : 06/26/06 comments added                                    *
;*    1.0.2     : 06/28/06 use bevel-color-swatch-dialog-item                *
;*    1.0.3     : 07/06/06 cross-platform key-down functions                 *
;*    1.0.4     : 07/07/06 get/set-color-at, fill-selected-pixels,           *
;*                         document-window menu methods                      *
;*    1.0.5     : 07/14/06 transparent background, eyedropper tool,          *
;*                         on-image-saved hook, new-image                    *
;*    1.0.6     : 07/19/06 integrate Travis' code: paint-bucket tool,        *
;*                         magic-wand, mirroring, grid                       *
;*                         Alex's fix for background texture scaling         *
;*    1.0.7     : 08/01/06 use choice-buttons, fix cursor problem,           *
;*                         integrate Travis' code: improve magic-wand,       *
;*                         paint-bucket                                      *
;*    1.0.8     : 08/07/06 integrate Travis' code:                           *
;*                         rename paint-bucket to flood-fill,                *
;*                         tolerance-function for magic-wand and flood-fill, *
;*                         clean up mirroring, mirroring choice buttons,     *
;*                         grid choice buttons (travis)                      *
;*    1.0.9     : 08/08/06 fix bug in absolute-color-difference-ignore-alpha *
;*                         (travis)                                          *
;*    1.0.10    : 08/25/06 AR: use MCL 5.2 standard-alert-dialog,            *
;*                                         window-save-as                    *
;*    1.0.11    : 01/04/07 AR: make-me-the-current-context in save-image     *
;*    1.0.12    : 08/16/07 AR: make-me-the-current-context -> delete-texture *
;*    2.0       : 08/21/09 AR: CCL                                           *
;* Abstract     : Simple OpenGL based image editor.                          *
;* Todo: make pixel buffer on init                                           * 
;*                                                                           *
;*****************************************************************************

;;;TODO:
;;; - always create 32-bit texture (otherwise problem with get/set-color)

(in-package :xlui)


;**************************************
;* Texture Functions                  *
;**************************************

(defun CREATE-EMPTY-TEXTURE (Width Height &optional (Depth 32))
  ;; (format t "create-empty-texture: ~A ~A ~A~%" Width Height Depth)
  (let* ((Bytes-Per-Pixel (truncate Depth 8)))
    (with-vector-of-size (&Texture (* Width Height Bytes-Per-Pixel))
      ; clear memory
      (dotimes (i (* Width Height Bytes-Per-Pixel))
        (setf (ccl::%get-byte &Texture i) 0))
      ;; create the OpenGL texture (hopefully in accelerator memory) and define parameters
      (ccl::rlet ((&texName :long))
        (glGenTextures 1 &texName)
        (glBindTexture GL_TEXTURE_2D (get-long &texName))
        (let ((Format (ecase Bytes-Per-Pixel (4 GL_RGBA) (3 GL_RGB))))
          (glTexImage2D GL_TEXTURE_2D 0 Bytes-Per-Pixel Width Height 0 Format GL_UNSIGNED_BYTE &Texture))
        (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER  GL_NEAREST)
        (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER  GL_NEAREST)
        ;; texture is in OpenGL now, can get rid of all the support data structures
        (get-long &texName)))))


(defun CREATE-32-BIT-TEXTURE-FROM-FILE ()
  ;;;TODO: implementation
  )


(defun DELETE-TEXTURE (Texture)
  "Deletes the specified texture."
  (when Texture
    (ccl::rlet ((&Tex-Id :long Texture))
      (glDeleteTextures 1 &Tex-Id))))


;**************************************
;* Line-Segment                       *
;**************************************

(defclass LINE-SEGMENT ()
  ((x1 :accessor x1 :initarg :x1 :initform nil :documentation "x-coord of the starting point")
   (y1 :accessor y1 :initarg :y1 :initform nil :documentation "y-coord of the starting point")
   (x2 :accessor x2 :initarg :x2 :initform nil :documentation "x-coord of the end point")
   (y2 :accessor y2 :initarg :y2 :initform nil :documentation "y-coord of the end point"))
  (:documentation "Line segments used in drawing selection outline."))


(defmethod CONNECTED-P ((Line1 line-segment) (Line2 line-segment))
  "Returns t if the end point of the first line is the starting point of the second line."
  (and (= (x2 Line1) (x1 Line2)) (= (y2 Line1) (y1 Line2))))


(defmethod PRINT-OBJECT ((Self line-segment) Stream)
  "Called by the Lisp printer to print this object.
   Specialized so it's easier to debug."
  (format Stream "[~A,~A]->[~A,~A]" (x1 Self) (y1 Self) (x2 Self) (y2 Self)))


(defun GROUP-LINE-SEGMENTS (Lines)
  "Groups the line segments into clusters where all lines in a cluster form a loop
   and lines in different clusters are not connected."
  (let ((Line-Clusters nil)
        (Current-Cluster nil)
        (Current-Line nil))
    (loop
      (when (null Lines) (return))
      (if (null Current-Line)
          (progn
            (setq Current-Line (first Lines))
            (setq Lines (rest Lines))
            (push Current-Line Current-Cluster))
        (let ((Next-Line (find-if #'(lambda (L) (connected-p Current-Line l)) Lines)))
          (when Next-Line
            (push Next-Line Current-Cluster)
            (setq Lines (remove Next-Line Lines))
            (setq Current-Line Next-Line))
          (when (or (null Next-Line) (null Lines))
            (push (reverse Current-Cluster) Line-Clusters)
            (setq Current-Line nil)
            (setq Current-Cluster nil)))))
    Line-Clusters))


;**************************************
;* Image-Editor                       *
;**************************************

(defparameter *Default-Background-Texture* "lui:resources;textures;image-editor-bg.png")


(defclass IMAGE-EDITOR (opengl-dialog)
  ((image :accessor image :initform nil :documentation "filename")
   (img-texture :accessor img-texture :initform nil :documentation "OpenGL texture to store the image")
   (img-width :accessor img-width :initarg :img-width :initform 32 :documentation "image width in pixels")
   (img-height :accessor img-height :initarg :img-height :initform 32 :documentation "image height in pixels")
   (img-depth :accessor img-depth :initform 32 :documentation "image depth")
   (bg-texture :accessor bg-texture :initform nil :documentation "background texture")
   (canvas-height :accessor canvas-height :initform 1.0 :documentation "world coordinate")
   (canvas-width :accessor canvas-width :initform 1.0 :documentation "world coordinate")
   (hovered-pixel-col :accessor hovered-pixel-col :initform nil :documentation "column of pixel currentl hovered over")
   (hovered-pixel-row :accessor hovered-pixel-row :initform nil :documentation "row of pixel currentl hovered over")
   (on-image-saved :accessor on-image-saved :initarg :on-image-saved :initform nil :documentation "this function will be called with the filename when the image is saved")
   (pen-color-vector :accessor pen-color-vector :initform (make-byte-vector 0 0 0 255) :documentation "current pen color used for drawing")
   (bg-color-vector :accessor bg-color-vector :initform (make-byte-vector 0 0 0 0) :documentation "current background color used for erasing")
   (stipple :accessor stipple :initform #x0F0F :documentation "stipple to mark selection")
   (selection-mask :accessor selection-mask :initform nil :documentation "selection mask to store selection")
   (selection-in-progress :accessor selection-in-progress :initform nil :documentation "list: <type> <values> that stores information about a selection currently in progress")
   (selection-outline :accessor selection-outline :initform nil :documentation "groups of line segments defining the selection outline (cached to avoid expensive recomputation)")
   (pixel-buffer :accessor pixel-buffer :initform nil :documentation "32 bit RGBA color buffer")
   (is-grid-on :accessor is-grid-on :initform nil :documentation "True if grid is visible, false otherwise")
   (is-horizontal-line-on :accessor is-horizontal-line-on :initform nil :documentation "True if horizonal line is on, false oterhwise")
   (is-vertical-line-on :accessor is-vertical-line-on :initform nil :documentation "True if vertical line is on, false oterhwise")
   (tolerance :accessor tolerance :initform 0 :documentation "This is the maximum allowed tolerance with the magic wand"))
  (:documentation "Simple image editor."))


;_______________________________
; Interface                     |
;_______________________________/

(defgeneric LOAD-IMAGE (Image-Editor Pathname)
  (:documentation "Loads an image from a file into the editor."))


(defgeneric CENTER-CANVAS (Image-Editor)
  (:documentation "Ajust camera that canvas is fully visible in center of view"))


(defgeneric SAVE-IMAGE (Image-Editor Pathname)
  (:documentation "Saves the currently edited image into a file."))


(defgeneric GET-RGBA-COLOR-AT (Image-Editor Column Row)
  (:documentation "Return 32 bit color at location <<column> row> as Red Green Blue Alpha 8 bit values"))


(defgeneric SET-RGBA-COLOR-AT (Image-Editor Column Row Red Green Blue &optional Alpha)
  (:documentation "Set 32 bit color value at location <column> <row>"))


(defgeneric GET-COLOR-AT (Image-Editor Column Row)
  (:documentation "Return single 32 bit  color at location <column> <row> as bignum value"))


(defgeneric SET-COLOR-AT (Image-Editor Column Row Color)
  (:documentation "Set 32 bit color value at location <column> <row>"))


(defgeneric IMAGE-CHANGED-EVENT (Image-Editor &optional Column1 Row1 Column2 Row2)
  (:documentation "Called after any part of image has changed. If Column1 Row1 Column2 Row2 are provided then assume change is limited to the rect containing these coordinated"))


;_______________________________
; Implementation                |
;_______________________________/


(defmethod CENTER-CANVAS ((Self image-editor))
  (aim-camera 
   (camera Self) 
   :eye-x (* 0.5 (canvas-width Self)) :eye-y (* 0.5 (canvas-height Self)) 
   :center-x (* 0.5 (canvas-width Self)) :center-y (* 0.5 (canvas-height Self))
   :eye-z 0.87))
  

(defmethod PREPARE-OPENGL ((Self image-editor))
  "Called when the image-editor is initialized."
  (glClearColor 0.0 0.0 0.0 0.0)
  ;; Alpha
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (when (image Self)
    (load-image Self (image Self))
    (center-canvas Self)
    (display Self)))


(defmethod LOAD-BACKGROUND-TEXTURE ((Self image-editor) &optional (Pathname (truename *Default-Background-Texture*)))
  (with-glcontext Self
    (setf (bg-texture Self) (create-texture-from-file Pathname :mag-filter GL_NEAREST :repeat t))))


(defmethod DISPOSE-BACKGROUND-TEXTURE ((Self image-editor))
  "Releases native resources used by the background texture"
  (with-glcontext Self
    (delete-texture (bg-texture Self))))


(defmethod DISPOSE-TEXTURE-IMAGE ((Self image-editor))
  "Releases native resources used by the texture image."
  (with-glcontext Self
    (delete-texture (img-texture Self))))


(defmethod NEW-IMAGE ((Self image-editor) Width Height &optional (Depth 32))
  "Creates an empty image."
  (with-glcontext Self
    (when (img-texture Self) 
      (dispose-texture-image Self))
    (setf (img-texture Self) (create-empty-texture Width Height))
    (setf (selection-mask Self) (make-instance 'selection-mask :width Width :height Height))
    (setf (img-width Self) Width)
    (setf (img-height Self) Height)
    (setf (img-depth Self) Depth)
    ;; mask
    (setf (selection-mask Self) (make-instance 'selection-mask :width Width :height Height))
    ;; canvas: normalize height to be 1.0 but adjust width
    (setf (canvas-width Self) (* (/ 1.0 (img-height Self)) (img-width Self)))))
    


(defmethod LOAD-IMAGE ((Self image-editor) From-Pathname)
  "Loads an image from a file into the editor."
  ;; (format t "loading image: ~A~%" From-Pathname)
  (with-glcontext Self
    (when (img-texture Self) (dispose-texture-image Self))
    (when (pixel-buffer Self) (setf (pixel-buffer Self) nil))
    ;;;  (clear-selection Self)
    ;;; replace with new user feedback? (with-cursor *Watch-Cursor*
    (multiple-value-bind (Name Width Height Depth)
                         (create-texture-from-file From-Pathname :mag-filter GL_NEAREST)
      ;; image
      (setf (img-texture Self) Name)
      (setf (img-width Self) Width)
      (setf (img-height Self) Height)
      (setf (img-depth Self) Depth)
      ;; mask
      (setf (selection-mask Self) (make-instance 'selection-mask :width Width :height Height))
      ;; canvas: normalize height to be 1.0 but adjust width
      (setf (canvas-width Self) (* (/ 1.0 (img-height Self)) (img-width Self))))))

;;  (display Self))


(defmethod SAVE-IMAGE ((Self image-editor) To-Pathname)
  "Saves the currently edited image into a file."
  (declare (ignorable To-Pathname))
  (with-glcontext Self
    (when (img-texture Self)
      (with-glcontext Self
        (save-texture-as-image (img-texture Self) To-Pathname
                               (img-width Self) (img-height Self) :depth (truncate (img-depth Self) 8))
        (when (on-image-saved Self) (funcall (on-image-saved Self) To-Pathname))))))


(defmethod COL-ROW-WITHIN-BOUNDS-P ((Self image-editor) Col Row)
  "Returns t if the specified Col and Row is within bounds."
  (and (>= Col 0) (< Col (img-width Self)) (>= Row 0) (< Row (img-height Self))))


(defmethod IMAGE-BYTES-PER-PIXEL ((Self image-editor))
  "Returns the number of bytes per pixel used to store the image."
  (truncate (img-depth Self) 8))


(defmethod IMAGE-BYTE-OFFSET ((Self image-editor) Col Row)
  (* (+ Col (* (- (img-height Self) Row 1) (img-width Self))) (image-bytes-per-pixel Self)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro WITH-RGBA-BYTE-VECTOR (Vector (Red Green Blue alpha) &body Forms)
    `(let ((,Vector (make-byte-vector ,Red ,Green ,Blue ,Alpha)))
       (unwind-protect
           (progn ,@Forms)
         (dispose-vector ,Vector)))))

        
(defmethod GET-RGBA-COLOR-AT ((Self image-editor) Col Row)
  "Returns the color at the specified pixel location."
  ;; make pixel buffer if needed
  (unless (pixel-buffer Self)
    (setf (pixel-buffer Self) (make-vector-of-size (* (img-width Self) (img-height Self) (image-bytes-per-pixel Self))))
    (with-glcontext Self
      (glBindTexture GL_TEXTURE_2D (img-texture Self))
      (glGetTexImage GL_TEXTURE_2D 0 GL_RGBA GL_UNSIGNED_BYTE (pixel-buffer Self))))
  ;; access pixel
  (let ((Byte-Offset (image-byte-offset Self Col Row)))
    (values
     (get-byte (pixel-buffer Self) Byte-Offset)
     (get-byte (pixel-buffer Self) (+ Byte-Offset 1))
     (get-byte (pixel-buffer Self) (+ Byte-Offset 2))
     (get-byte (pixel-buffer Self) (+ Byte-Offset 3)))))


(defmethod SET-RGBA-COLOR-AT ((Self image-editor) Col Row Red Green Blue &optional (Alpha 255))
  "Sets the color at the specified pixel location."
  ;; update texture
  (when (img-texture Self)
    (with-rgba-byte-vector &color (Red Green Blue Alpha)
      (with-glcontext Self
        (glBindTexture GL_TEXTURE_2D (img-texture Self))
        (glTexSubImage2D GL_TEXTURE_2D 0 Col (- (img-height Self) Row 1) 1 1 GL_RGBA GL_UNSIGNED_BYTE &color)))) 
  ;; update pixel buffer
  (when (pixel-buffer Self)
    (let ((Byte-Offset (image-byte-offset Self Col Row)))
      (when (> Byte-Offset (sizeof (pixel-buffer Self))) (error "out of range"))
      (set-byte (pixel-buffer Self) Red Byte-Offset)
      (set-byte (pixel-buffer Self) Green (+ Byte-Offset 1))
      (set-byte (pixel-buffer Self) Blue (+ Byte-Offset 2))
      (set-byte (pixel-buffer Self) Alpha (+ Byte-Offset 3)))))


(defmethod GET-COLOR-AT ((Self image-editor) Col Row)
  (multiple-value-bind (Red Green Blue Alpha)
                       (get-rgba-color-at Self Col Row)
    (+ Red
       (ash Green 8)
       (ash Blue 16)
       (ash Alpha 24))))


(defmethod SET-COLOR-AT ((Self image-editor) Col Row Color)
  (set-rgba-color-at Self Col Row
   (logand Color 255)
   (logand (ash Color -8) 255)
   (logand (ash Color -16) 255)
   (logand (ash Color -24) 255)))


(defmethod PEN-COLOR ((Self image-editor))
  "Returns the color currently being used to draw or fill."
  (values 
   (get-byte (pen-color-vector Self) 0)
   (get-byte (pen-color-vector Self) 1)
   (get-byte (pen-color-vector Self) 2)
   (get-byte (pen-color-vector Self) 3)))


(defmethod SET-PEN-COLOR ((Self image-editor) Red Green Blue &optional (Alpha 255))
  "Sets the color to be used in subsequent draw or fill."
  (dispose-vector (pen-color-vector Self))
  (setf (pen-color-vector Self) (make-byte-vector Red Green Blue Alpha)))


(defmethod BG-COLOR ((Self image-editor))
  "Returns the color currently used to erase."
  (values
   (get-byte (bg-color-vector Self) 0)
   (get-byte (bg-color-vector Self) 1)
   (get-byte (bg-color-vector Self) 2)
   (get-byte (bg-color-vector Self) 3)))


(defmethod GET-BIGNUM-PEN-COLOR ((Self image-editor))
  (let ((Pen-Color (pen-color-vector Self)))
    (+ (get-byte pen-color 0)
       (ash (get-byte pen-color 1) 8)
       (ash (get-byte pen-color 2) 16)
       (ash (get-byte pen-color 3) 24))))


(defmethod GET-BIGNUM-ERASER-COLOR ((Self image-editor))
  (let ((Eraser-Color (bg-color-vector Self)))
    (+ (get-byte eraser-color 0)
       (ash (get-byte eraser-color 1) 8)
       (ash (get-byte eraser-color 2) 16)
       (ash (get-byte eraser-color 3) 24))))


(defmethod BIGNUM-TO-RGBA ((Self image-editor) Bignum-Color)
  (values
   (logand Bignum-Color 255)
   (logand (ash Bignum-Color -8) 255)
   (logand (ash Bignum-Color -16) 255)
   (logand (ash Bignum-Color -24) 255)))


(defmethod SELECTION-ACTIVE-P ((Self image-editor))
  "Returns t if currently there is an active selection."
  (not (null (selection-outline Self))))


(defmethod DRAW-PIXEL ((Self image-editor) Col Row)
  "Paints the specified pixel with the current pen color."
  (multiple-value-bind (Red Green Blue Alpha) (pen-color Self)
    (if (selection-active-p Self)
        (when (pixel-selected-p (selection-mask Self) Col Row)
          (set-rgba-color-at Self Col Row Red Green Blue Alpha))
      (set-rgba-color-at Self Col Row Red Green Blue Alpha)))
  ;; mirroring
  (mirror-pixel Self Col Row)
  (display Self))


(defmethod ERASE-PIXEL ((Self image-editor) Col Row)
  "Paints the specified pixel with the current background color."
  (multiple-value-bind (Red Green Blue Alpha) (bg-color Self)
    (if (selection-active-p Self)
        (when (pixel-selected-p (selection-mask Self) Col Row)
          (set-rgba-color-at Self Col Row Red Green Blue Alpha))
      (set-rgba-color-at Self Col Row Red Green Blue Alpha)))
  ;; mirroring
  (mirror-pixel Self Col Row)
  (display Self))


(defmethod FILL-SELECTED-PIXELS ((Self image-editor))
  "Paints all selected pixels with the current pen color."
  (when (selection-active-p Self)
    (multiple-value-bind (Red Green Blue Alpha) (pen-color Self)
      (dotimes (Row (img-height Self))
        (dotimes (Col (img-width Self))
          (when (pixel-selected-p (selection-mask Self) Col Row)
            (set-rgba-color-at Self Col Row Red Green Blue Alpha)
            (mirror-pixel Self Col Row)))))
    (display Self)))


(defmethod ERASE-SELECTED-PIXELS ((Self image-editor))
  (when (selection-active-p Self)
    (multiple-value-bind (Red Green Blue Alpha) (bg-color Self)
      (dotimes (Row (img-height Self))
        (dotimes (Col (img-width Self))
          (when (pixel-selected-p (selection-mask Self) Col Row)
            (set-rgba-color-at Self Col Row Red Green Blue Alpha)
            (mirror-pixel Self Col Row)))))
    (display Self)))


(defmethod ERASE-ALL ((Self image-editor))
  (dotimes (X (img-width Self))
    (dotimes (Y (img-height Self))
      (multiple-value-bind (Red Green Blue Alpha) (bg-color Self)
        (set-rgba-color-at Self x y Red Green Blue Alpha))))
  (display Self))


(defmethod DRAW-BACKGROUND-TEXTURE ((Self image-editor))
  "Draw the background texture image."
  (unless (bg-texture Self) (load-background-texture Self))
  (glEnable GL_TEXTURE_2D)
  (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
  (glBindTexture GL_TEXTURE_2D (bg-texture Self))
  (glBegin GL_QUADS)
  (glnormal3f 0.0 0.0 -1.0)
  ;; default camera scales only when view is resized vertically
  ;; 8 gray + 8 white pixels
  (let ((Tdy (float (/ (height Self) 16) 0.0))
        (Tdx (float (/ (height Self) (/ (* 16 (canvas-height Self)) (canvas-width Self))) 0.0)))
    (glTexCoord2f 0.0 Tdy) (glVertex2f 0.0 (canvas-height Self))
    (glTexCoord2f 0.0 0.0) (glVertex2f 0.0 0.0)
    (glTexCoord2f Tdx 0.0) (glVertex2f  (canvas-width Self) 0.0)
    (glTexCoord2f Tdx Tdy) (glVertex2f  (canvas-width Self) (canvas-height Self)))
  (glEnd))


(defmethod DRAW-TEXTURE-IMAGE ((Self image-editor))
  "Draw the texture image."
  (when (img-texture Self)
    (glEnable GL_TEXTURE_2D)
    (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
    (glBindTexture GL_TEXTURE_2D (img-texture Self))
    (glBegin GL_QUADS)
    (glNormal3f 0.0 0.0 -1.0)
    (glTexCoord2f 0.0 1.0) (glVertex2f 0.0 (canvas-height Self))
    (glTexCoord2f 0.0 0.0) (glVertex2f 0.0 0.0)
    (glTexCoord2f 1.0 0.0) (glVertex2f (canvas-width Self) 0.0)
    (glTexCoord2f 1.0 1.0) (glVertex2f (canvas-width Self) (canvas-height Self))
    (glEnd)))


(defmethod DRAW-HORIZONTAL-GUIDE-LINE ((Self image-editor))
  (glLineWidth 2.0)
  (glBegin GL_LINES)
  (glColor3f 0.15 0.65 1.0)
  (glVertex2f 0.0 (/ (canvas-height Self) 2.0))
  (glVertex2f  (canvas-width Self)  (/ (canvas-height Self) 2.0))
  (glEnd)
  (glLineWidth 1.0))


(defmethod DRAW-VERTICAL-GUIDE-LINE ((Self image-editor))
  (glLineWidth 2.0)
  (glBegin GL_LINES)
  (glColor3f 0.15 0.65 1.0)
  (glVertex2f (/ (canvas-width Self) 2.0) 0.0)
  (glVertex2f (/ (canvas-width Self) 2.0)  (canvas-height Self))
  (glEnd)
  (glLineWidth 1.0))


(defmethod DRAW-GRID ((Self image-editor)) 
  (glBegin GL_LINES)
  (glColor3f 0.5 0.5 0.5)
  (let ((Grid-Spacing (/ (canvas-width Self) (img-width Self))))
    ;; horizontal
    (dotimes (I (img-height Self))
      (let ((Grid-Position (* i Grid-Spacing)))
        (glVertex2f 0.0 Grid-Position)
        (glVertex2f (canvas-width Self) Grid-Position)))
    ;; vertical
    (dotimes (I (img-width Self))
      (let ((Grid-Position (* i Grid-Spacing)))
        (glVertex2f Grid-Position 0.0)
        (glVertex2f Grid-Position (canvas-height Self))))
    (glEnd)))


(defmethod DRAW-GUIDE-LINES ((Self image-editor))
  (glDisable GL_LINE_STIPPLE)
  (if (is-grid-on Self) (draw-grid Self))
  (if (is-horizontal-line-on Self) (draw-horizontal-guide-line Self))
  (if (is-vertical-line-on Self)   (draw-vertical-guide-line Self))
  (glEnable GL_LINE_STIPPLE))
  

(defmethod IMAGE-CHANGED-EVENT ((Sefl image-editor) &optional Column1 Row1 Column2 Row2)
  (declare (ignore Row1 Column1 Row2 Column2))
  ;; what to do: set window changed to true maybe?
  ;; nop for now
  )

;_______________________________
; Grid and Mirroring            |
;_______________________________/

(defmethod TOGGLE-GRID ((Self image-editor) Is-Visible)
  (setf (is-grid-on Self) Is-Visible)
  (display Self))


(defmethod TOGGLE-MIRROR-LINES ((Self image-editor) Horizontal-Is-Visible Vertical-Is-Visible)
  (setf (is-horizontal-line-on Self) Horizontal-Is-visible)
  (setf (is-vertical-line-on Self) Vertical-Is-visible)
  (if Horizontal-Is-visible
      (mirror-image-top-to-bottom Self))
  (if Vertical-Is-visible
      (mirror-image-left-to-right Self))
  (display Self))


(defmethod MIRROR-IMAGE-LEFT-TO-RIGHT ((Self image-editor))
  (dotimes (X (/ (img-width Self) 2))
    (dotimes (Y (img-height Self))
      (set-color-at Self (- (img-width Self) (+ X 1)) Y (get-color-at Self X Y))))
  (display Self))


(defmethod MIRROR-IMAGE-TOP-TO-BOTTOM ((Self image-editor))
  (dotimes (X (img-width Self))
    (dotimes (Y (/ (img-height Self) 2))
      (set-color-at Self X (- (img-height Self) (+ Y 1)) (get-color-at Self X Y))))
  (display Self))


(defmethod MIRROR-PIXEL-VERTICALLY ((Self image-editor) Col Row)
  (multiple-value-bind (r g b a) (get-rgba-color-at Self Col Row)
    (set-rgba-color-at Self (- (- (img-width Self) 1) Col) Row r g b a)))


(defmethod MIRROR-PIXEL-HORIZONTALLY ((Self image-editor) Col Row)
  (multiple-value-bind (r g b a) (get-rgba-color-at Self Col Row)
    (set-rgba-color-at Self Col (- (- (img-height Self) 1) Row) r g b a)))


(defmethod MIRROR-PIXEL-DIAGONALLY ((Self image-editor) Col Row)
  (multiple-value-bind (r g b a) (get-rgba-color-at Self Col Row)
    (set-rgba-color-at Self (- (- (img-width Self) 1) Col) (- (- (img-height Self) 1) Row) r g b a)))


(defmethod MIRROR-PIXEL ((Self image-editor) Col Row)
  (let ((Horizontal-Mirror (is-horizontal-line-on Self))
        (Vertical-Mirror (is-vertical-line-on   Self)))
    (if horizontal-mirror (mirror-pixel-horizontally Self Col Row))
    (if vertical-mirror   (mirror-pixel-vertically Self Col Row))
    (if (and horizontal-mirror vertical-mirror)
        (mirror-pixel-diagonally Self Col Row))))

;; Mirroring selections
(defmethod MIRROR-RECT-OR-ELLIPSE-SELECTION-IN-PROGRESS ((Self image-editor) Left Top Right Bottom Shape-Function)
  (let ((Mirror-Right  (- (img-width Self) Left))
        (Mirror-Bottom (- (img-height Self) Top))
        (Mirror-Left   (- (img-width Self) Right))
        (Mirror-Top    (- (img-height Self) Bottom))
        (Horizontal-Mirror (is-horizontal-line-on Self))
        (Vertical-Mirror (is-vertical-line-on   Self)))
    (if Horizontal-Mirror
        (funcall shape-function Self Left Mirror-Top Right Mirror-Bottom))
    (if Vertical-Mirror
        (funcall shape-function Self Mirror-Left Top Mirror-Right Bottom))
    (if (and Horizontal-Mirror Vertical-Mirror)
        (funcall shape-function Self Mirror-Left Mirror-Top Mirror-Right Mirror-Bottom))))


(defmethod MIRROR-POLYGON-SELECTION-HORIZONTALLY ((Self image-editor) Vertices)
  (let ((Width (img-width Self)))
    (dolist (V Vertices)
      (setf (car V) (- Width (car V))))))


(defmethod MIRROR-POLYGON-SELECTION-VERTICALLY ((Self image-editor) Vertices)
  (let ((Height (img-height Self)))
    (dolist (V Vertices)
      (setf (car (cdr V)) (- Height (car (cdr V)))))))


(defmethod MIRROR-POLYGON-SELECTION-DIAGONALLY ((Self image-editor) Vertices)
  (let ((Width (img-width Self))
        (Height (img-height Self)))
    (dolist (V Vertices)
      (setf (car V) (- Width (car V)))
      (setf (car (cdr V)) (- Height (car (cdr V)))))))


(defmethod MIRROR-POLYGON-SELECTION-IN-PROGRESS ((Self image-editor) Vertices) 
  (let ((Horizontal-Mirror (is-horizontal-line-on Self))
        (Vertical-Mirror (is-vertical-line-on   Self))
        (Return-Vertices Vertices))
    (if Horizontal-Mirror
        (setf Return-Vertices 
          (append Return-Vertices 
                  (mirror-polygon-selection-horizontally Self Vertices))))
    (if Vertical-Mirror
        (setf Return-Vertices 
          (append Return-Vertices 
                  (mirror-polygon-selection-vertically Self Vertices))))
    (if (and Horizontal-Mirror Vertical-Mirror)
        (setf Return-Vertices 
          (append Return-Vertices 
                  (mirror-polygon-selection-diagonally Self Vertices))))
    (return-from mirror-polygon-selection-in-progress Return-Vertices)))


;_______________________________
; Selections                    |
;_______________________________/

(defmethod SCREEN->PIXEL-COORD ((Self image-editor) x y)
  "Converts a point in screen coordinate into a pixel coordinate.
   If there is no valid pixel coordinate, i.e., outside of image then return nil nil"
  (let ((Col (floor (/ (* (- x (truncate (- (width Self) (* (/ (height Self) (canvas-height Self)) (canvas-width Self))) 2))
                          (img-width Self)) (* (/ (height Self) (canvas-height Self)) (canvas-width Self)))))
        (Row (floor (* (img-height Self) (/ y (height Self))))))
    (values
     (when (<= 0 Col (1- (img-width Self))) Col)
     (when (<= 0 Row (1- (img-height Self))) Row))))


(defmethod PIXEL->WORLD-COORD ((Self image-editor) Col Row)
  "Converts from pixel coordinate into OpenGL world coordinate."
  (values 
   (* (/ (canvas-width Self) (img-width Self)) Col)
   (- (canvas-height Self) (* (/ (canvas-height Self) (img-height Self)) Row))))


(defmethod DRAW-SELECTION ((Self image-editor))
  "Displays the selection with marching ants effect."
  (flet ((draw-selection-outline ()
           (dolist (Cluster (selection-outline Self))
             (glBegin GL_LINE_LOOP)
             (dolist (Segment Cluster)
               (glVertex2f (x1 Segment) (y1 Segment)))
             (glEnd))))
    (glEnable GL_LINE_STIPPLE)
    (glLogicOp GL_COPY)
    (glLineStipple 1 (stipple Self))
    (glColor3f 0.0 0.0 0.0)
    (draw-selection-outline)
    (glLineStipple 1 (logand #xFFFF (lognot (stipple Self))))
    (glColor3f 1.0 1.0 1.0)
    (draw-selection-outline)))


(defmethod DRAW-RECT-SELECTION-FEEDBACK ((Self image-editor) X1 Y1 X2 Y2)
  "Displays visual feedback for a rectangular selection in progress."
  (multiple-value-bind (Xw1 yw1) (pixel->world-coord Self X1 Y1)
    (multiple-value-bind (Xw2 Yw2) (pixel->world-coord Self X2 Y2)
      (glEnable GL_LINE_STIPPLE)
      (glEnable GL_COLOR_LOGIC_OP)
      (glLogicOp GL_XOR)
      (glLineStipple 1 (stipple Self))
      (glBegin GL_LINE_LOOP)
      (glcolor3f 1.0 1.0 1.0)
      (glVertex2f xw1 yw1)
      (glVertex2f xw1 yw2)
      (glVertex2f xw2 yw2)
      (glVertex2f xw2 yw1)
      (glEnd)
      (glDisable GL_COLOR_LOGIC_OP))))


(defmethod DRAW-ELLIPSE-SELECTION-FEEDBACK ((Self image-editor) X1 Y1 X2 Y2)
  "Displays visual feedback for an elliptical selection in progress."
  (multiple-value-bind (Xw1 yw1) (pixel->world-coord Self X1 Y1)
    (multiple-value-bind (Xw2 Yw2) (pixel->world-coord Self X2 Y2)
      (glEnable GL_LINE_STIPPLE)
      (glEnable GL_COLOR_LOGIC_OP)
      (glLogicOp GL_XOR)
      (glLineStipple 1 (stipple Self))
      ;; draw a polygon with many segments to approximate an ellipse
      (let* ((Segments 50)
             (X0 (/ (+ xw1 xw2) 2.0))
             (Y0 (/ (+ yw1 yw2) 2.0))
             (Rx (/ (- xw2 xw1) 2.0))
             (Ry (/ (- yw2 yw1) 2.0))
             (Dangle (float (/ (* 2 pi) Segments) 0s0))
             (Angle 0.0))
        ;; not very efficient but we only need one
        (glBegin GL_LINE_LOOP)
        (glVertex2f (+ x0 rx) y0)
        (dotimes (I Segments)
          (glVertex2f (+ x0 (* (cos Angle) Rx)) (+ y0 (* (sin Angle) Ry)))
          (incf Angle dAngle))
        (glEnd)
        (glDisable GL_COLOR_LOGIC_OP)))))


(defmethod DRAW-POLYGON-SELECTION-FEEDBACK ((Self image-editor) Vertices)
  "Displays visual feedback for a polygonal selection in progress."
  (glEnable GL_LINE_STIPPLE)
  (glEnable GL_COLOR_LOGIC_OP)
  (glLogicOp GL_XOR)
  (glLineStipple 1 (stipple Self))
  (glBegin GL_LINE_STRIP)
  (glcolor3f 1.0 1.0 1.0)
  (dolist (V Vertices)
    (multiple-value-bind (vx vy) (pixel->world-coord Self (first v) (second v))
      (glVertex2f vx vy)))
  (multiple-value-bind (vx vy) (pixel->world-coord Self (hovered-pixel-col Self) (hovered-pixel-row Self))
    (glVertex2f vx vy))
  (glEnd)
  (glDisable GL_COLOR_LOGIC_OP))


(defmethod DRAW-SELECTION-IN-PROGRESS-FEEDBACK ((Self image-editor))
  "Displays visual feedback for a selection currently in progress if there is one."
  (when (selection-in-progress Self)
    (case (first (selection-in-progress Self))
      (:rect 
       (let ((Left   (second (selection-in-progress Self)))
             (Top    (third (selection-in-progress Self)))
             (Right  (fourth (selection-in-progress Self)))
             (Bottom (fifth (selection-in-progress Self))))
         (draw-rect-selection-feedback Self Left Top Right Bottom)
         (mirror-rect-or-ellipse-selection-in-progress Self Left Top Right Bottom #'draw-rect-selection-feedback)))
      (:ellipse 
       (let ((Left   (second (selection-in-progress Self)))
             (Top    (third (selection-in-progress Self)))
             (Right  (fourth (selection-in-progress Self)))
             (Bottom (fifth (selection-in-progress Self))))
         (draw-ellipse-selection-feedback Self Left Top Right Bottom)
         (mirror-rect-or-ellipse-selection-in-progress Self Left Top Right Bottom #'draw-ellipse-selection-feedback)))
      ;; Do not have the mirroring working for this yet
      (:polygon (draw-polygon-selection-feedback
                 Self (rest (selection-in-progress Self)))))))


(defmethod DRAW ((Self image-editor))
  "Called when the image-editor needs to display its contents.
   Specialized to draw texture image, selection, and feedback for selection in progress."
  (draw-background-texture Self)
  (draw-texture-image Self)
  (glDisable GL_TEXTURE_2D)
  (draw-guide-lines Self)
  (draw-selection Self)
  (draw-selection-in-progress-feedback Self)
  )


(defmethod RECOMPUTE-SELECTION-OUTLINE ((Self image-editor))
  "Returns clusters of line segments that form the current selection edges."
  (let ((Pixel-Width (/ (canvas-width Self)  (img-width Self)))
        (Pixel-Height (/ (canvas-height Self) (img-height Self)))
        (Lines nil))
    (dotimes (Row (img-height Self))
      (dotimes (Col (img-width Self))
        (unless (zerop (pixel-at (selection-mask Self) Col Row))
          (multiple-value-bind (px py) (pixel->world-coord Self Col Row)
            ;; top
            (when (zerop (pixel-at (selection-mask Self) Col (1- Row)))
              (push (make-instance 'line-segment :x2 px :y2 py :x1 (+ px Pixel-Width) :y1 py)
                    Lines))
            ;; left
            (when (zerop (pixel-at (selection-mask Self) (1- Col) Row))
              (push (make-instance 'line-segment :x2 px :y2 (- py Pixel-Height) :x1 px :y1 py)
                    Lines))
            ;; bottom
            (when (zerop (pixel-at (selection-mask Self) Col (1+ Row)))
              (push (make-instance 'line-segment :x2 (+ px Pixel-Width) :y2 (- py Pixel-Height)
                      :x1 px :y1 (- py Pixel-Height)) Lines))
            ;; right
            (when (zerop (pixel-at (selection-mask Self) (1+ Col) Row))
              (push (make-instance 'line-segment :x2 (+ px Pixel-Width) :y2 py
                      :x1 (+ px Pixel-Width) :y1 (- py Pixel-Height))
                    Lines))))))
    (group-line-segments (reverse Lines))))


(defmethod CLEAR-SELECTION ((Self image-editor))
  "Clears all selection."
  (unselect-all (selection-mask Self))
  (setf (selection-outline Self) nil)
  (display Self))


(defmethod UPDATE-SELECTION ((Self image-editor) New-Selection)
  "Updates the selection mask and recomputes the selection outline."
  (if New-Selection
      (destructuring-bind (Shape &rest Shape-Specs) New-Selection
        ;; update selection-mask
        (if (alt-key-p)
            (if (shift-key-p)
                (apply #'intersect-selection (selection-mask Self) Shape Shape-Specs)
              (apply #'subtract-selection (selection-mask Self) Shape Shape-Specs))
          (apply #'add-selection (selection-mask Self) Shape Shape-Specs))
        ;; update selection-outline
        (setf (selection-outline Self) (recompute-selection-outline Self)))
    (clear-selection Self)))


(defmethod SELECT-ALL ((Self image-editor))
  "Selects the whole image."
  (select-all (selection-mask Self))
  (setf (selection-outline Self) (recompute-selection-outline Self))
  (display Self))


(defmethod INVERT-SELECTION ((Self image-editor))
  "Inverts the current selection."
  (invert-selection (selection-mask Self))
  (setf (selection-outline Self) (recompute-selection-outline Self))
  (display Self))


(defmethod ANIMATE ((Self image-editor) Time)
  "Called periodically by window-null-event-handler to update the line stipple to create
   the marching ants effect."
  (declare (ignore Time))
  (setf (stipple Self) (logand (logior (ash (stipple Self) 1) (if (logbitp 15 (stipple Self)) 1 0)) #xFFFF)))


(defmethod REMOVE-VIEW-FROM-WINDOW ((Self image-editor))
  "Called when the image-editor is removed from its parent window or when the parent
   window is closed. Specialized to deallocate native resources."
  (call-next-method)
  (dispose-texture-image Self)
  (dispose-background-texture Self)
  (dispose-vector (pen-color-vector Self))
  (dispose-vector (bg-color-vector Self))
  (when (pixel-buffer Self) (dispose-vector (pixel-buffer Self))))

;_______________________________
; Tools                         |
;_______________________________/

(defmethod EYE-DROPPER ((Self image-editor) Col Row)
  (when (and (img-texture Self) (col-row-within-bounds-p Self Col Row))
    (multiple-value-bind (Red Green Blue Alpha) (get-rgba-color-at Self Col Row)
      (set-pen-color Self Red Green Blue Alpha))))


(defmethod FLOOD-FILL ((Self image-editor) Col Row 
                       New-Red New-Green New-Blue New-Alpha 
                       &key (Orig-Red -1) Orig-Green Orig-Blue Orig-Alpha Tolerance (Tolerance-Function nil))
  "This function uses the flood fill method to paint the nodes which are of the same color as the original node
   to the new color. For the key arguments, the caller should only specify the tolerance-function to use."
  ;; Verify that the point given by Row and Col is within bounds
  (when (and 
         (< Col (img-width Self))  (>= Col 0)
         (< Row (img-height Self)) (>= Row 0))
    ;; If there is a selection and the pixel is not in that selection, exit:
    (if (selection-active-p Self)
        (if (not (pixel-selected-p (selection-mask Self) Col Row))
            (return-from flood-fill nil)))
    ;; Set Current-Pixel-Color to the current pixel's color and set
    ;; first iteration to false
    (multiple-value-bind (Cur-Red Cur-Green Cur-Blue Cur-Alpha) (get-rgba-color-at Self Col Row)
      (let ((First-Iteration nil))
        ;; When Orig-Red is -1 it means this is the first iteration, so set Original-Colors
        ;; to the current pixel's color and set First-Iteration to true
        (when (= Orig-Red -1)
          (setf Orig-Red Cur-Red) (setf Orig-Green Cur-Green)
          (setf Orig-Blue Cur-Blue) (setf Orig-Alpha Cur-Alpha)
          ;; To save computing time, if the new color is the same as the original color, exit
          (when (and
                 (= Orig-Red New-Red) (= Orig-Green New-Green)
                 (= Orig-Blue New-Blue) (= Orig-Alpha New-Alpha))
            (return-from flood-fill nil))
          (setf First-Iteration t))
        ;; Check to see if the Current color is within the tolerance
        ;;  of the original color
        (when (if (and Tolerance-Function
                       (not (and (= New-Red Cur-Red) (= New-Green Cur-Green)
                                 (= New-Blue Cur-Blue) (= New-Alpha Cur-Alpha))))
                  (funcall Tolerance-Function Tolerance
                           Orig-Red Orig-Green Orig-Blue Orig-Alpha
                           Cur-Red Cur-Green Cur-Blue Cur-Alpha)
                (and (= Cur-Red Orig-Red) (= Cur-Green Orig-Green)
                     (= Cur-Blue Orig-Blue) (= Cur-Alpha Orig-Alpha)))
          (set-rgba-color-at Self Col Row New-Red New-Green New-Blue New-Alpha)
          ;; Mirror the new pixel if needed
          (mirror-pixel Self Col Row)
          
          ;; Call neighbors
          (flood-fill Self Col (- Row 1) 
                      New-Red New-Green New-Blue New-Alpha 
                      :orig-red Orig-Red :orig-green Orig-Green 
                      :orig-blue Orig-Blue :orig-alpha Orig-Alpha 
                      :tolerance Tolerance :tolerance-function Tolerance-Function)
          (flood-fill Self Col (+ Row 1) 
                      New-Red New-Green New-Blue New-Alpha 
                      :orig-red Orig-Red :orig-green Orig-Green 
                      :orig-blue Orig-Blue :orig-alpha Orig-Alpha 
                      :tolerance Tolerance :tolerance-function Tolerance-Function)
          (flood-fill Self (+ Col 1) Row 
                      New-Red New-Green New-Blue New-Alpha 
                      :orig-red Orig-Red :orig-green Orig-Green 
                      :orig-blue Orig-Blue :orig-alpha Orig-Alpha 
                      :tolerance Tolerance :tolerance-function Tolerance-Function)
          (flood-fill Self (- Col 1) Row 
                      New-Red New-Green New-Blue New-Alpha 
                      :orig-red Orig-Red :orig-green Orig-Green 
                      :orig-blue Orig-Blue :orig-alpha Orig-Alpha 
                      :tolerance Tolerance :tolerance-function Tolerance-Function)
          ;; Only when exiting this function for the last time (when the first iteration is done)
          ;; update the scene
          (when First-Iteration
            (display Self)))))))


(defmethod MAGIC-WAND ((Self image-editor) Col Row  
                       &key (Orig-Red -1) Orig-Green Orig-Blue Orig-Alpha Tolerance (Tolerance-Function nil))
  "This function selects the pixels around the original pixel which are of the same color as the original color.
   For the key arguments, the caller should only specify the tolerance-function to use"
  (when (and 
         ;; Make sure the pixel is within bounds and that there is an image
         (< Col (img-width Self))  (>= Col 0)
         (< Row (img-height Self)) (>= Row 0)
         (img-texture Self)
         ;; If the pixel is in the selection, exit
         (not (pixel-selected-p (selection-mask Self) Col Row)))
    ;; If this is the first call, set the original-color to the current pixel's color
    (let ((First-Iteration nil))
      (multiple-value-bind (Cur-Red Cur-Green Cur-Blue Cur-Alpha) (get-rgba-color-at Self Col Row)
        ;; When Orig-Red is -1 that means this is the first iteration
        (when (= Orig-Red -1)
          ;; Start stippling and setthe orig color to the current pixels color and set first-iteration to true
          (glEnable gl_line_stipple)
          (setf Orig-Red Cur-Red) (setf Orig-Green Cur-Green)
          (setf Orig-Blue Cur-Blue) (setf Orig-Alpha Cur-Alpha)
          (setf First-Iteration t))
        ;; Test the tolerance using the given tolerance function
        (when (if Tolerance-Function
                  (funcall Tolerance-Function Tolerance
                           Orig-Red Orig-Green Orig-Blue Orig-Alpha
                           Cur-Red Cur-Green Cur-Blue Cur-Alpha)
                (and (= Cur-Red Orig-Red) (= Cur-Green Orig-Green)
                     (= Cur-Blue Orig-Blue) (= Cur-Alpha Orig-Alpha)))
          
          ;; Put the pixel into the selection and then call the four neighbors
          (select-pixel (selection-mask Self) Col Row)
          (magic-wand Self Col (- Row 1) 
                      :orig-red Orig-Red :orig-green Orig-Green 
                      :orig-blue Orig-Blue :orig-alpha Orig-Alpha 
                      :tolerance Tolerance :tolerance-function tolerance-function)
          (magic-wand Self Col (+ Row 1) 
                      :orig-red Orig-Red :orig-green Orig-Green 
                      :orig-blue Orig-Blue :orig-alpha Orig-Alpha 
                      :tolerance Tolerance :tolerance-function tolerance-function)
          (magic-wand Self (+ Col 1) Row
                      :orig-red Orig-Red :orig-green Orig-Green 
                      :orig-blue Orig-Blue :orig-alpha Orig-Alpha 
                      :tolerance Tolerance :tolerance-function tolerance-function)
          (magic-wand Self (- Col 1) Row 
                      :orig-red Orig-Red :orig-green Orig-Green 
                      :orig-blue Orig-Blue :orig-alpha Orig-Alpha 
                      :tolerance Tolerance :tolerance-function tolerance-function)))
      ;; If this is the end of the first iteration, recompute the outline so that the selection
      ;; can be displayed
      (if First-Iteration (setf (selection-outline Self) (recompute-selection-outline Self))))))

;_______________________________
; Tolerance Functions           |
;_______________________________/

(defun ABSOLUTE-COLOR-DIFFERENCE (Tolerance Orig-Red Orig-Green Orig-Blue Orig-Alpha
                                            Cur-Red Cur-Green Cur-Blue Cur-Alpha)
  "Example tolerance-function"
  (if (>= Tolerance (+ (abs (- Orig-Red   Cur-Red))
                       (abs (- Orig-Green Cur-Green))
                       (abs (- Orig-Blue  Cur-Blue))
                       (abs (- Orig-Alpha Cur-Alpha))))
      (return-from absolute-color-difference t)
    (return-from absolute-color-difference nil)))


(defun ABSOLUTE-COLOR-DIFFERENCE-IGNORE-ALPHA (Tolerance Orig-Red Orig-Green Orig-Blue Orig-Alpha Cur-Red Cur-Green Cur-Blue Cur-Alpha)
  "Example tolerance-function: Returns true when the orig-alpha value is less then 255 and the cur-alpha
   is also less than 255. It also returns true when the red, green, and blue's difference is less then
   the tolerance. Returns nil otherwise"
  (if (< Orig-Alpha 255)
      (if (< Cur-Alpha 255)
          (return-from absolute-color-difference-ignore-alpha t))
    (if (and (= Cur-Alpha 255)
             (>= Tolerance (+ (abs (- Orig-Red   Cur-Red))
                              (abs (- Orig-Green Cur-Green))
                              (abs (- Orig-Blue  Cur-Blue)))))
        (return-from absolute-color-difference-ignore-alpha t)))
  (return-from absolute-color-difference-ignore-alpha nil))

;_______________________________
; Mouse  Handlers                |
;_______________________________/

(defmethod CLICK-OR-DRAG-PIXEL ((Self image-editor) Col Row Dragged)
  (case (selected-tool (window Self))
    ;; DRAW
    (draw 
     (draw-pixel Self Col Row))
    ;; ERASE
    (erase
     (erase-pixel Self Col Row))
    ;; DROPPER
    (eye-dropper
     (eye-dropper Self Col Row)
     (multiple-value-bind (Red Green Blue Alpha) (get-rgba-color-at Self Col Row)
       (set-color (view-named (window Self) "color well") 
                  :red (/ Red 255.0) :green (/ Green 255.0) :blue (/ Blue 255.0) :alpha (/ Alpha 255.0))))
    ;; FLOOD FILL
    (paint-bucket 
     (multiple-value-bind (New-Red New-Green New-Blue New-Alpha) (pen-color Self)
       (flood-fill Self Col Row New-Red New-Green New-Blue New-Alpha
                   :tolerance (tolerance Self) :tolerance-function #'absolute-color-difference-ignore-alpha)))
    ;; MAGIC WAND
    (magic-wand
     ;; When not holding shift, clear the selection
     (unless (shift-key-p)
       (clear-selection Self))
     ;; When there is not a double click, make a selection, otherwise exit
     (magic-wand Self Col Row :tolerance (tolerance Self) :tolerance-function #'absolute-color-difference-ignore-alpha)
     (display Self))
    ;; Select-Rect
    (select-rect
     (cond
      ;; first click
      ((not Dragged)
       (unless (shift-key-p) 
         (clear-selection Self)
         (display Self))
       (setf (selection-in-progress Self) (list :rect Col Row (+ Col 1) (+ Row 1))))
      ;; dragging
      (Dragged
       (setf (fourth (selection-in-progress Self)) (1+ Col))
       (setf (fifth (selection-in-progress Self)) (1+ Row))
       (display Self))))
    ;; SELECT ELLIPSE
    (select-ellipse
     (cond
      ;; first click
      ((not Dragged)
       (unless (shift-key-p) 
         (clear-selection Self)
         (display Self))
       (setf (selection-in-progress Self) (list :ellipse Col Row (+ Col 1) (+ Row 1))))
      ;; dragging
      (Dragged
       (setf (fourth (selection-in-progress Self)) (1+ Col))
       (setf (fifth (selection-in-progress Self)) (1+ Row))
       (display Self))))
    ;; Select Polygon
    (select-polygon
     (cond
      ;; starting
      ((or (not (selection-in-progress Self)) (member (first (selection-in-progress Self)) '(:rect :ellipse)))
       (unless (shift-key-p) 
         (clear-selection Self)
         (display Self))
       (setf (selection-in-progress Self) (list :polygon (list Col Row))))
      ;; continue
      (t
       (cond
        ;; done
        ((double-click-p)
         (update-selection Self (selection-in-progress Self))
         (setf (selection-in-progress Self) nil)
         (display Self))
        (t
         (setf (selection-in-progress Self) (append (selection-in-progress Self) (list (list Col Row)))))))))))



(defmethod VIEW-LEFT-MOUSE-DOWN-EVENT-HANDLER ((Self image-editor) x y)
  "Called when the image-editor is clicked. Specialized to implement mouse handling for 
   the currently selected tool."
  (unless (img-texture Self) (return-from view-left-mouse-down-event-handler))
  (multiple-value-bind (Col Row) (screen->pixel-coord Self x y)
    (when (and Row Col)
      (click-or-drag-pixel Self Col Row nil))))


(defmethod VIEW-LEFT-MOUSE-DRAGGED-EVENT-HANDLER ((Self image-editor) X Y DX DY)
  (declare (ignore DX DY))
  (unless (img-texture Self) (return-from view-left-mouse-dragged-event-handler))
  (multiple-value-bind (Col Row) (screen->pixel-coord Self x y)
    (when (and Row Col)
      (click-or-drag-pixel Self Col Row t))))


(defmethod VIEW-MOUSE-MOVED-EVENT-HANDLER ((Self image-editor) x y dx dy)
  (declare (ignore DX DY))
  (unless (img-texture Self) (return-from view-mouse-moved-event-handler))
  (multiple-value-bind (Col Row) (screen->pixel-coord Self x y)
    (when (and Row Col)
      (setf (hovered-pixel-col Self) Col)
      (setf (hovered-pixel-row Self) Row)
      ;; there may be a need to display some feedback, slighly excessive 
      (display Self))))


(defmethod VIEW-LEFT-MOUSE-UP-EVENT-HANDLER ((Self image-editor) x y)
  (unless (img-texture Self) (return-from view-left-mouse-up-event-handler))
  (multiple-value-bind (Col Row) (screen->pixel-coord Self x y)
    (when (and Row Col)
      (case (selected-tool (window Self))
        ;; SELECT RECTANGLE
        (select-rect
         (update-selection Self (selection-in-progress Self))
         (setf (selection-in-progress Self) nil)
         (display Self))
        ;; SELECT ELLIPSE
        (select-ellipse 
         (update-selection Self (selection-in-progress Self))
         (setf (selection-in-progress Self) nil)
         (display Self))))))


;**************************************
;* Image-Editor-Window                *
;**************************************

(defclass IMAGE-EDITOR-WINDOW (application-window)
  ((selected-tool :accessor selected-tool :initform nil :type symbol :initarg :selected-tool :documentation "the name of the currently selected tool")
   (image-editor-view :accessor image-editor-view :initform nil :initarg :image-editor-view :documentation "the image editor view"))
  (:default-initargs 
    :selected-tool 'draw)
  (:documentation "Window containing an image-editor view."))


(defmethod LOAD-IMAGE-FROM-FILE ((Self image-editor-window) Pathname)
  "Loads the specified image file into the editor window."
  (load-image (image-editor-view Self) Pathname)
  (setf (file Self) Pathname))


(defmethod SAVE-IMAGE-TO-FILE ((Self image-editor-window) Pathname)
  "Saves the current image in the editor window to the specified file."
  (save-image (image-editor-view Self) Pathname))


(defmethod VIEW-KEY-EVENT-HANDLER ((Self image-editor-window) Key)
 (print key)
  "Called when a key is typed while the image-editor-window has keyboard focus."
  (cond
   ((command-key-p)
    (case Key
      (#\l (load-image-from-file Self (choose-file-dialog :directory "ad3d:resources;textures;")))
      (#\s (save-image-to-file Self (choose-new-file-dialog)))
      (#\a (select-all (image-editor-view Self)))
      (#\d (clear-selection (image-editor-view Self)))
      (#\I (invert-selection (image-editor-view Self)))))
   (t
    (case Key
      (#\Delete 
       (cond
        ((option-key-p)
         (fill-selected-pixels (image-editor-view Self))
         (image-changed-event (image-editor-view Self)))
        (t
         (erase-selected-pixels (image-editor-view Self))
         (image-changed-event (image-editor-view Self)))))))))


(defmethod DOCUMENT-TYPE-NAME ((Self image-editor-window))
  "Image")


(defmethod DOCUMENT-TYPE-FILE-EXTENSION ((Self image-editor-window))
  "png")


(defmethod WINDOW-SAVE-AS ((Self image-editor-window) &optional External-Format)
  (declare (ignore External-Format))
  (let ((File (choose-new-file-dialog 
               :prompt nil
               :name (format nil "untitled.~A" (document-type-file-extension Self))
               :window-title (format nil "Save ~A As:" (document-type-name Self))
               :directory (document-default-directory Self))))
    (save-image-to-file Self File)
    (add-window-proxy-icon Self File)
    ;; set file to this new file
    (setf (file Self) File)
    (setf (window-needs-saving-p Self) nil)))


(defmethod WINDOW-SAVE-COPY-AS ((Self image-editor-window) &optional File)
  (declare (ignore File))
  ;; do not change saved status or make this the new file
  (let ((File (choose-new-file-dialog 
               :prompt nil
               :name (format nil "untitled.~A" (document-type-file-extension Self))
               :window-title (format nil "Save ~A As:" (document-type-name Self))
               :directory (document-default-directory Self))))
    (save-image-to-file Self File)))


(defmethod WINDOW-SAVE ((Self image-editor-window))
  (cond
   ;; overwrite existing files
   ((file Self)
    (let ((File-Existed-Before-Save (probe-file (file Self))))
      (save-image-to-file Self (file Self))
      (unless File-Existed-Before-Save (add-window-proxy-icon Self (file Self))))
    (setf (window-needs-saving-p Self) nil))
   ;; ask user
   (t
    (window-save-as Self))))

  
;**************************************
;* GUI                                *
;**************************************

;; Control Actions


(defmethod PICK-COLOR-ACTION ((w window) (Color-Well color-well))
  (set-pen-color (view-named w "image editor") (get-red Color-Well) (get-green Color-Well) (get-blue Color-Well) (get-alpha Color-Well)))


(defmethod DRAW-TOOL-ACTION ((W window) (Button image-button))
  (set-cursor "CopyArrow")
  (setf (selected-tool W) 'draw))


(defmethod ERASE-TOOL-ACTION ((W window) (Button image-button))
  (set-cursor "NotAllowed")
  (setf (selected-tool W) 'erase))


(defmethod EYE-DROPPER-TOOL-ACTION ((W window) (Button image-button))
  (setf (selected-tool W) 'eye-dropper))


(defmethod PAINT-BUCKET-TOOL-ACTION ((W window) (Button image-button))
  (setf (selected-tool W) 'paint-bucket))


(defmethod MAGIC-WAND-TOOL-ACTION ((W window) (Button image-button))
  (setf (selected-tool W) 'magic-wand))


(defmethod SELECT-RECT-TOOL-ACTION ((W window) (Button image-button))
  (setf (selected-tool W) 'select-rect))


(defmethod SELECT-ELLIPSE-TOOL-ACTION ((W window) (Button image-button))
  (setf (selected-tool W) 'select-ellipse))


(defmethod SELECT-POLYGON-TOOL-ACTION ((W window) (Button image-button))
  (setf (selected-tool W) 'select-polygon))


(defmethod ENABLE-GRID-ACTION ((W window) (Check-Box check-box))
  (toggle-grid (view-named w "image editor") (value Check-Box)))


(defmethod MIRROR-NONE-ACTION ((W window) (Choice choice-image-button))
  (toggle-mirror-lines (view-named w "image editor") nil nil))


(defmethod MIRROR-HORIZONTALLY-ACTION ((W window) (Choice choice-image-button))
  (toggle-mirror-lines (view-named w "image editor") t nil))


(defmethod MIRROR-VERTICALLY-ACTION ((W window) (Choice choice-image-button))
  (toggle-mirror-lines (view-named w "image editor") nil t))


(defmethod MIRROR-BOTH-ACTION ((W window) (Choice choice-image-button))
  (toggle-mirror-lines (view-named w "image editor") t t))

#|

(defparameter *ie*
  
<image-editor-window margin="20" track-mouse="true" title="Image Editor" width="500" height="450">
 <column align="stretch" valign="stretch">
  <row align="center" valign="middle" height="28" >
    <check-box text="show grid" action="enable-grid-action"/>
    <choice-image-button width="180">                                                                                   
      <choice-button-item text="no mirror" image="mirror-none-button.png" action="mirror-none-action"/>
      <choice-button-item text="mirror horizontally" image="mirror-horizontally-button.png" action="mirror-horizontally-action"/>
      <choice-button-item text="mirror vertically" image="mirror-vertically-button.png" action="mirror-vertically-action"/>
      <choice-button-item text="mirror both" image="mirror-both-button.png" action="mirror-both-action"/>
    </choice-image-button>
  </row>
  <row align="stretch" valign="stretch" vflex="1">
    <image-button-cluster>
     <image-button name="draw button" action="draw-tool-action" image="draw-button.png"/> 
     <image-button name="erase button" action="erase-tool-action" image="erase-button.png"/> 
     <image-button name="eye dropper button" action="eye-dropper-tool-action" image="eye-dropper-button.png"/> 
     <image-button name="paint bucket button" action="paint-bucket-tool-action" image="paint-bucket-button.png"/>
     <image-button name="magic wand button" action="magic-wand-tool-action" image="magic-wand-button.png"/>
     <image-button name="select rectangle button" action="select-rect-tool-action" image="select-rect-button.png"/>
     <image-button name="select ellipse button" action="select-ellipse-tool-action" image="select-ellipse-button.png"/>
     <image-button name="select polygon button" action="select-polygon-tool-action" image="select-polygon-button.png"/>
     <spacer height="10"/>
     <color-well name="color well" action="pick-color-action" color="000000"/>
   </image-button-cluster>
  <spacer width="5"/>
  <image-editor name="image editor" image="lui:resources;images;redlobster.png" flex="1" vflex="1"/>
  </row>
  </column>
</image-editor-window>  )


<image-editor-window margin="20" title="Image Editor">
  <row align="stretch" valign="stretch">
    <column width="30">
     <image-button name="draw button" action="draw-tool-action" image="draw-button.png"/> 
     <image-button name="erase button" action="erase-tool-action" image="erase-button.png"/> 
     <image-button name="eye dropper button" action="eye-dropper-tool-action" image="eye-dropper-button.png"/> 
     <image-button name="paint bucket button" action="paint-bucket-tool-action" image="paint-bucket-button.png"/> 
     <spacer height="10"/>
     <color-well name="color well" action="pick-color-action" color="FF00FF"/>
   </column>
  <image-editor name="image editor" image="" flex="1" vflex="1"/>
  </row>
</image-editor-window>


<image-editor-window margin="20" title="Image Editor">
  <image-editor image="/Users/alex/Desktop/aqua_blue.png"/>
</image-editor-window>


(defparameter w (make-instance 'image-editor-window 
                  :window-show t
                  :file (choose-file-dialog :directory "ccl:resources;textures;")
                  :on-image-saved #'(lambda (Pathname) (print Pathname))))


(load-image-from-file w (choose-file-dialog :directory "ccl:resources;textures;"))

(setf (tolerance (image-editor-view w)) 100)

(erase-all (image-editor-view w))


|#

