;;;-*- Mode: Lisp; Package: XLUI -*-
;*********************************************************************
;*                                                                   *
;*        F O N T         M A N A G E R                              *
;*                                                                   *
;*********************************************************************
   ;* Author    : Ronald Sudomo & Alexander Repenning                *
   ;*             http://www.agentsheets.com                         *
   ;* Copyright : (c) 1996-2005, AgentSheets Inc.                    *
   ;* Filename  : Font-Manager.lisp                                  *
   ;* Updated   : 02/04/05                                           *
   ;* Version   :                                                    *
   ;*   1.0     : 10/20/04 Alex                                      *
   ;*   1.1     : 11/08/04 Ronald                                    *
   ;*   2.0     : 06/14/09 Clozure CL, Cocoa                         *
   ;* SW/HW     : PowerPC G4, OS X 10.5.6, CCL 1.3                   *
   ;* Abstract  : Font and Glyph class create and load scalable      *
   ;*   fonts for OpenGL.                                            *
   ;*                                                                *
   ;******************************************************************

(in-package :xlui)


(defun FONT-DIR ()
  "lui:resources;fonts;")

;;;---------------
;;; Glyphs Class |
;;;---------------

(defclass GLYPH (xml-serializer)
  ((name :accessor name :initform #\Space :initarg :name :type character)
   (x0 :accessor x0 :initform -1s0 :initarg :x0 :type short-float)
   (y0 :accessor y0 :initform -1s0 :initarg :y0 :type short-float)
   (x1 :accessor x1 :initform -1s0 :initarg :x1 :type short-float)
   (y1 :accessor y1 :initform -1s0 :initarg :y1 :type short-float))
  (:documentation "A glyph represents a character symbol in a font"))


(defmethod WIDTH ((Self glyph))
  "Returns the glyph width."
  (- (x1 Self) (x0 Self)))


(defmethod HEIGHT ((Self glyph))
  "Returns the glyph height."
  (- (y1 Self) (y0 Self)))

;;;-------------
;;; Font Class |
;;;-------------

(defparameter *Padding-H* 6 "Horizontal padding between character glyphs")

(defparameter *Padding-V* 2 "Vertical padding between lines of character glyphs")


(defclass FONT (xml-serializer)
  ((font-name :accessor font-name :initform "helvetica" :initarg :font-name)
   (font-size :accessor font-size :initform 16)
   (font-style :accessor font-style :initform :plain)
   (start :accessor start :initform 32 :type integer :initarg :start)
   (end :accessor end :initform 127 :type integer :initarg :end)
   (texture :accessor texture :initform nil)
   (texture-size :accessor texture-size :initform 512 :type integer)
   (glyphs :accessor glyphs :initform nil :documentation "Array of glyphs"))
  (:documentation "An antialiased font based on a font specification"))


(defmethod PRINT-SLOTS ((Self font))
  ;; Return a list of slot names to be printed by xml-serializer
  '(font-name font-size font-style texture-size start end glyphs))


(defmethod FINISHED-READING-ATTRIBUTES ((Self font) Stream)
  ;; Called by xml-serializer when finished reading 'font' XML attributes.
  ;; Perfect time to initialize the glyphs array.
  (declare (ignore Stream))
  (setf (glyphs Self) (make-array (- (end Self) (start Self)) :initial-element nil)))


(defmethod ADD-SUBOBJECT ((Self font) (Object glyph))
  ;; Since 'glyph' sub-objects should added to the glyphs array slot, we need to 
  ;; override this xml-serializer method.
  (setf (svref (glyphs Self) (char-index Self (name Object))) Object))


(defmethod CHAR-INDEX ((Self font) Char)
  "Returns the index of character glyph.
   in: Char {character}."
  (- (char-code Char) (start Self)))


(defmethod LINE-HEIGHT ((Self font))   0.15)   ;; JIKES hard coded

#|
  "Returns the height of a line of characters."
  (let ((Font-Spec (list (font-name Self) (font-size Self) (font-style Self))))
    (multiple-value-bind (Ascent Descent Max-Width Leading)
                         (font-info Font-Spec)
      (declare (ignore Max-Width Leading))
      (float (/ (+ Ascent Descent) (texture-size Self)) 0s0))))

|#

(defmethod FITS-INTO-BOX ((Self font) Width Height)
  "Returns t if all the characters in the font can be rendered using the current font-size
   into a box with the given width and height.
   in: Width {integer}, Height {integer}."
  (let ((Font-Spec (list (font-name Self) (font-size Self) (font-style Self))))
    (multiple-value-bind (Ascent Descent Max-Width Leading)
                         (font-info Font-Spec)
      (declare (ignore Max-Width))
      ;; (format t "~%font info: ~A ~A ~A ~A" ascent descent max-width leading)
      (let ((X 0) (Y (+ Ascent Descent)))
        (dotimes (I (- (end Self) (start Self)) y)
          (let ((W (string-width (string (code-char (+ (start Self) i))) Font-Spec)))
            (cond
             ;; same row
             ((<= (+ x w) Width)
              (incf x (+ w *Padding-H*)))
             ;; next row
             (t 
              (incf y (+ Ascent Descent Leading *Padding-V*))
              ;; check that the current font-baseline + font-descent does not exceed the box height
              (when (> (+ y Descent) Height) (return nil))
              (setq x (+ w *Padding-H*))))))))))


(defmethod OPTIMIZE-CHAR-SIZE ((Self font))
  "Increments or decrements the font size until we find the largest font size that still
   fit inside the texture size."
  (setf (font-size Self) 1)
  (loop
    (unless (fits-into-box Self (texture-size Self) (texture-size Self))
      (decf (font-size Self))
      (return))
    (incf (font-size Self))))


(defmethod MAKE-GLYPHS ((Self font) &key (Optimized t))
  "Creates glyphs for all the characters in the font. 
   If optimized, the character size will be adjusted to best fit the texture size.
   in: Optimized {boolean}."
  ;; ensure glyphs array is initialized
  (unless (glyphs Self) 
    (setf (glyphs Self) (make-array (- (end Self) (start Self)) :initial-element nil)))
  ;; optimized character size if requested
  (when Optimized (optimize-char-size Self))
  (let ((Font-Spec (list (font-name Self) (font-size Self) (font-style Self))))
    (multiple-value-bind (Ascent Descent Max-Width Leading)
                         (font-info Font-Spec)
      (declare (ignore Max-Width))
      (let ((X 0) (Y (+ Ascent Descent)))
        (dotimes (I (- (end Self) (start Self)) y)
          (let* ((Ch (code-char (+ (start Self) i)))
                 (String (string Ch))
                 (W (string-width String Font-Spec)))
            ;; new row?
            (when (> (+ x w) (texture-size Self))
              (incf y (+ Ascent Descent Leading *Padding-V*))
              (setq x 0))
            ;; add glyph to array
            (setf (svref (glyphs Self) (char-index Self Ch))
                  (make-instance 'glyph :name Ch 
                                 :x0 (float (/ x (texture-size Self)) 0s0)
                                 :y0 (float (- 1 (/ y (texture-size Self))) 0s0)
                                 :x1 (float (/ (+ x w) (texture-size Self)) 0s0)
                                 :y1 (float (- 1 (/ (- y Ascent Descent) (texture-size Self))) 0s0)))
            (incf x (+ w *Padding-H*))))))))

#| if needed reimplement with Cocoa:
(defmethod RENDER-GLYPHS ((Self font) Window &key (Clear-Background t) Debug-Lines)
  "Renders all the font glyphs onto the window.
   in: Window {window}, Clear-Background {boolean}, Debug-Lines {boolean}."
  ;; ensure glyphs are created
  (unless (glyphs Self) (make-glyphs Self))
  (with-focused-view Window
    (let ((Font-Spec (list (font-name Self) (font-size Self) (font-style Self))))
      (multiple-value-bind (Ascent Descent Max-Width Leading)
                           (font-info Font-Spec)
        (declare (ignore Ascent Max-Width Leading))
        (set-view-font Window Font-Spec)
        (when Clear-Background
          (rlet ((&rect :rect :topleft #@(0 0) :botright (view-size Window)))
            (#_EraseRect &rect)))
        ;; draw all character glyphs
        (dotimes (I (length (glyphs Self)))
          (let ((Glyph (svref (glyphs Self) i)))
            ;; Note that the glyph coordinates are for its bounding box in 
            ;; the texture coordinate. To draw it on screen we need to translate them
            ;; to the screen coordinate and compute the glyph's baseline.
            (#_MoveTo 
             (round (* (x0 Glyph) (texture-size Self))) 
             (round (- (* (- 1 (y0 Glyph)) (texture-size Self)) Descent))) ;; baseline
            (with-pstrs ((&string (string (name Glyph))))
              (#_DrawString &string))
            ;; Draw lines for debugging
            (when Debug-Lines
              (let ((Ytop (round (* (- 1 (y1 Glyph)) (texture-size Self))))
                    (Ybase (round (- (* (- 1 (y0 Glyph)) (texture-size Self)) Descent)))
                    (Ybottom (round (* (- 1 (y0 Glyph)) (texture-size Self)))))
                (with-fore-color *Red-Color*
                  ;; topline
                  (#_MoveTo 0 YTop)
                  (#_LineTo 512 YTop)
                  ;;; baseline
                  (#_MoveTo 0 YBase)
                  (#_LineTo 512 YBase)
                  ;;; bottomline
                  (#_MoveTo 0 YBottom)
                  (#_LineTo 512 YBottom))))
            ))))))
|#

(defmethod GET-GLYPH ((Self font) Char)
  "Returns the character glyph.
   in: Char {character}."
  (svref (glyphs Self) (char-index Self Char)))


(defmethod SAVE-GLYPHS-TEXTURE-IMAGE ((Self font) To-File)
  "Saves the glyphs texture image to the given file.
   in: To-File {pathname}."
  (let ((Buffer-Window (make-instance 'font-rendering-window :font Self :window-show nil)))
    (render-and-save-to-file Buffer-Window To-File)
    (window-close Buffer-Window)))


(defmethod SAVE-FONT-SPEC ((Self font) To-File)
  "Saves the font specification to the given file.
   in: To-File {pathname}."
  (with-open-file (Output To-File :direction :output :if-exists :supersede)
    (princ Self Output)))



#| Reimplement with Cocoa if needed

;;;------------------------------
;;; Font-Rendering-Window Class |
;;;------------------------------

(defclass FONT-RENDERING-WINDOW (window)
  ((font :accessor font :initform nil :initarg :font))
  (:documentation "Window for rendering font glyphs."))


(defmethod INITIALIZE-INSTANCE :after ((Self font-rendering-window) &rest Initargs)
  (declare (ignore Initargs))
  (set-view-size Self (texture-size (font Self)) (texture-size (font Self)))
  (set-window-title Self (font-name (font Self))))


(defmethod VIEW-DRAW-CONTENTS ((Self font-rendering-window))
  (render-glyphs (font Self) Self :debug-lines nil))


(defmethod RENDER-AND-SAVE-TO-FILE ((Self font-rendering-window) Pathname)
  ;;; 2008-01-02TA unwind-protect extents corrected, now cleanup form is called in case of return from error
  "Renders the font glyphs onto a transparent window and saves it to the given file."
  (rlet ((&window pointer)
         (&rect :rect :topleft #@(100 100)
                :bottom (+ 100 (texture-size (font Self))) :right (+ 100 (texture-size (font Self)))))
    (let ((Wptr (wptr Self)))
      (unwind-protect  ;; make sure there is a way to get rid of this window in case of an error
        (progn
          (#_CreateNewWindow #$kOverlayWindowClass 0 &rect &window)
          (let ((Overlay-Wptr (%get-ptr &window)))
            (#_ShowWindow Overlay-Wptr)
            (#_SetPort (#_GetWindowPort Overlay-Wptr))
            ;; swap indentities to set font info
            (setf (wptr Self) Overlay-Wptr)
            ;; render all chars
            (render-glyphs (font Self) Self :clear-background t)
            ;; write to file
            (#_QDFlushPortBuffer (#_GetWindowPort Overlay-Wptr) (%null-ptr))
            (save-window-content-as-image Self Pathname
                                          0 0 (texture-size (font Self)) (texture-size (font Self))
                                          :pixel-format #$k32RGBAPixelFormat )))
        ;; done with overlay window
        (#_ReleaseWindow (%get-ptr &window))
        ;; reset indentity
        (setf (wptr Self) Wptr)))))

|#

;;;-----------------------------
;;; Font save & load functions |
;;;-----------------------------

(defun GENERATE-FONT-FILES (Font-Names &optional (Font-Directory "fonts:"))
  "Generates the font specification files and glyphs texture images for all the specified fonts.
   in: Font-Names {list}, Font-Directory {pathname}."
  (dolist (Font-Name Font-Names)
    (let ((Name (if (listp Font-Name) (first Font-Name) Font-Name)))
      (let ((Spec-File (merge-pathnames (format nil "~A.font" (string-downcase Name)) Font-Directory))
            (Glyphs-File (merge-pathnames (format nil "~A.png" (string-downcase Name)) Font-Directory))
            (Font (if (listp Font-Name)
                    (make-instance 'font 
                      :font-name Name
                      :start (second Font-Name)
                      :end (third Font-Name))
                    (make-instance 'font :font-name Name))))
        (save-glyphs-texture-image Font Glyphs-File)
        (save-font-spec Font Spec-File)))))


(defun LOAD-FONT (Font-Name &optional (Font-Directory "fonts:"))
  "Loads the specified font from the font specification file in the given directory.
   in: Font-Name {string}, Font-Directory {pathname}."
  (let* ((*Package* (find-package :xlui))
         (Spec-File (merge-pathnames (format nil "~A.font" (string-downcase Font-Name)) Font-Directory)))
    (unless (probe-file Spec-File)
      (error "Missing font specification file: ~A" (truename Spec-File)))
    (load-object Spec-File)))


(defun CREATE-FONT-TEXTURE (Font-Name)
  "Creates an OpenGL texture for the font glyphs. Must be called with active AGL context.
   in: Font-Name {string}, Font-Directory {pathname}."
  (let ((Texture-File (format nil "~A~A.png" (font-dir) (string-downcase Font-Name))))
    (unless (probe-file Texture-File)
      (error "Missing font texture image file: ~A" (truename Texture-File)))
    (let ((Path (truename Texture-File)))
      (create-texture-from-file (namestring Path)))))


#| Examples:


;;--- font XML spec ---

<font/>

<font><glyph/></font>



;;--- font rendering ---

(defparameter *Font* (make-instance 'font :font-name "Comic Sans MS"))
(defparameter *Font* (make-instance 'font :font-name "Helvetica"))
(defparameter *Font* (make-instance 'font :font-name "Geneva"))
(defparameter *Font* (make-instance 'font :font-name "Monaco"))
(defparameter *Font* (make-instance 'font :font-name "Arial"))
(defparameter *Font* (make-instance 'font :font-name "Lucida Grande"))
(defparameter *Font* (make-instance 'font :font-name "Symbol" :end 255))

(make-glyphs *Font* :optimized t)

(defparameter *Window* (make-instance 'font-rendering-window :font *Font*))

(render-and-save-to-file *Window* (choose-new-file-dialog))



;;--- save and load font ---

(generate-font-files '("Comic Sans MS"
                       "Helvetica"
                       "Geneva"
                       "Monaco"
                       "Arial"
                       "Lucida Grande"))


(generate-font-files '(("Symbol" 0 255)))

(generate-font-files '(("charcoal cy" 0 127)))

(defparameter *Font* (load-font "Comic Sans MS"))


|#



;;;---------------------
;;; Font-Manager Class |
;;;---------------------

(defclass FONT-MANAGER ()
  ((font-directory
    :documentation "Directory to load font files from"
    :initform nil
    :initarg :font-directory
    :accessor font-directory)
   (font-table
    :documentation "Hash table of currently loaded fonts"
    :initform (make-hash-table :test #'equal)
    :accessor font-table))
  (:documentation "Manages font loading and sharing."))


(defmethod GET-FONT ((Self font-manager) Font-Name)
  "Returns the specified font. If not yet loaded it will be loaded automatically.
   in: Font-Name {string}."
  ;; should not be case sensitive
  (setq Font-Name (string-downcase Font-Name))
  (let ((Font (gethash Font-Name (font-table Self))))
    (unless Font
      ;; need to have an active glcontext, assume it is shared
      ;; perhaps better: check if there is current glcontext
      (with-glcontext (shared-opengl-view)
        (setq Font (load-font Font-Name (font-directory Self)))
        (setf (texture Font) (create-font-texture Font-Name))
        (setf (gethash Font-Name (font-table Self)) Font)))
    Font))


(defmethod CLEAR-ALL-FONTS ((Self font-manager))
  "Unloads all currently loaded fonts."
  (clrhash (font-table Self)))



#| Examples:

(defparameter *fm* (make-instance 'font-manager :font-directory (font-dir)))

(get-font *fm* "Lucida Grande")
(get-font *fm* "Comic Sans MS")

(inspect *fm*)

(clear-all-fonts *fm*)

|#


