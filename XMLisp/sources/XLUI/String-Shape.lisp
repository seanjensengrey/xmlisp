;;;-*- Mode: Lisp; Package: XLUI -*-
;*********************************************************************
;*                                                                   *
;*        S T R I N G    S H A P E                                   *
;*                                                                   *
;*********************************************************************
   ;* Author    : Ronald Sudomo ronald@agentsheets.com               *
   ;*             http://www.agentsheets.com                         *
   ;* Copyright : (c) 1996-2009, AgentSheets Inc.                    *
   ;* Filename  : String-Shape.lisp                                  *
   ;* Updated   : 08/13/07                                           *
   ;* Version   :                                                    *
   ;*   1.0     : 10/20/04                                           *
   ;*   1.1     : 02/23/05 color, display-string-underline           *
   ;*   1.1.1   : 03/05/05 0 garbage in display                      *
   ;*   1.2     : 11/09/05 size                                      *
   ;*   1.3     : 08/13/07 Garbage Collectable arrays (MCL)          *
   ;*   1.4     : 07/16/09 AR: CCL                                   *
   ;*   1.4.1   : 08/08/09 64 bit use _memmove                       *
   ;* SW/HW     : PowerPC G4, OS X 10.5.6, CCL 1.3                   *
   ;* Abstract  : Definition of String-Shape & Editable-String-Shape *
   ;*                                                                *
   ;******************************************************************

(in-package :xlui)

(defconstant *Sizeof-Float* (sizeof 'float))


(defvar *Font-Manager* 
  (make-instance 'font-manager :font-directory (font-dir)))


;;;---------------------------
;;; Memory Management Macros |
;;;---------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defmacro MEMORY-ALLOC (Size)
    #+(and ccl (not windows-target)) `(#_NewPtr ,Size)
    #+(and ccl windows-target) `(#_HeapAlloc (#_GetProcessHeap) 0 ,Size)
    #-ccl (error "MEMORY-ALLOC not implemented"))
  
  (defmacro MEMORY-DEALLOC (Buffer)
    #+(and ccl (not windows-target)) `(#_DisposePtr ,Buffer)
    #+(and ccl windows-target) `(#_HeapFree (#_GetProcessHeap) 0 ,Buffer)
    #-ccl (error "MEMORY-DEALLOC not implemented"))

  (defmacro MEMORY-COPY (Src Dest Size)
    #+ccl `(#_memmove ,Dest ,Src ,Size)
    #-ccl (error "MEMORY-COPY not implemented"))

  (defmacro MEMORY-PUT-FLOAT (Buffer Value Index)
    #+ccl `(setf (ccl::%get-single-float ,Buffer, Index) ,Value)
    #-ccl (error "MEMORY-PUT-FLOAT not implemented"))
  
  (defmacro MEMORY-GET-FLOAT (Buffer Index)
    #+ccl `(ccl::%get-single-float ,Buffer ,Index)
    #-ccl (error "MEMORY-GET-FLOAT not implemented"))
  
  )


;;;-------------------
;;; Helper Functions |
;;;-------------------

(defun SPLIT-STRING-INTO-LINES (String)
  "Splits a string containing newline characters into multiple lines."
  (let ((Start 0) (Lines nil))
    (dotimes (I (length String) Lines)
      ;; end of line
      (when (or (char= (char String i) #\Return) 
                (char= (char String i) #\Linefeed))
        (setq Lines (nconc Lines (list (subseq String Start i))))
        (setq Start (+ i 1)))
      ;; the end
      (when (= i (1- (length String)))
        (setq Lines (nconc Lines (list (subseq String Start))))))))


(defun NEWLINES-COUNT (String)
  "Returns the number of newline characters in the given string."
  (let ((Count 0))
    (dotimes (I (length String) Count)
      (when (or (char= (char String i) #\Return)
                (char= (char String i) #\Linefeed))
        (incf Count)))))


(defun TEXT-BOX-DIMENSION (String Font)
  "Returns the text box dimension for the given String and Font.
   in: String {string}, Font {font}."
  (let ((Width 0s0) (Height 0s0) (X 0s0))
    (dolist (Line (split-string-into-lines String))
      (setq x 0s0)
      (incf Height (line-height Font))
      (dotimes (I (length Line))
        (incf x (width (get-glyph Font (char Line i))))
        (when (> x Width) (setq Width x))))
    (values Width Height)))



;;;---------------------
;;; String-Shape Class |
;;;---------------------

(defclass STRING-SHAPE ()
  ((str
    :documentation "The string to be displayed."
    :initform ""
    :initarg :str
    :accessor str)
   (font
    :documentation "The font to be used."
    :initform (get-font *Font-Manager* "Lucida Grande")
    :initarg :font
    :accessor font)
   (width
    :documentation "The width of the string's bounding box."
    :initform 0
    :accessor width)
   (height
    :documentation "The height of the string's bounding box."
    :initform 0
    :accessor height)
   (vertex-arrays
    :documentation "Arrays to store pre-computed vertices."
    :initform nil
    :accessor vertex-arrays)
   (va-elements-count ;; stored to increase performance
    :documentation "Vertex arrays elements count."
    :initform 0
    :accessor va-elements-count)
   (va-stride         ;; stored to increase performance
    :documentation "Vertex arrays stride parameter."
    :allocation :class
    :initform (* 5 *Sizeof-Float*)
    :accessor va-stride)
   (color-vector 
    :documentation "24 bit RGB color vector"
    :accessor color-vector
    :initform nil)
   (color
    :documentation "24 bit RGB color specification in hex string format, e.g., \"00FF00\" for green"
    :reader color
    :initarg :color
    :initform nil)
   (size 
    :accessor size
    :documentation "size used to scale string"
    :initform 1.0
    :initarg :size))
  (:documentation "A class to efficiently display text in OpenGL window."))


(defmethod ALLOCATE-VERTEX-ARRAYS ((Self string-shape) String-Length)
  ;; allocate just enough to fit the string
  ;; 4 vertices per character, 5 entries per vertex (x-texture, y-texture, x, y, z)
  (memory-alloc (* 4 String-Length (va-stride Self))))


(defmethod PUT-INTO-VERTEX-ARRAYS-AT-INDEX ((Self string-shape) Index String Font X Y &optional (Z 0s0))
  (dolist (Line (split-string-into-lines String))
    (dotimes (I (length Line))
      (let ((Char (char Line i)))
        ;; put texcoords and vertices into interleaved vertex-arrays
        (let ((Glyph (get-glyph Font Char)))
          ;; x0,y0
          (memory-put-float (vertex-arrays Self) (x0 Glyph) Index)
          (memory-put-float (vertex-arrays Self) (y0 Glyph) (incf Index *Sizeof-Float*))
          (memory-put-float (vertex-arrays Self) x (incf Index *Sizeof-Float*))
          (memory-put-float (vertex-arrays Self) y (incf Index *Sizeof-Float*))
          (memory-put-float (vertex-arrays Self) z (incf Index *Sizeof-Float*))
          ;; x0,y1
          (memory-put-float (vertex-arrays Self) (x0 Glyph) (incf Index *Sizeof-Float*))
          (memory-put-float (vertex-arrays Self) (y1 Glyph) (incf Index *Sizeof-Float*))
          (memory-put-float (vertex-arrays Self) x (incf Index *Sizeof-Float*))
          (memory-put-float (vertex-arrays Self) (+ y (height Glyph)) (incf Index *Sizeof-Float*))
          (memory-put-float (vertex-arrays Self) z (incf Index *Sizeof-Float*))
          ;; x1,y1
          (memory-put-float (vertex-arrays Self) (x1 Glyph) (incf Index *Sizeof-Float*))
          (memory-put-float (vertex-arrays Self) (y1 Glyph) (incf Index *Sizeof-Float*))
          (memory-put-float (vertex-arrays Self) (+ x (width Glyph)) (incf Index *Sizeof-Float*))
          (memory-put-float (vertex-arrays Self) (+ y (height Glyph)) (incf Index *Sizeof-Float*))
          (memory-put-float (vertex-arrays Self) z (incf Index *Sizeof-Float*))
          ;; x1,y0
          (memory-put-float (vertex-arrays Self) (x1 Glyph) (incf Index *Sizeof-Float*))
          (memory-put-float (vertex-arrays Self) (y0 Glyph) (incf Index *Sizeof-Float*))
          (memory-put-float (vertex-arrays Self) (+ x (width Glyph)) (incf Index *Sizeof-Float*))
          (memory-put-float (vertex-arrays Self) y (incf Index *Sizeof-Float*))
          (memory-put-float (vertex-arrays Self) z (incf Index *Sizeof-Float*))
          ;; increment x and index
          (incf x (width Glyph))
          (incf Index *Sizeof-Float*))))
    ;; newline
    (setq x 0s0)
    (decf y (line-height Font))))


(defmethod UPDATE-TEXT-BOX-DIMENSION ((Self string-shape))
  (multiple-value-bind (Width Height) (text-box-dimension (str Self) (font Self))
    (setf (width Self) (* (size Self) Width))
    (setf (height Self) (* (size Self) Height))))


(defmethod UPDATE-VA-ELEMENTS-COUNT ((Self string-shape))
  "Updates the number of elements contained in the vertex arrays."
  (let ((Char-Count 0))
    (dolist (String (split-string-into-lines (str Self)))
      (incf Char-Count (length String)))
    (setf (va-elements-count Self) (* 4 Char-Count))))


(defmethod UPDATE-VERTEX-ARRAYS ((Self string-shape))
  "Updates the vertex-arrays which store pre-computed vertices. 
   Pre-computed vertices are stored so that the string can be displayed faster
   and more efficiently."
  ;; release old vertex-arrays if necessary
  (when (vertex-arrays Self) (memory-dealloc (vertex-arrays Self)))
  ;; create new vertex-arrays
  (setf (vertex-arrays Self) (allocate-vertex-arrays Self (length (str Self))))
  ;; store pre-computed vertices
  (let ((X 0s0) (Y 0s0) (Z 0s0) (Index 0))
    (put-into-vertex-arrays-at-index Self Index (str Self) (font Self) x y z)
    (update-va-elements-count Self))
  (update-text-box-dimension Self))



(defun HEX-CHAR-NUMBER (Char)
  ;; quick and dirty
  (ecase Char
   (#\0 0) (#\1 1) (#\2 2) (#\3 3) (#\4 4) (#\5 5) (#\6 6) (#\7 7) (#\8 8) (#\9 9)
   ((#\a #\A) 10) ((#\b #\B) 11) ((#\c #\C) 12) ((#\d #\D) 13) ((#\e #\E) 14) ((#\f #\F) 15)))


(defmethod (SETF COLOR) (String (Self string-shape))
  ;; the color writer
  (setf (slot-value Self 'color) String)
  ;; compute and store vector
  (let ((Values nil))
    (dotimes (I 3)
      (push (+ (* 16 (hex-char-number (char String (* i 2))))
               (hex-char-number (char String (1+ (* i 2)))))
            Values))
    (if (color-vector Self)
      (apply #'set-byte-vector (color-vector Self) (reverse Values))
      (setf (color-vector Self) (apply #'make-byte-vector (reverse Values))))))
  


(defmethod INITIALIZE-INSTANCE :after ((Self string-shape) &rest Initargs)
  (declare (ignore Initargs))
  (update-vertex-arrays Self)
  ;; set color: make sure to compute vector
  (when (color Self)
    (setf (color Self) (color Self)))
  ;; make GC able: when GCed free the vector array
  #+:ccl (ccl::terminate-when-unreachable Self))


#+:ccl
(defmethod TERMINATE ((self string-shape))
  ;; (format t "~%disposed vector array: ~A for string ~A" Self (str Self))
  (when (vertex-arrays Self) (memory-dealloc (vertex-arrays Self))))
  


(defmethod SET-STRING-SHAPE ((Self string-shape) &key Str Font)
  "Changes the string and/or font of this string-shape.
   in: Str {string}, Font {font}."
  (when Str (setf (str Self) Str))
  (when Font (setf (font Self) Font))
  (update-vertex-arrays Self))

;__________________________
; Display methods          |
;__________________________

(defmethod DISPLAY-VERTEX-ARRAYS ((Self string-shape))
  (glEnable gl_texture_2d)
  (cond
   ;; Color!
   ((color-vector Self)
    (glcolor3ubv (color-vector Self))
    (gltexenvi gl_texture_env gl_texture_env_mode gl_blend))
   ;; Black
   (t 
    (gltexenvi gl_texture_env gl_texture_env_mode gl_modulate)))
  (glbindtexture gl_texture_2d (texture (font Self)))
  (glInterleavedArrays GL_T2F_V3F (va-stride Self) (vertex-arrays Self))
  (glDrawArrays gl_quads 0 (va-elements-count Self)) 
  (gltexenvi gl_texture_env gl_texture_env_mode gl_modulate)
  (glDisable gl_texture_2d)
  ;; if color was use better reset to white? 
  (when (color-vector Self) (glColor3f 1.0 1.0 1.0)))


(defmethod DISPLAY-TEXT-BOX ((Self string-shape))
  "Displays the bounding text box."
  (let ((Z-Offset 0.001S0)
        (Line-Height (line-height (font Self))))
    (glColor3f 0.5s0 0.5s0 0.5s0)
    (glBegin GL_LINE_LOOP)
    (glVertex3f 0 Line-Height z-Offset)
    (glVertex3f (width Self) Line-Height  z-Offset)
    (glVertex3f (width Self) (+ Line-Height (* -1 (height Self))) z-Offset)
    (glVertex3f 0 (+ Line-Height (* -1 (height Self))) z-Offset)
    (glEnd)))


(defmethod BASIC-DISPLAY-STRING-SHAPE ((Self string-shape))
  "Displays the string without any translation.
   Subclass should probably specialize this method instead of display-string-shape."
  (display-vertex-arrays Self)
  ;; display bounding text box for debugging
  ;;(display-text-box Self)
  )


(defmethod DISPLAY-STRING-SHAPE ((Self string-shape) X Y &optional (Z 0s0))
  "Displays the string at the specified location.
   in: x {short-float}, y {short-float}, z {short-float}."
  (glPushMatrix)
  (glTranslatef x y z)
  (basic-display-string-shape Self)
  (glPopMatrix))


(defmethod DISPLAY-STRING-UNDERLINE ((Self string-shape))
  "Underline string. Same color as string"
  (let ((Z-Offset 0.001S0))
    (when (color-vector Self) (glcolor3ubv (color-vector Self)))
    (glLineWidth 1s0)
    (glBegin GL_LINEs)
    (glVertex3f 0s0 0s0 z-Offset)
    (glVertex3f (width Self) 0s0  z-Offset)
    (glEnd)))


(defmethod DRAW ((Self string-shape))
  (glPushmatrix)
  (glScalef (size Self) (size Self) 1.0)
  (display-vertex-arrays Self)
  (glPopmatrix))


(defmethod DISPOSE-STRING-SHAPE ((Self string-shape))
  "Disposes memory used by this string-shape object.
   Should be called when this string-shape object is no longer used."
  ;; delete vertex-arrays
  (when (vertex-arrays Self) (memory-dealloc (vertex-arrays Self))))


(defun ENABLE-STRING-SHAPE ()
  "Enables OpenGL settings needed by string-shape."
  ;; enable alpha because the font texture images have alpha channels
  (glenable gl_blend)
  (glBlendFunc gl_src_alpha gl_one_minus_src_alpha))



;;;------------------------------
;;; Editable-String-Shape Class |
;;;------------------------------

(defparameter *Cursor-Blinking-Period* 0.333s0) ;; seconds


(defclass EDITABLE-STRING-SHAPE (string-shape)
  ((cursor-position
    :documentation "The cursor's current position."
    :initform 0
    :accessor cursor-position)
   (cursor-x
    :documentation "The cursor's current x-coordinate."
    :initform 0s0
    :accessor cursor-x)
   (cursor-y
    :documentation "The cursor's current y-coordinate."
    :initform 0s0
    :accessor cursor-y)
   (cursor-visible-p
    :documentation "Flag to indicate whether to show or hide the cursor."
    :initform t
    :accessor cursor-visible-p)
   (cursor-animation-time
    :documentation "Time accumulator for animating the cursor."
    :initform 0
    :accessor cursor-animation-time)
   (va-capacity
    :documentation "The capacity of the vertex-arrays."
    :initform 0
    :accessor va-capacity))
  (:documentation "An editable string-shape class."))


(defmethod ALLOCATE-VERTEX-ARRAYS ((Self editable-string-shape) String-Length)
  ;; allocate double the size of the string
  (unless (> String-Length 0) (setq String-Length 5))
  (setf (va-capacity Self) (* 2 String-Length))
  (memory-alloc (* 4 (va-capacity Self) (va-stride Self))))


(defmethod GROW-VERTEX-ARRAYS ((Self editable-string-shape) &optional (Factor 2))
  (let* ((New-Capacity (* Factor (va-capacity Self)))
         (&New-Array (memory-alloc (* 4 New-Capacity (va-stride Self)))))
    ;; copy and deallocate old array
    (memory-copy (vertex-arrays Self) &New-Array (* (length (str Self)) 4 (va-stride Self)))
    (memory-dealloc (vertex-arrays Self))
    ;; set new array and capacity
    (setf (vertex-arrays Self) &New-Array)
    (setf (va-capacity Self) New-Capacity)))


(defmethod CHECK-POSITION-WITHIN-BOUND ((Self editable-string-shape) Position &key Allow-End)
  "Checks whether the given cursor position is within bound."
  (when (or (< Position 0)
            (> Position (length (str Self)))
            (and (not Allow-End) (= Position (length (str Self)))))
    (error "Character position out of bound: ~A" Position)))


(defmethod VA-INDEX-FOR-POSITION ((Self editable-string-shape) Position)
  "Return an index to the vertex array for the given character position."
  (* (- Position (newlines-count (subseq (str Self) 0 Position))) 4 (va-stride Self)))


(defmethod COORDINATE-FOR-POSITION ((Self editable-string-shape) Position)
  "Returns the xy-coordinate for the given character position.
   in: Position {integer}.
   out: x {short-float}, y {short-float}."
  (let ((X 0s0) (Y 0s0) (Index (va-index-for-position Self Position)))
    (when (> Position 0)
      (setq x (memory-get-float (vertex-arrays Self) (- Index (* 3 *Sizeof-Float*)))  ;; prev glyph x1
            y (memory-get-float (vertex-arrays Self) (- Index (* 2 *Sizeof-Float*)))) ;; prev glyph y0
      ;; need to handle CR/LF specially
      (do* ((Index (1- Position) (1- Index))
            (Prev-Char (char (str Self) Index) (char (str Self) Index)))
           ((not (or (char= Prev-Char #\Return) (char= Prev-Char #\Linefeed)))
            (return))
        (setq x 0s0)
        (decf y (line-height (font Self)))))
    (values x y)))


(defmethod INSERT-CHAR ((Self editable-string-shape) Char Position)
  "Inserts a character a the specified position.
   in: Char {character}, Position {integer}."
  ;; check index within bound, unlike delete-char it's legal to insert after the last character
  (check-position-within-bound Self Position :allow-end t)
  ;; ensure vertex-arrays is big enough
  (when (> (1+ (length (str Self))) (va-capacity Self))
    (grow-vertex-arrays Self))
  ;; update vertices in vertex-arrays
  (multiple-value-bind (x y) (coordinate-for-position Self Position)
    (put-into-vertex-arrays-at-index Self
                                     (va-index-for-position Self Position)
                                     (format nil "~C~A" Char (subseq (str Self) Position))
                                     (font Self) x y))
  ;; update the string
  (setf (str Self) 
        (format nil "~A~C~A" (subseq (str Self) 0 Position) Char (subseq (str Self) Position)))
  (update-text-box-dimension Self)
  (update-va-elements-count Self))


(defmethod DELETE-CHAR ((Self editable-string-shape) Position)
  "Deletes the character at the specified position.
   in: Position {integer}."
  (check-position-within-bound Self Position)
 ;; update vertices in vertex-arrays
  (multiple-value-bind (x y) (coordinate-for-position Self Position)
    (put-into-vertex-arrays-at-index Self 
                                     (va-index-for-position Self Position)
                                     (subseq (str Self) (1+ Position))
                                     (font Self) x y))
  ;; update the string
  (setf (str Self)
        (format nil "~A~A" (subseq (str Self) 0 Position) (subseq (str Self) (1+ Position))))
  (update-text-box-dimension Self)
  (update-va-elements-count Self))


(defmethod POSITION-TO-ROW-COL ((Self editable-string-shape) Position)
  (check-position-within-bound Self Position :allow-end t)
  (let* ((Lines (split-string-into-lines (subseq (str Self) 0 Position)))
         (Row (max (1- (length Lines)) 0))
         (Col (length (first (last Lines)))))
    (values Row Col)))


(defmethod ROW-COL-TO-POSITION ((Self editable-string-shape) Row Col)
  (let ((Lines (split-string-into-lines (str Self)))
        (Position 0))
    (when (null Lines) (return-from row-col-to-position 0))
    (cond
     ((< Row 0) (setq Row 0))
     ((>= Row (length Lines)) (setq Row (1- (length Lines)))))
    (dotimes (I Row)
      (incf Position (length (elt Lines i))))
    (+ Position (min Col (length (elt Lines Row))) Row)))


(defmethod UPDATE-CURSOR-COORDINATE ((Self editable-string-shape))
  (multiple-value-bind (x y) (coordinate-for-position Self (cursor-position Self))
    (setf (cursor-x Self) x)
    (setf (cursor-y Self) y)))


(defmethod MOVE-CURSOR-TO-POSITION ((Self editable-string-shape) Position)
  ;; ensure position is between 0 and string-length
  (cond 
   ((< Position 0) (setq Position 0))
   ((> Position (length (str Self))) (setq Position (length (str Self)))))
  (setf (cursor-position Self) Position)
  (update-cursor-coordinate Self))


(defmethod MOVE-CURSOR-TO-ROW-COL ((Self editable-string-shape) Row Col)
  (move-cursor-to-position Self (row-col-to-position Self Row Col)))


(defmethod MOVE-CURSOR-TO-START ((Self editable-string-shape))
  (move-cursor-to-position Self 0))


(defmethod MOVE-CURSOR-TO-END ((Self editable-string-shape))
  (move-cursor-to-position Self (length (str Self))))


(defmethod MOVE-CURSOR-LEFT ((Self editable-string-shape) &optional (Unit 1))
  (move-cursor-to-position Self (- (cursor-position Self) Unit)))


(defmethod MOVE-CURSOR-RIGHT ((Self editable-string-shape) &optional (Unit 1))
  (move-cursor-to-position Self (+ (cursor-position Self) Unit)))


(defmethod MOVE-CURSOR-UP ((Self editable-string-shape) &optional (Unit 1))
  (multiple-value-bind (Row Col) (position-to-row-col Self (cursor-position Self))
    (move-cursor-to-row-col Self (- Row Unit) Col)))


(defmethod MOVE-CURSOR-DOWN ((Self editable-string-shape) &optional (Unit 1))
  (multiple-value-bind (Row Col) (position-to-row-col Self (cursor-position Self))
    (move-cursor-to-row-col Self (+ Row Unit) Col)))


(defmethod ANIMATE-CURSOR ((Self editable-string-shape) Delta-Time)
  "Makes the cursor blinks periodically."
  (incf (cursor-animation-time Self) Delta-Time)
  (when (> (cursor-animation-time Self) *Cursor-Blinking-Period*)
    (setf (cursor-visible-p Self) (not (cursor-visible-p Self)))
    (setf (cursor-animation-time Self) 0)))


(defmethod DISPLAY-CURSOR ((Self editable-string-shape))
  (when (cursor-visible-p Self)
    ;; specify z-offset to display the cursor slightly above the text to avoid overlapping
    ;; (overlapping may cause undefined behavior)
    (let ((Z-Offset 0.002S0))
      (glColor3f 1s0 0s0 0s0)
      (glBegin GL_LINE_STRIP)
      (glVertex3f (cursor-x Self) (cursor-y Self) z-Offset)
      (glVertex3f (cursor-x Self) (+ (cursor-y Self) (line-height (font Self))) z-Offset)
      (glEnd))))


(defmethod BASIC-DISPLAY-STRING-SHAPE :after ((Self editable-string-shape))
  ;; specialized to display cursor
  ;;(display-cursor Self)
  )



#| Examples:

;;;--- Test-Window Class ---

(defclass TEXT-TEST-WINDOW (application-window)
  ())


(defclass OPENGL-TEXT (opengl-dialog)
  ((ss-content :initform nil :accessor ss-content)))


(defmethod PREPARE-OPENGL ((Self opengl-text))
  (call-next-method)
  ;; GL setup
  (glClearColor 1.0 1.0 1.0 0.0)
  (glPixelStorei GL_UNPACK_ALIGNMENT 1)
  ;; enablers
  (glEnable GL_DEPTH_TEST)
  (enable-string-shape)
  (setf (ss-content Self) (make-instance 'editable-string-shape :str ""))
  (aim-camera (camera Self) :eye-z 1.0))


(defmethod DRAW ((Self opengl-text))
  (let ((X -1.2s0) (Y 0.40s0))
    (display-string-shape (ss-content Self) x y)))


(defmethod ANIMATE ((Self opengl-text) Time)
  (animate-cursor (ss-content Self) Time))


(defmethod LOAD-STRING-FROM-FILE ((Self opengl-text) Pathname)
  (setf (ss-content Self)
        (make-instance 'editable-string-shape
          :str (with-output-to-string (Output)
                 (with-open-file (Input Pathname :direction :input)
                   (loop
                     (multiple-value-bind (Line Eof-P) (read-line Input nil nil)
                       (format Output "~A~%" Line)
                       (when Eof-P (return)))))))))


(defmethod VIEW-KEY-EVENT-HANDLER ((Self opengl-text) Key)
  (with-slots (ss-content) Self
    (case Key    
      (#\UpArrow (move-cursor-up ss-content))
      (#\DownArrow (move-cursor-down ss-content))
      (#\ForwardArrow (move-cursor-right ss-content))
      (#\BackArrow (move-cursor-left ss-content))
      (#\Backspace
       (if (zerop (cursor-position ss-content))
         (ed-beep)
         (progn
           (delete-char ss-content (1- (cursor-position ss-content)))
           (move-cursor-left ss-content))))
      (#\Del 
       (if (>= (cursor-position ss-content) (length (str ss-content)))
         (ed-beep)       
         (delete-char ss-content (cursor-position ss-content))))
      (#\Home (move-cursor-to-start ss-content))
      (#\End (move-cursor-to-end ss-content))
      (otherwise
       (when (or (char= Key #\Return) (char= Key #\Linefeed)
                 (and (>= (char-code Key) 32) (< (char-code Key) 128)))
         (insert-char ss-content Key (cursor-position ss-content))
         (move-cursor-right ss-content)))))
  (view-draw-contents Self))


;;;--- show window & draw string ---


(defparameter *Window*

<text-test-window title="OpenGL text" margin="0">
  <opengl-text name="OpenGL Text"/>
</text-test-window>

)



(defparameter *Window* (make-instance 'test-window))

(start-animation *Window*)

(stop-animation *Window*)

(frame-rate (view-named *Window* "OpenGL text"))

(load-string-from-file *Window* (choose-file-dialog))

(time (view-draw-contents *Window*))

(set-string-shape (ss-content (view-named *Window* "OpenGL text")) :str "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG.")


(set-string-shape (ss-content (view-named *Window* "OpenGL text")) :str "the quick brown fox jumps over the lazy dog.")
(set-string-shape (ss-content (view-named *Window* "OpenGL text")) :str "The Quick Brown Fox Jumps Over The Lazy Dog.")
(set-string-shape (ss-content (view-named *Window* "OpenGL text")) :str "~`!@#$%^&*()_-+={[}]|\\:;\"'<,>.?/")
(set-string-shape (ss-content (view-named *Window* "OpenGL text")) :str (format nil "0123456789~%9876543210~%"))

(set-string-shape (ss-content (view-named *Window* "OpenGL text")) :font (get-font *Font-Manager* "comic sans ms"))
(set-string-shape (ss-content (view-named *Window* "OpenGL text")) :font (get-font *Font-Manager* "helvetica"))
(set-string-shape (ss-content *Window*) :font (get-font *Font-Manager* "geneva"))
(set-string-shape (ss-content *Window*) :font (get-font *Font-Manager* "monaco"))
(set-string-shape (ss-content *Window*) :font (get-font *Font-Manager* "arial"))
(set-string-shape (ss-content *Window*) :font (get-font *Font-Manager* "lucida grande"))

|#
