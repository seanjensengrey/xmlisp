;;-*- Mode: Lisp; Package: xlui -*-
;*********************************************************************
;*                                                                   *
;*            V A L U E   E D I T O R S                              *
;*                                                                   *
;*********************************************************************
;* Author       : Alexander Repenning, alexander@agentsheets.com     *
;*                http://www.agentsheets.com                         *
;* Copyright    : (c) 1996-2009, AgentSheets Inc.                    *
;* Filename     : Value-Editors.lisp                                 * 
;* Last Update  : 10/04/07                                           *
;* Version      :                                                    *
;*    1.0       : 01/28/05                                           *
;*    1.1       : 07/14/05 mandatory use for instructions, CODECs    *
;*    1.1.1     : 07/29/05 Color-Editor                              *
;*    1.1.2     : 08/11/05 most-specific-class                       *
;*    1.1.3     : 08/15/05 generic-editor                            *
;*    1.2       : 08/30/05 set-value-from-slot-attribute             *
;*    1.2.1     : 09/14/05 static-string-editor without encode       *
;*    1.3       : 10/11/05 variant-name-editor                       *
;*    1.4       : 11/09/05 key-editor edit AgentSheets style         *
;*    1.4.1     : 11/22/05 != operator for comparator-editor         *
;*    1.4.2     : 02/06/06 agent-class-name-editor is pop-up         *
;*    1.4.3     : 02/10/06 background-name-editor                    *
;*    1.4.4     : 02/23/06 extended-agent-class-name-editor          *
;*    1.5       : 06/26/06 shape-name-editor based on 2D pop up      *
;*    1.5.1     : 07/06/06 use ad3d: host                            *
;*    1.5.2     : 02/16/07 edit :after draws containing view         *
;*    1.6       : 08/13/07 shape-name-editor - shape-selector-window *
;*    1.6.1     : 08/29/07 sound-name-editor for QickTime sounds     *
;*    1.6.2     : 10/04/07 use class documentation as tooltip        *
;*    2.0       : 10/29/09 Clozure CL                                *
;* Systems      : Intel-Mac, CCL 1.4, OS X 10.6.1                    *
;* Abstract     : Classes to edit attribute values                   *
;*                                                                   *
;*********************************************************************

(in-package :xlui)


(defclass VALUE-EDITOR (agent-3d)
  ((width :accessor width :initform 0.0 :initarg :width)
   (height :accessor height :initform 0.0 :initarg :height)
   (slot-name :accessor slot-name :initform nil :initarg :slot-name :documentation "the name of the slot I am representing"))
  (:documentation "Base class of all XML attribute value editors"))


(defmethod PRINT-SLOTS ((Self value-editor))
  '())


(defmethod XML-PRINTABLE-AS-ATTRIBUTE-VALUE-P ((Self value-editor)) t)


(defmethod XML-PRINTABLE-AS-SUBELEMENT-P ((Self value-editor)) nil)


(defmethod PRINT-TYPED-ATTRIBUTE-VALUE ((Editor value-editor) Type Stream)
  (declare (ignore Type))
  ;; editor should only print its value, not itself
  (let ((Value (value Editor)))
    (format
     Stream
     "\"~A\""
     (etypecase Value
       (string (encode-xml-string Value))
       (number Value)
       (symbol Value)
       (list Value)))))


(defgeneric VALUE (Value-Editor)
  (:documentation "get my current value"))


(defgeneric (SETF VALUE) (Value Value-Editor)
  (:documentation "set my value"))


(defgeneric SET-VALUE-FROM-SLOT-ATTRIBUTE (Value-Editor Xml-Editor Slot-Definition)
  (:documentation "
  in: Value-Editor, Xml-Editor, Slot-Definition.
  Call (setf (value Self) <value>) with <value> derived from <slot-definition> from <xml-editor>.
  Value may be encoded version of slot-value."))


(defgeneric EDIT (Value)
  (:documentation "Let user edit my value"))

;__________________________________
; Default Implementation           |
;__________________________________

(defmethod SET-VALUE-FROM-SLOT-ATTRIBUTE ((Self value-editor) (Xml-Editor xml-editor) Slot-Definition)
  ;; if slot is unbound leave the value editor as is
  (unless (slot-boundp Xml-Editor (slot-definition-name Slot-Definition))
    (return-from set-value-from-slot-attribute))
  ;; as default assume the value is a string and try to derive information from slot-definition-type
  (let ((Type (slot-definition-type Slot-Definition))
        (Value (slot-value Xml-Editor (slot-definition-name Slot-Definition))))
    (setf (value Self)
          (if Type
            ;; typed
            (read-from-string  ;; output of print-typed-attribute-value includes "..."
             (with-output-to-string (stream)
               (print-typed-attribute-value Value Type Stream)))
            ;; untyped
            (etypecase Value
              (string (encode-xml-string Value))
              (number (write-to-string Value))
              (symbol (symbol-name Value))
              (list (write-to-string Value)))))))


(defmethod EDIT ((Self value-editor))
  (format t "~%;;; edit ~A=\"~A\" of type ~A" (name Self) (value Self) (type-of Self)))


(defmethod EDIT :after ((Self value-editor))
  ;; in most cases a good idea to draw the containing view
  (when (view Self)
    (display (view Self))))


(defmethod VALUE ((Self t))
  ;; super generic implementatilon
  Self)


(defmethod MOUSE-CLICK-EVENT-HANDLER ((Self value-editor))
  (edit Self))


(defun MAKE-VALUE-EDITOR (Class-Name Value &rest Args) "
  in:  Class-Name symbol; Value t; &rest Args
  out: Value-Editor value-editor.
  Create and initialize value editor."
  (let ((Editor (apply #'make-instance (most-specific-class Class-Name) Args)))
    (setf (value Editor) Value)
    Editor))


(defmethod PROCESSED-TOOLTIP-TEXT ((Self value-editor))
  ;; use the class documentation as tooltip
  (documentation (type-of Self) 'type))

;__________________________________
; Static String Editor             |
;__________________________________

(defclass STATIC-STRING-EDITOR (string-shape value-editor)
  ()
  (:documentation "static string"))


(defmethod VALUE ((Self static-string-editor))
  (str Self))


(defmethod (SETF VALUE) (Value (Self static-string-editor))
  (set-string-shape Self :str Value))


(defmethod SET-VALUE-FROM-SLOT-ATTRIBUTE ((Self static-string-editor) (Xml-Editor xml-editor) Slot-Definition)
  ;; no value translation
  (setf (value Self) (slot-value XML-Editor (slot-definition-name Slot-Definition))))


(defmethod DRAW ((Self static-string-editor))
  (when (is-visible Self) (call-next-method)))


(defmethod FIND-AGENT-BY-REFERENCE-ID ((Self static-string-editor) Reference-Id)
  (declare (ignore Reference-Id))
  ;; no need to find me since I cannot be edited
  )


(defmethod MOUSE-CLICK-EVENT-HANDLER ((Self static-string-editor))
  ;; nothing to edit here. Just reroute the click to my container
  (mouse-click-event-handler (part-of Self)))

;__________________________________
; Property Label Name              |
;__________________________________

(defclass PROPERTY-NAME-LABEL (static-string-editor)
  ()
  (:documentation "Adjusts width according to window width"))



(defmethod WIDTH ((Self property-name-label))
  ;; 40% of window width
  (let ((View (view Self)))
    (if View
      (* 0.4 (- (object-coordinate-window-width View) (* 3 *Spacing*)))
      (call-next-method))))


;__________________________________
; Link String Editor               |
;__________________________________


(defclass LINK-STRING-EDITOR (string-shape value-editor)
  ((click-action :accessor click-action :initform nil :initarg :click-action :documentation "run this function on click #'(lambda (Self) ...)"))
  (:default-initargs 
    :color "0000FF") ;; blue color
  (:documentation "String representing a URL-like link"))


(defmethod VALUE ((Self link-string-editor))
  (str Self))


(defmethod (SETF VALUE) (Value (Self link-string-editor))
  (set-string-shape Self :str Value))


(defmethod MOUSE-HOVER-ENTER-EVENT-HANDLER ((Self link-string-editor))
  (call-next-method)
  #+:MCL (#_SetThemeCursor #$kThemePointingHandCursor)
  (setf (color Self) "0000FF"))


(defmethod MOUSE-HOVER-LEAVE-EVENT-HANDLER ((Self link-string-editor))
  (call-next-method)
  #+:MCL (#_SetThemeCursor #$kThemeArrowCursor)
   ;; (setf (color Self) "000000")
   )


(defmethod OPEN-LINK ((Self link-string-editor))
  (when (click-action Self) (funcall (click-action Self) Self)))


(defmethod MOUSE-CLICK-EVENT-HANDLER ((Self link-string-editor))
  (open-link Self))


(defmethod DRAW ((Self link-string-editor))
  (unless (is-visible Self) (return-from draw))
  (call-next-method)
  (when (is-hovered Self) (draw-string-underline Self)))

;__________________________________
; String Editor                    |
;__________________________________


(defclass STRING-EDITOR (editable-string-shape value-editor)
  ((horizontal-text-padding :accessor horizontal-text-padding :initform 0.02 :allocation :class))
  (:documentation "Basic string editor"))


(defmethod VALUE ((Self string-editor))
  (str Self))


(defmethod (SETF VALUE) (Value (Self string-editor))
  (set-string-shape Self :str Value))


(defmethod UPDATE-TEXT-BOX-DIMENSION ((Self string-editor))
  (multiple-value-bind (Width Height) (text-box-dimension (str Self) (font Self))
    (setf (width Self) (* (size Self) (+ Width (* 2 (horizontal-text-padding Self)))))
    (setf (height Self) (* (size Self) Height))))


(defmethod SELECTED-TEXTURE ((Self string-editor))
  "editTextBox.png")


(defmethod UNSELECTED-TEXTURE ((Self string-editor))
  "editTextBox.png")


(defmethod DRAW ((Self string-editor))
  (unless (is-visible Self) (return-from draw))
  ;; tone background back unless if selected or hovered
  ;;(glcolor4f 1.0 1.0 1.0 (if (is-selected (part-of Self)) 1.0 (if (is-hovered (part-of Self)) 0.7 0.07)))
  (draw-stretched-texture 
   Self
   (if (or (is-selected Self) (is-hovered Self))
     (selected-texture Self)
     (unselected-texture Self))
   0.0 0.0
   (width Self) (height Self)
   0.12 0.02)
  (glcolor4f 1.0 1.0 1.0 1.0)
  (glTranslatef (* (size Self) (horizontal-text-padding Self)) 0.0 0.001)
  (call-next-method))


(defmethod EDIT ((Self editable-string-shape))
  (setf (value Self) (get-string-from-user
                      (format nil "~A=" (string-capitalize (slot-name Self)))
                      :initial-string (value Self)
                      #| :position (add-points (view-mouse-position nil) #@(-33 -86)) |#))
  ;; adjust attribute value of slot I am editing
  (set-attribute-value (part-of Self) (slot-name Self) (value Self))
  ;; layout 
  (let ((View (view (part-of Self))))
    (dolist (Agent (agents View))
      (layout Agent))
    (display View)
    ;; (setf (window-needs-saving-p (window Self)) t)
    ))

;;*********************************
;; generic-editor                 *
;;*********************************

(defclass GENERIC-EDITOR (string-editor)
  ()
  (:documentation "code/decode everything as string"))


(defmethod PRINT-TYPED-ATTRIBUTE-VALUE ((Value t) (Type (eql 'generic-editor)) Stream)
  (format Stream "\"~A\"" Value))


(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value t) (Type (eql 'generic-editor)))
  (read-from-string Value))


;;*********************************
;; string-menu-editor             *
;;*********************************

(defclass STRING-MENU-EDITOR (string-editor)
  ((pop-up-menu :accessor pop-up-menu :initform nil :documentation "used to pop up legal values"))
  (:documentation "Menu"))


(defgeneric MENU-STRINGS (String-Menu-Editor)
  (:documentation "return list of strings to be used in menu"))


(defmethod MENU-STRINGS ((Self string-menu-editor))
  ;; not very useful: need to overwrite 
  '("Colorado" "Utah" "California"))


(defmethod MAKE-POP-UP-P ((Self string-menu-editor))
  ;; if true need to make a pop up: redefine make more dynamic menues
  (not (pop-up-menu Self)))


(defmethod SELECTED-TEXTURE ((Self string-menu-editor))
  "popUp.png")


(defmethod UNSELECTED-TEXTURE ((Self string-menu-editor))
  "popUp.png")


(defmethod EDIT ((Self string-menu-editor))
  ;; just in time menu
  (when (make-pop-up-p Self)
    (setf (pop-up-menu Self)
          (make-instance 'pop-up-menu
            #| :view-position #@(20 55) |#
            :view-font '("Geneva" 9)
            :default-item 0
            :auto-update-default nil
            :menu-items
            (mapcar 
             #'(lambda (String)
                 (make-instance 'menu-item
                   :menu-item-title String
                   :menu-item-action #'(lambda () (setf (value Self) String))))
             (menu-strings Self)))))
  (add-subviews (window Self) (pop-up-menu Self))
  ;; (set-view-position (pop-up-menu Self) (add-points (view-mouse-position (window Self)) #@(-20 -10)))
  ;; activate pop up window
  (menu-select (pop-up-menu Self) 0)
  (remove-subviews (window Self) (pop-up-menu Self)) ;; no point in keeping it in there
  ;; adjust attribute value of slot I am editing
  (set-attribute-value (part-of Self) (slot-name Self) (value Self))
  ;; layout 
  (let ((View (view (part-of Self))))
    (dolist (Agent (agents View))
      (layout Agent))
    (display View)
    (setf (window-needs-saving-p (window Self)) t)))


;;*********************************
;; Comparator-editor              *
;;*********************************

(defclass COMPARATOR-EDITOR (string-menu-editor)
  ()
  (:documentation "Compare values"))


(defmethod MENU-STRINGS ((Self comparator-editor))
  '(">" ">=" "!=" "<" "<=" "="))


(defmethod SET-VALUE-FROM-SLOT-ATTRIBUTE ((Self comparator-editor) (Xml-Editor xml-editor) Slot-Definition)
  ;; no value translation
  (setf (value Self) (slot-value XML-Editor (slot-definition-name Slot-Definition))))


;;*********************************
;; adjacency-editor               *
;;*********************************

(defclass ADJACENCY-EDITOR (string-menu-editor)
  ()
  (:documentation "where in stack are things?"))


(defmethod MENU-STRINGS ((Self adjacency-editor))
  '("immediately above" "somewhere above" "immediately below" "somewhere below" "above or below"))


;;*********************************
;; Numbers                        *
;;*********************************

;__________________________________
; Integer Editor                   |
;__________________________________


(defclass INTEGER-EDITOR (string-editor)
  ()
  (:documentation "Integer"))

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'integer-editor)) Stream)
  (print-typed-attribute-value Value 'integer Stream))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'integer-editor)))
  (read-typed-attribute-value Value 'integer))


;__________________________________
; Float Editor                     |
;__________________________________


(defclass FLOAT-EDITOR (string-editor)
  ()
  (:documentation "Float"))


(defmethod (SETF VALUE) (Value (Self float-editor))
  (set-string-shape Self :str (format nil "~F" (read-from-string Value))))


;__________________________________
; Property Value                   |
;__________________________________

(defclass PROPERTY-VALUE (float-editor)
  ()
  (:documentation "Value editor for properties. Adjusts width according to window width"))


(defmethod WIDTH ((Self property-value))
  ;; 60% of window width
  (let ((View (view Self)))
    (if View
      (* 0.6 (- (object-coordinate-window-width View) (* 3 *Spacing*)))
      (call-next-method))))

;__________________________________
; Formula Editor                   |
;__________________________________

(defclass FORMULA-EDITOR (string-editor)
  ()
  (:documentation "Formula
example:  12.3 + sin(angle) / @count

angle is an agent attribute
sin() is a function
@count is a simulation attribute"))




(defmethod SET-VALUE-FROM-SLOT-ATTRIBUTE ((Self formula-editor) (Xml-Editor xml-editor) Slot-Definition)
  ;; no value translation
  (setf (value Self) (slot-value XML-Editor (slot-definition-name Slot-Definition))))

;__________________________________
; Boolean Editor                   |
;__________________________________

(defclass BOOLEAN-EDITOR (string-editor)
  ()
  (:documentation "Boolean"))

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'boolean-editor)) Stream)
  (print-typed-attribute-value Value 'boolean Stream))


(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'boolean-editor)))
  (read-typed-attribute-value Value 'boolean))

;;*********************************
;; Names                          *
;;*********************************

;; Agent Class Name

(defclass AGENT-CLASS-NAME-EDITOR (string-menu-editor)
  ()
  (:documentation "Agent Class Name"))


(defmethod MENU-STRINGS ((Self agent-class-name-editor))
  (mapcar 
   #'string-capitalize
   (agent-class-names (project-manager *Project-Manager-Window*))))


(defmethod MAKE-POP-UP-P ((Self agent-class-name-editor))
  ;; agents get added and removed all the time 
  t)


(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'agent-class-name-editor)) Stream)
  (prin1 (ad3d-class-string Value) Stream))


(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'agent-class-name-editor)))
  (ad3d-class-symbol Value))


;; Extended Agent Class Name

(defclass EXTENDED-AGENT-CLASS-NAME-EDITOR (agent-class-name-editor)
  ()
  (:documentation "Agent Class Name or _every-agent"))


(defmethod MENU-STRINGS ((Self extended-agent-class-name-editor))
  (append (call-next-method) (list "_every-agent")))


(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'extended-agent-class-name-editor)) Stream)
  (prin1 (ad3d-class-string Value) Stream))


(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'extended-agent-class-name-editor)))
  (ad3d-class-symbol Value))


;; AGENT-INSTANCE-NAME-EDITOR

(defclass AGENT-INSTANCE-NAME-EDITOR (string-editor)
  ()
  (:documentation "Agent Instance Name"))


(defclass METHOD-NAME-EDITOR (string-editor)
  ()
  (:documentation "Method Name"))

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'method-name-editor)) Stream)
  (prin1 (ad3d-method-string Value) Stream))


(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'method-name-editor)))
  (ad3d-method-symbol Value))


;__________________________________
; Sound Name                       |
;__________________________________

(defclass SOUND-NAME-EDITOR (string-menu-editor)
  ()
  (:documentation "Sound Name"))


(setq *Secondary-Sound-File-Directory-Hook*
      #'(lambda () 
          (when *Project-Manager-Window*
            (make-pathname
             :directory (append (pathname-directory (file *Project-Manager-Window*)) '("sounds"))))))


(defmethod MENU-STRINGS ((Self sound-name-editor))
  (mapcar
   #'(lambda (Pathname) 
       (format nil "~A.~A" (pathname-name Pathname) (pathname-type Pathname)))
   (sound-files-in-sound-file-directory)))


(defmethod MAKE-POP-UP-P ((Self sound-name-editor))
  ;; just in case user added sounds 
  t)

;__________________________________
; Mouse Modifier                   |
;__________________________________

(defclass MOUSE-MODIFIER-NAME-EDITOR (string-menu-editor)
  ()
  (:documentation "mouse click modifier"))


(defmethod MENU-STRINGS ((Self mouse-modifier-name-editor))
  '("no modifier" "control" "option" "command" "shift control" "shift option" "shift command" "double"))

;__________________________________
; World Name                       |
;__________________________________

(defclass WORLD-NAME-EDITOR (string-menu-editor)
  ()
  (:documentation "World Name"))


(defmethod MENU-STRINGS ((Self world-name-editor))
  (world-file-names))


(defmethod MAKE-POP-UP-P ((Self world-name-editor))
  ;; this is too eager: could cache menu-strings and only make true if different
  t)


;__________________________________
; Background Name                   |
;__________________________________

(defclass BACKGROUND-NAME-EDITOR (string-menu-editor)
  ()
  (:documentation "Background Name"))


(defmethod MENU-STRINGS ((Self background-name-editor))
  (background-file-names))


(defmethod MAKE-POP-UP-P ((Self background-name-editor))
  ;; this is too eager: could cache menu-strings and only make true if different
  t)

;__________________________________
; Attribute Name                   |
;__________________________________

(defclass ATTRIBUTE-NAME-EDITOR (string-editor)
  ()
  (:documentation "Attribute Name"))

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE ((Value t) (Type (eql 'attribute-name-editor)) Stream)
  (prin1 (string-capitalize (symbol-name Value)) Stream))


(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value t) (Type (eql 'attribute-name-editor)))
  (intern (string-upcase Value) :ad3d))



;__________________________________
; Voice Name                       |
;__________________________________

(defclass VOICE-NAME-EDITOR (string-menu-editor)
  ()
  (:documentation "Voice Name"))


(defvar *Voice-Names-Available* nil "list of voice names")


(defmethod MENU-STRINGS ((Self voice-name-editor))
  (or *Voice-Names-Available*
      (setq *Voice-Names-Available* (available-voices))))
 
;;*********************************
;; Shape Name                     *
;;*********************************

;___________________________________
; Shape-icon-pop-up-image-menu      |
;___________________________________

(defclass SHAPE-ICON-POP-UP-IMAGE-MENU (pop-up-image-menu)
  ((image-names :accessor image-names :initform nil :allocation :class :documentation "determine before pop up"))
  (:default-initargs
    #| :normalized-size #@(32 32) |#)  ;; pixel size of pop up icons
  (:documentation "Shape Icons"))


(defgeneric GARBAGE-COLLECT-SHAPE-ICON (Shape-Icon-Pop-Up-Image-Menu Name)
  (:documentation "Shape <name> no longer exists. Garbage collect all side effects"))


(defmethod GARBAGE-COLLECT-SHAPE-ICON ((Self shape-icon-pop-up-image-menu) Name)
  (format t "~%;; GC shape icon=\"~A\"" Name))


(defmethod POP-UP ((Self shape-icon-pop-up-image-menu) &optional Position)
  (declare (ignore Position))
  ;; determine images based on currently open project
  (let ((New-Names
         (let ((Shapes))
           (dolist (Agent-Manager (agent-managers (project-manager *Project-Manager-Window*)))
             (dolist (Shape (shapes Agent-Manager))
               (push Shape Shapes)))
           (mapcar #'name Shapes))))
    ;; GC expired names
    (dolist (Name (image-names Self))
      (unless (find Name New-Names)
        (garbage-collect-shape-icon Self Name)))
    ;; adjust list
    (setf (image-names Self) New-Names))
  (call-next-method))
  

(defmethod IMAGE-NAME-PATHNAME ((Self shape-icon-pop-up-image-menu) Name)
  ;; name may not be unique!!!
  (let ((Shape (find-first-shape (project-manager *Project-Manager-Window*) Name)))
    (if Shape
      (icon-pathname Shape)
      "ad3d:resources;textures;missing_image.png")))

;__________________________________________
; Shape Name Editor                        |
;__________________________________________

(defclass SHAPE-NAME-EDITOR (value-editor)
  ((shape :accessor shape :initform nil :initarg :shape)
   (value :accessor value :initform nil :documentation "shape name string"))
  (:documentation "Shape Name"))

;; CODECs

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'shape-name-editor)) Stream)
  (prin1 (ad3d-string Value) Stream))


(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'shape-name-editor)))
  (ad3d-symbol Value))

;; value

(defmethod (SETF VALUE) :after (Value (Self shape-name-editor))
  ;; if there is a shape icon matching my value 
  (setf (shape Self) 
        (or (find-first-shape (project-manager *Project-Manager-Window*) (ad3d-symbol Value))
            (the-missing-shape-placeholder))))


;; accessors

(defmethod POP-UP-IMAGE-MENU ((Self shape-name-editor))
  ;; make one if needed
  (or (slot-value Self 'pop-up-image-menu)
      (setf (slot-value Self 'pop-up-image-menu) (make-instance 'shape-icon-pop-up-image-menu))))


;; Display & Layout

(defmethod DRAW ((Self shape-name-editor))
  (unless (is-visible Self) (return-from DRAW))
  (when (shape Self)
    (draw-as-proxy-icon (shape Self))))


(defmethod LAYOUT ((Self shape-name-editor))
  ;; use shape size but fall back onto default if no shape
  (let ((Scale (or (or (and (shape Self) (proxy-icon-scale (shape Self))) 0.15))))
    (setf (height Self) Scale)
    (setf (width Self) Scale)))

;; User interactions


(defmethod EDIT ((Self shape-name-editor))
  (multiple-value-bind (Cathegory Shape-Name Shape)
                       (user-select-shape (all-shapes-as-tree (project-manager *project-manager-window*)))
    (declare (ignore Cathegory Shape))
    (when Shape-Name
      (setf (value Self) Shape-Name)
      ;; adjust attribute value of slot I am editing
      (set-attribute-value (part-of Self) (slot-name Self) (value Self))
      (setf (window-needs-saving-p (window Self)) t))))

;;*********************************
;; Input Devices                  *
;;*********************************

(defclass KEY-EDITOR (string-editor)
  ((horizontal-text-padding :initform 0.04)
   (value :type integer :documentation "key chord code. accessor needs to translate into platform independent name"))
  (:default-initargs
    :size 2.0
    :font (get-font *Font-Manager*  "charcoal cy"))
  (:documentation "Key or key chord"))


(defmethod VALUE ((Self key-editor))
  ;; translate into string
  (key-chord-code->name (slot-value Self 'value)))


(defmethod (SETF VALUE) (Value (Self key-editor))
  ;; in comes the name
  (setf (slot-value Self 'value) (key-chord-name->code Value)) ;; which gets turned into the chord code
  (set-string-shape Self :str (key-chord-code->label (slot-value Self 'value))))  ;; and that code back into the button label



(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'key-editor)) Stream)
  (format Stream "\"~A\"" (key-chord-code->name Value)))


(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'key-editor)))
  (let ((Code (key-chord-name->code Value)))
    ;; is warning suffient? Could open stream in editor and show position
    (unless Code (warn "~A is not a valid key name" Value))
    Code))


(defmethod UNSELECTED-TEXTURE ((Self key-editor))
  "PowerBookKey.png")

(defmethod SELECTED-TEXTURE ((Self key-editor))
  "PowerBookKey.png")


(defmethod EDIT ((Self key-editor))
  (let ((Old-String (str Self)))
    (set-string-shape Self :str "Press any key")
    (let ((Width (width Self)))
      ;; layout 
      (let ((View (view (part-of Self))))
        (dolist (Agent (agents View))
          (layout Agent))
        (view-draw-contents (window Self))
        (loop (unless (mouse-down-p) (return)))
        ;; "press the key"  animation
        (let* ((Prompt "Press any key on the keyboard. Press any key on the keyboard")
               (Time (get-internal-real-time))
               (Stop-Time (+ Time (* 10 internal-time-units-per-second)))
               (Index 0)
               (Code (catch :exit-user-define-key-chord 
                       (user-define-key-chord
                        :idle-function 
                        #'(lambda ()
                            (cond
                             ;; Animation Frame
                             ((> (get-internal-real-time) Time)  ;; else animate
                              (incf Time #.(truncate (* 0.07 internal-time-units-per-second)))
                              (set-string-shape Self :str (subseq Prompt Index (+ Index 12)))
                              (setf (width Self) Width) ;; overwrite proportional font width
                              (view-draw-contents (window Self))
                              (if (> Index 29)
                                (setq Index 0)
                                (incf Index)))
                             ;; Timeout: not a good idea to keep hanging here
                             ((> (get-internal-real-time) Stop-Time)
                              (throw :exit-user-define-key-chord nil))
                             ;; Mouse
                             ((mouse-down-p) 
                              (throw :exit-user-define-key-chord nil))))))))
          (cond
           ;; valid code
           (Code
            (set-string-shape Self :str (key-chord-code->label Code))
            (setf (slot-value Self 'value) Code)
            ;; adjust attribute value of slot I am editing
            (set-attribute-value (part-of Self) (slot-name Self) (value Self))
            (setf (window-needs-saving-p (window Self)) t))
           ;; invalid or timeout: fall back to last value
           (t 
            (set-string-shape Self :str Old-String)))
          ;; cleanup layout
          (let ((View (view (part-of Self))))
            (dolist (Agent (agents View))
              (layout Agent))
            (view-draw-contents (window Self))))))))
  
;;*********************************
;; Containers                     *
;;*********************************

(defclass INSTRUCTIONS-EDITOR (string-editor)
  ()
  (:documentation "Instructions"))


(defclass INSTRUCTION-EDITOR (string-editor)
  ()
  (:documentation "Instruction"))


(defclass ACTIONS-EDITOR (string-editor)
  ()
  (:documentation "Actions"))


(defclass CONDITIONS-EDITOR (string-editor)
  ()
  (:documentation "Conditions"))


(defclass METHODS-EDITOR (string-editor)
  ()
  (:documentation "Methods"))


(defclass RULES-EDITOR (string-editor)
  ()
  (:documentation "Rules"))


;;*********************************
;; Disclosure                     *
;;*********************************

(defclass DISCLOSURE-EDITOR (value-editor)
  ((value :accessor value :initform nil :type boolean :initarg :value))
  (:documentation "Disclosure")
  (:default-initargs 
    :height 0.06 
    :width 0.06))


(defmethod SET-VALUE-FROM-SLOT-ATTRIBUTE ((Self disclosure-editor) (Xml-Editor xml-editor) Slot-Definition)
  ;; no value translation
  (setf (value Self) (slot-value XML-Editor (slot-definition-name Slot-Definition))))


(defmethod PRINT-SLOTS ((Self disclosure-editor))
  '(value))

;; CODECs

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE ((Value t) (Type (eql 'disclosure-editor)) Stream)
  (print-typed-attribute-value Value 'boolean Stream))


(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value t) (Type (eql 'disclosure-editor)))
  (read-typed-attribute-value Value 'boolean))



(defmethod SELECTED-TEXTURE ((Self disclosure-editor))
  (if (value Self)
    "disclosureDown.png"
    "disclosureRight.png"))


(defmethod UNSELECTED-TEXTURE ((Self disclosure-editor))
  (selected-texture Self))


(defmethod EDIT ((Self disclosure-editor))
  (setf (value Self) (not (value Self)))
  ;; adjust attribute value of slot I am editing
  (set-attribute-value (part-of Self) (slot-name Self) (value Self)))


(defmethod DRAW ((Self disclosure-editor))
  (unless (is-visible Self) (return-from draw))
  (glcolor4f 1.0 1.0 1.0 (if (is-selected Self) 1.0 (if (is-hovered Self) 0.9 0.3)))
  (draw-texture 
   Self
   (if (or (is-selected Self) (is-hovered Self))
     (selected-texture Self)
     (unselected-texture Self))
   0.0 0.0
   (width Self) (height Self))
  (glcolor4f 1.0 1.0 1.0 1.0))


;;*********************************
;; Check Box                      *
;;*********************************

(defclass CHECK-BOX-EDITOR (value-editor)
  ((value :accessor value :initform nil :type boolean :initarg :value))
  (:documentation "Check Box")
  (:default-initargs 
    :height 0.1 
    :width 0.1))


(defmethod SET-VALUE-FROM-SLOT-ATTRIBUTE ((Self check-box-editor) (Xml-Editor xml-editor) Slot-Definition)
  ;; no value translation
  (setf (value Self) (slot-value XML-Editor (slot-definition-name Slot-Definition))))


(defmethod PRINT-SLOTS ((Self check-box-editor))
  '(value))

;; CODECs

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE ((Value t) (Type (eql 'check-box-editor)) Stream)
  (print-typed-attribute-value Value 'boolean Stream))


(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value t) (Type (eql 'check-box-editor)))
  (read-typed-attribute-value Value 'boolean))



(defmethod SELECTED-TEXTURE ((Self check-box-editor))
  (if (value Self)
    "checkboxTrue.png"
    "checkboxFalse.png"))


(defmethod UNSELECTED-TEXTURE ((Self check-box-editor))
  (selected-texture Self))


(defmethod EDIT ((Self check-box-editor))
  (setf (value Self) (not (value Self)))
  ;; adjust attribute value of slot I am editing
  (set-attribute-value (part-of Self) (slot-name Self) (value Self))
  (setf (window-needs-saving-p (window Self)) t))


(defmethod DRAW ((Self check-box-editor))
  (unless (is-visible Self) (return-from draw))
  (draw-texture 
   Self
   (if (or (is-selected Self) (is-hovered Self))
     (selected-texture Self)
     (unselected-texture Self))
   0.0 0.0
   (width Self) (height Self))
  (glcolor4f 1.0 1.0 1.0 1.0))


;;*********************************
;; Colors                         *
;;*********************************


(defclass COLOR-EDITOR (value-editor)
  ((color :accessor color :initform nil :documentation "holds a 32 bit RGBA vector but only 24 bits are used"))
  (:documentation "24 bit RGB Color Hex String, e.g., \"FF0000\" for red")
  (:default-initargs 
    :height 0.15 
    :width 0.15))


(defmethod VALUE ((Self color-editor))
  ;; return Hex string, e.g, "FF0000" for red
  (when (color Self)
    (let ((*Print-Base* 16))
      (write-to-string 
       (rgb-to-fixnum 
        (get-byte (color Self))
        (get-byte (color Self) 1)
        (get-byte (color Self) 2))))))
      
                   
(defmethod (SETF VALUE) (Value (Self color-editor))
  ;; set Hex string, e.g., "FF0000" for red
  (unless (color Self)    ;; just in time vector
    (setf (color Self) (make-byte-vector 255 255 255 255)))
  (let ((*Read-Base* 16))
    (multiple-value-bind (Red Green Blue)
                         (fixnum-to-rgb (read-from-string Value))
      (set-byte-vector (color Self) Red Green Blue))))


;; converter

(defun RGB-TO-FIXNUM (Red Green Blue) "
  in:  Red Green Blue : byte.
  out: RGB fixnum.
  Convert RGBA to fixnum."
 (+ (lsh Red 16)
    (lsh Green 8)
    Blue))


(defun FIXNUM-TO-RGB (Number) "
  in:  Number fixnum.
  out: Red Green Blue byte.
  Convert Number to color values."
 (values
  (logand (lsh Number -16) #xFF)
  (logand (lsh Number -8) #xFF)
  (logand Number #xFF)))


(defmethod PRINT-SLOTS ((Self color-editor))
  '())


(defmethod SELECTED-TEXTURE ((Self color-editor))
  "color_well.png")


(defmethod UNSELECTED-TEXTURE ((Self color-editor))
  (selected-texture Self))


(defmethod EDIT ((Self color-editor))
  (let ((Color (user-pick-color))
        (*Print-Base* 16))
    (setf (value Self) 
          (write-to-string 
           (rgb-to-fixnum
            (truncate (color-red Color) 256)
            (truncate (color-green Color) 256)
            (truncate (color-blue Color) 256)))))
  ;; adjust attribute value of slot I am editing
  (set-attribute-value (part-of Self) (slot-name Self) (value Self))
  (setf (window-needs-saving-p (window Self)) t))


(defmethod DRAW ((Self color-editor))
  (unless (is-visible Self) (return-from draw))
  (glDisable gl_lighting)
  (when (color Self)
    ;; colorizing shape but ignore alpha
    (glcolor3ubv (color Self)))
  (draw-texture
   Self
   (if (or (is-selected Self) (is-hovered Self))
     (selected-texture Self)
     (unselected-texture Self))
   0.0 0.0
   (width Self) (height Self))
  (glcolor4f 1.0 1.0 1.0 1.0))


;********************************
;* Variant Name Editor          *
;********************************

(defclass VARIANT-NAME-EDITOR (string-menu-editor)
  ((variants :accessor variants :initform nil :initarg :variants :documentation "list of XML strings"))
  (:documentation "Instruction variant"))


(defmethod VARIANT-INSTRUCTIONS ((Self variant-name-editor))
  (let ((*Package* (find-package :ad3d)))
    (mapcar #'read-from-string (variants Self))))


(defmethod MENU-STRINGS ((Self variant-name-editor))
  (mapcar #'xml-tag-name-string (variant-instructions Self)))


(defmethod EDIT ((Self variant-name-editor))
  ;; just in time menu
  (unless (pop-up-menu Self)
    (setf (pop-up-menu Self)
          (make-instance 'pop-up-menu
            #| :view-position #@(20 55) |#
            :view-font '("Geneva" 9)
            :menu-items
            (mapcar 
             #'(lambda (String)
                 (make-instance 'menu-item
                   :menu-item-title String
                   :menu-item-action #'(lambda () (setf (value Self) String))))
             (menu-strings Self)))))
  (add-subviews (window Self) (pop-up-menu Self))
  ;; (set-view-position (pop-up-menu Self) (add-points (view-mouse-position (window Self)) #@(-20 -10)))
  ;; activate pop up window
  (menu-select (pop-up-menu Self) 0)
  (remove-subviews (window Self) (pop-up-menu Self)) ;; no point in keeping it in there
  ;; switch to variant
  (let ((Container (part-of (part-of Self)))
        (Instruction (find (value Self) (variant-instructions Self) 
                           :key #'xml-tag-name-string
                           :test #'string-equal)))
    (setf (view Instruction) (view Self))
    (setf (part-of Instruction) Container)
    (switch-subobject Container (part-of Self) Instruction))
  ;; layout 
  (let ((View (view (part-of Self))))
    (dolist (Agent (agents View))
      (layout Agent))
    (display View)
    (setf (window-needs-saving-p (window Self)) t)))

;********************************
;* Animation                    *
;********************************

(defclass ANIMATION-SPEED-EDITOR (string-menu-editor)
  ()
  (:documentation "Using constant speed or acceleration"))


(defmethod MENU-STRINGS ((Self animation-speed-editor))
  '("constant" "accelerated"))



#| Examples:


(instruction make-sound ((Disclosed disclosure-editor) (Sound sound-name-editor) (Check check-box-editor))
  :superclass basic-action)


(defparameter *Instruction* <make-sound sound="click" disclosed="true" check="true"/>)

(edit *Instruction*)



(instruction compare ((a formula-editor "value1") (operator comparator-editor ">") (b formula-editor "value2"))
  :superclass basic-condition)

(edit <compare />)


|#