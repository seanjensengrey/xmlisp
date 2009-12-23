;;-*- Mode: Lisp; Package: xlui -*-
;*********************************************************************
;*                                                                   *
;*            X M L   E D I T O R                                    *
;*                                                                   *
;*********************************************************************
;* Author       : Alexander Repenning, alexander@agentsheets.com     *
;*                http://www.agentsheets.com                         *
;* Copyright    : (c) 1996-2009, AgentSheets Inc.                    *
;* Filename     : XML-Editor.lisp                                    * 
;* Last Update  : 10/29/09                                           *
;* Version      :                                                    *
;*    1.0       : 01/27/05                                           *
;*    1.1       : 05/26/05 agent mapping                             *
;*    1.2       : 07/14/05 use slot-definition-type to create editor *
;*    1.3       : 08/16/05 built-in-class and standard-class :type   *
;*    1.4       : 10/21/05 update-edit-slot-list                     *
;*    2.0       : 10/29/09 Clozure CL, less forgiving MOP            *
;*                :initform nil is not compatible with value-editor  *
;*                typed slots, need to use unbound instead           *
;*    2.1       : 12/21/09 factored sequence operations into         *
;*                xml-editor-sequence.lisp                           *
;* Systems      : Intel-Mac, CCL 1.4, OS X 10.6.1                    *
;* Abstract     : XML-Editors can be displayed and edited in         *
;*                xml-editor-windows                                 *
;*                                                                   *
;*********************************************************************

(in-package :xlui)

;****************************************
;  Globals                              *
;****************************************

(defparameter *Spacing* 0.05 "Spacing used for XML editor layout")

;______________________________
; Base Class                   |
;______________________________


(defclass XML-EDITOR (agent-3d)
  ((display-name-p :accessor display-name-p :initform t :initarg :display-name-p :documentation "if non-nil use name as first element of argument-agents")
   (argument-agents :accessor argument-agents :initform nil :documentation "agents representing use editable attributes and sub elements")
   (slot-editors :accessor slot-editors :initform nil :documentation "slot editor instances used to edit value")
   (texture :accessor texture :initform "VAT2action.png" :initarg :texture)
   (widht :accessor width :initform 0.0 :initarg :width)
   (height :accessor height :initform 0.0 :initarg :height)
   (depth :accessor depth :initform 0.0 :initarg :depth))
  (:documentation "XML serializable objects that can be displayed and edited in worlds"))


(defgeneric EDIT-SLOTS (Xml-Editor)
  (:documentation "Use these slots in this order to display object"))


(defgeneric SLOT-EDITOR-CLASSES (xml-editor)
  (:documentation "association list of slot name, editor class name"))


(defgeneric SLOT-EDITOR (xml-editor slot-name)
  (:documentation "return the slot editor for slot <slot-name>"))


(defgeneric (setf SLOT-EDITOR) (editor xml-editor slot-name)
  (:documentation "set the slot editor of slot <slot-name> to <editor>"))


(defgeneric PRINT-NAME (xml-editor)
  (:documentation "return string of name to be used to label xml-editor, default to element name"))


(defgeneric MAKE-VALUE-EDITOR-FROM-SLOT-DEFINITION (Xml-Editor Slot-Definition)
  (:documentation "Make a value editor based on slot-definition. Use value, type, ... to create appropriate editor"))

;______________________________
; Default Implementation       |
;______________________________

(defmethod EDIT-SLOTS ((Self xml-editor))
  ;; default to same slots and order as used for serialization
  (print-slots Self))


(defmethod SLOT-EDITOR ((Self xml-editor) Slot-Name)
  (getf (slot-editors Self) Slot-Name))


(defmethod (setf SLOT-EDITOR) (Editor (Self xml-editor) Slot-Name)
  (setf (getf (slot-editors Self) Slot-Name) Editor))


(defmethod PRINT-NAME ((Self xml-editor))
  (xml-tag-name-string Self))


(defmethod SELECTED-TEXTURE ((Self xml-editor))
  "aqua_red.png")


(defmethod UNSELECTED-TEXTURE ((Self xml-editor))
  "aqua_red.png")


(defmethod COMPUTE-BACKGROUND-COLOR ((Self xml-editor))
  (glcolor4f 1.0 1.0 1.0 (if (is-selected Self) 0.7 (if (is-hovered Self) 0.4 0.05))))


(defmethod DRAW-BACKGROUND ((Self xml-editor))
  (compute-background-color Self)
  (draw-stretched-texture
   Self
   (if (or (is-selected Self) (is-hovered Self))
     (selected-texture Self)
     (unselected-texture Self))
   0.0 0.0
   (width Self) (height Self)))


(defmethod DRAW-INSERTION-ARROWS ((Self xml-editor))
  (glcolor4f 1.0 1.0 1.0 1.0)
  (draw-texture Self "insertion_arrow.png" 
                   (- *Spacing*) (- (height Self) *Spacing*) 0.2 0.2)
  (draw-texture Self "insertion_arrow_right.png"
                   (- (width Self) *Spacing* *Spacing* *Spacing*) (- (height Self) *Spacing*) 0.2 0.2))

  
(defmethod DRAW ((Self xml-editor))
  (when (is-visible Self) 
    (draw-background Self)
    (when (is-drag-entered Self) (draw-insertion-arrows Self)))
  ;; argument agents
  (glcolor4f 1.0 1.0 1.0 1.0)
  (map-argument-agents Self #'draw)
  ;; regular agents
  (dolist (Agent (agents Self))
    (draw Agent)))


(defmethod MOUSE-CLICK-EVENT-HANDLER ((Self xml-editor))
  (call-next-method)
  (when (shift-key-p) (inspect Self)))


(defmethod SLOTS-TO-EDIT-LIST ((Self xml-editor))
  (let ((Slot-Names (edit-slots Self)))
    (if (equal Slot-Names :all)
      (xml::class-slots (class-of Self))
      (mapcar 
       #'(lambda (Slot-Name) 
           (or (find-slot-definition Self Slot-Name)
               (error "print error: Class \"~A\" does not have slot \"~A\"" (type-of Self) Slot-Name)))
       Slot-Names))))


(defmethod MAKE-VALUE-EDITOR-FROM-SLOT-DEFINITION ((Self xml-editor) Slot-Definition)
  (let ((Value-Editor-Class-Name (second (assoc (slot-definition-name Slot-Definition)
                                                (slot-editor-classes Self)))))
    (unless Value-Editor-Class-Name
      (error "no value editor defined for slot \"~A\" of class \"~A\"" (slot-definition-name Slot-Definition) (type-of Self)))
    (let ((Value-Editor (make-instance
                          Value-Editor-Class-Name
                          :part-of Self
                          :name (slot-definition-name Slot-Definition)
                          :view (view Self))))
      (setf (slot-editor Self (slot-definition-name Slot-Definition)) Value-Editor)
       (set-value-from-slot-attribute Value-Editor Self Slot-Definition)
       ;; some value editors remember which slots they represent
       (when (slot-exists-p Value-Editor 'slot-name)
         (setf (slot-value Value-Editor 'slot-name) (slot-definition-name Slot-Definition)))
      Value-Editor)))
    


(defmethod MAKE-ARGUMENT-AGENTS ((Self xml-editor))
  ;; create new agents as necessary
  ;; The name may be used as the first argument agent
  (setf (argument-agents Self) 
        (when (display-name-p Self)
          (list (make-value-editor
                              'static-string-editor
                              (print-name Self)
                              :part-of Self
                              :view (view Self)))))
  ;; scan editable slots
  (setf (argument-agents Self) 
        (append 
         (argument-agents Self)
         (mapcar 
          #'(lambda (Slot-Definition)
              (cond
               ((slot-boundp Self (slot-definition-name Slot-Definition))
                (let ((Value (slot-value Self (slot-definition-name Slot-Definition))))
                  (typecase Value
                    ;; an agent-3d is ready to go
                    (agent-3d Value)
                    ;; a non agent value that needs to be mapped to a value-editor
                    (t  (make-value-editor-from-slot-definition Self Slot-Definition)))))
               ;; slot is unbound, e.g., no init value and attribute not set
               (t
                (make-value-editor-from-slot-definition Self Slot-Definition))))
          (slots-to-edit-list Self)))))


(defmethod UPDATE-EDIT-SLOT-LIST ((Self xml-editor) New-List)
  (declare (ignore New-List))
  ;; default nothing
  )


(defmethod MAP-EDIT-SLOTS-LISTS ((Self xml-editor) Function)
  ;; Call function with all the editable slot values that are lists
  ;; update slots with new values
  ;; update arguments views
  ;; (format t "~%argument agents: ~A~%slots: ~A" (argument-agents Self) (slots-to-edit-list Self))
  (setf (argument-agents Self)
        (mapcar
         #'(lambda (Agent)
             (if (listp Agent)
               (let ((New-List (funcall Function Agent)))
                 (update-edit-slot-list Self New-List)
                 New-List)
               Agent))
         (argument-agents Self))))

  
(defmethod MAP-ARGUMENT-AGENTS ((Self xml-editor) Function &rest Args)
  (dolist (Agent (argument-agents Self))
    (if (listp Agent)
      (dolist (Agent Agent)
        (apply Function Agent Args))
      (apply Function Agent Args))))


(defmethod BROADCAST-TO-AGENTS ((Self xml-editor) Function &rest Args)
  (call-next-method)
  ;; also to my argument views: careful: if agent are in both lists they will receive 
  ;; the message twice!!!
  (apply #'map-argument-agents Self #'broadcast-to-agents Function Args))


(defmethod INITIALIZE-INSTANCE :after ((Self xml-editor) &rest Args)
  (declare (ignore Args))
  (make-argument-agents Self)
  (layout Self))  ;; excessive when nested xml-editors

;;______________________________
;; Layout                       |
;;______________________________

(defmethod LAYOUT-HORIZONTAL ((Self xml-editor) Agents X Y &key (Align :bottom) (Recursivep t))
  (declare (ignore Align))
  (let ((X0 x)
        (Height-Max 0)
        (Xmax 0))
    (dolist (Agent Agents)
      (when Recursivep (layout Agent))
      ;; wrap?
      (when (layout-wrap-before-p Agent)
        (setq x x0)
        (incf y (+ Height-Max *Spacing*))
        (setq Height-Max 0))
      ;; position agent
      (setf (x Agent) x)
      (setf (y Agent) y)
      (setf (z Agent) 0.01) ;; avoid deepth culling
      (setq Height-Max (max Height-Max (height Agent)))
      (incf x (+ (width Agent) *Spacing*))
      (setq Xmax (max Xmax x)))
    (values XMax (+ y Height-Max))))


(defmethod LAYOUT-VERTICAL ((Self xml-editor) Agents X Y &key (Align :left) (Recursivep t))
  (declare (ignore Align))
  (dolist (Agent Agents)
    (when Recursivep (layout Agent))
    (setf (x Agent) x)
    (setf (y Agent) y)
    (setf (z Agent) 0.01) ;; avoid depth culling
    (incf y (+ (height Agent) 0.0)))
  (values (+ X (reduce #'max Agents :key #'width :initial-value 0.0)) Y))


(defmethod FLIP-VERTICAL ((Self xml-editor))
  ;; if height is know content needs be flipped vertically because of OpenGL coordinate system
  (let ((Height (height Self)))
    (map-argument-agents 
     Self
     #'(lambda (Agent) (setf (y Agent) (- Height (y Agent) (height Agent)))))))
  

(defmethod LAYOUT ((Self xml-editor))
  (unless (argument-agents Self) (make-argument-agents Self))
  (let* ((X *Spacing*)
         (Y *Spacing*)
         (Xmax X)
         (Horizontal-Agents nil))
    (dolist (Agent (argument-agents Self))
      (cond
       ((listp Agent)
        ;; first horizonal
        (multiple-value-bind (x2 y2)
                             (layout-horizontal Self Horizontal-Agents x y)
          (setq Xmax (max Xmax x2))
          (setq y y2))
        (setf Horizontal-Agents nil)
        ;; vertical
        (setf x *Spacing*) ;; => indent block
        (multiple-value-bind (x2 y2)
                             (layout-vertical Self Agent x y)
          (setq Xmax (max Xmax x2))
          (setq y y2)
          (decf x *Spacing*))) ;; <= outdent block
       (t
        (setf Horizontal-Agents (append Horizontal-Agents (list Agent))))))
    ;; deal with leftover horizontal agents
    (multiple-value-bind (x2 y2)
                         (layout-horizontal Self Horizontal-Agents x y)
      (setq Xmax (max Xmax x2))
      (setq y y2))
    ;; set size
    (setf (width Self) (+ Xmax *Spacing*))
    (setf (height Self) (+ y *Spacing*))
    ;; flip vertical 
    (flip-vertical Self)))
          
 
;______________________________
; Picking                      |
;______________________________

(defmethod FIND-AGENT-BY-REFERENCE-ID ((Self xml-editor) Reference-Id)
  ;; (print Reference-Id)
  (or (call-next-method)
      (block find-agent
        (map-argument-agents 
         Self 
         #'(lambda (Agent) 
             (let ((Result (find-agent-by-reference-id Agent Reference-Id)))
               (when Result (return-from find-agent Result))))))))


#| Examples:

(defclass EXCHANGE-RATE-XML-EDITOR (xml-editor)
  ((rate :accessor rate :initform 3.14 :type float)
   (size :accessor size :initform "hello" :type string)))


(defmethod SLOT-EDITOR-CLASSES ((Self exchange-rate-xml-editor))
  '((rate float-editor) 
    (size string-menu-editor)))


(defmethod EDIT-SLOTS ((Self exchange-rate-xml-editor))
  '(rate size))


(defmethod PRINT-SLOTS ((Self exchange-rate-xml-editor))
  '(rate size))


(rate <exchange-rate-xml-editor/>)

(inspect <exchange-rate-xml-editor rate="4.5"/>)

<exchange-rate-xml-editor rate="4.5"/>



(defmethod DRAW ((Self exchange-rate-xml-editor))
  (layout Self)  ;; excessive
  (print
  (truncate (/ (hemlock::time-to-run   (call-next-method)) 1000  ))))


<application-window>
  <agent-3d-view name="opengl" full-scene-anti-aliasing="false">
   <exchange-rate-xml-editor rate="1000.0" draggable="true"/>
  <exchange-rate-xml-editor y="1.0" draggable="true"/>
  </agent-3d-view>
</application-window>  


|#
  

