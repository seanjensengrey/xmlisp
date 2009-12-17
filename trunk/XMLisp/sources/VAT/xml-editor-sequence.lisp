;;-*- Mode: Lisp; Package: xlui -*-
;*********************************************************************
;*                                                                   *
;*            X M L   E D I T O R    S E Q U E N C E                 *
;*                                                                   *
;*********************************************************************
;* Author       : Alexander Repenning, alexander@agentsheets.com     *
;*                http://www.agentsheets.com                         *
;* Copyright    : (c) 1996-2009, AgentSheets Inc.                    *
;* Filename     : xml-editor-sequence.lisp                           * 
;* Last Update  : 10/29/09                                           *
;* Version      :                                                    *
;*    1.0       : 12/11/09                                           *
;* Systems      : Intel-Mac, CCL 1.4, OS X 10.6.2                    *
;* Abstract     : value-editors that contain sequences of xml-editor *
;*                                                                   *
;*********************************************************************


(in-package :xlui)


(defclass XML-EDITOR-SEQUENCE (value-editor)
  ((width :accessor width :initform 0.2 :initarg :width)
   (height :accessor height :initform 0.2 :initarg :height)
   (slot-name :accessor slot-name :initform nil :initarg :slot-name :documentation "the name of the slot I am representing"))
  (:documentation "A xml-editor sequence editor"))


;; Print

(defmethod PRINT-SLOTS ((Self xml-editor-sequence))
  '(agents))


;; Value

(defmethod VALUE ((Self xml-editor-sequence))
  (agents Self))


(defmethod (SETF VALUE) (Value (Self xml-editor-sequence))
  (setf (agents Self) Value)
  ;; make each editor recognize that it is part of the sequence
  (dolist (Agent (agents Self))
    (setf (part-of Agent) Self)))


(defmethod SET-VALUE-FROM-SLOT-ATTRIBUTE ((Self xml-editor-sequence) (Xml-Editor xml-editor) Slot-Definition)
  ;; must be a list: take as is
  (setf (value Self) (slot-value Xml-Editor (slot-definition-name Slot-Definition))))


;; Display 

(defmethod DISPLAY-STRETCHED-TEXTURE-DT ((Self xml-editor-sequence))
  0.3s0)


(defmethod DISPLAY-STRETCHED-TEXTURE-DV ((Self xml-editor-sequence))
  0.15s0)


(defmethod COMPUTE-BACKGROUND-COLOR ((Self xml-editor-sequence))
  (glcolor4f 0.97 0.95 0.95 1.0))


(defmethod SELECTED-TEXTURE ((Self xml-editor-sequence))
  "whiteBox.png")


(defmethod UNSELECTED-TEXTURE ((Self xml-editor-sequence))
  "whiteBox.png")


(defmethod DRAW-BACKGROUND ((Self xml-editor-sequence))
  (compute-background-color Self)
  (draw-stretched-texture
   Self
   (if (or (is-selected Self) (is-hovered Self))
     (selected-texture Self)
     (unselected-texture Self))
   0.0 0.0
   (width Self) (height Self)))


(defmethod DRAW ((Self xml-editor-sequence)) 
  (when (is-visible Self) 
    (draw-background Self)
    (glcolor4f 1.0 1.0 1.0 1.0))
  (dolist (Agent (agents Self))
    (draw Agent)))


;; layout


(defmethod FLIP-VERTICAL ((Self xml-editor-sequence))
  ;; if height is know content needs be flipped vertically because of OpenGL coordinate system
  (let ((Height (height Self)))
    (dolist (Agent (agents Self))
      (setf (y Agent) (- Height (y Agent) (height Agent))))))



(defmethod LAYOUT ((Self xml-editor-sequence))
  ;; basic vertical, left aligned layout
  (let* ((X *Spacing*)
         (Y *Spacing*)
         (X-max 0.0))
    (dolist (Agent (agents Self))
      (layout Agent)  ;; excessive??
      (setf (x Agent) x)
      (setf (y Agent) y)
      (setf (z Agent) 0.01) ;; prevent dept buffer fight
      (setf X-max (max X-Max (width Agent)))
      (incf Y (height Agent)))
    (setf (width Self) (+ X-max *Spacing* *Spacing*))
    (setf (height Self) (+ Y *Spacing*)))
  (flip-vertical Self))


#| Examples:


(defclass REPEAT (xml-editor)
  ((times :accessor times :initform 10 :type integer)
   (actions :accessor actions :initform nil :type list)))


(defmethod SLOT-EDITOR-CLASSES ((Self repeat))
  '((times integer-editor)
    (actions xml-editor-sequence)))


(defmethod EDIT-SLOTS ((Self repeat))
  '(times actions))


(defmethod PRINT-SLOTS ((Self repeat))
  (edit-slots Self))


(defmethod INITIALIZE-INSTANCE :after ((Self repeat) &rest Args)
  (declare (ignore Args))
  (layout Self))



(defclass ACTION (xml-editor)
  ()
  (:documentation "Action base class"))


(defclass FORWARD (action)
  ((distance :accessor distance :initform 1.0 :type float))
  (:documentation "turtle action to make turtle move forward a certain distance"))


(defmethod COULD-RECEIVE-DROP ((Action1 action) (Action2 action))
  (values 
   t
   "why not"))


(defmethod COULD-RECEIVE-DROP ((Action1 action) (Action2 repeat))
  (values 
   nil
   "nope"))


(defmethod RECEIVE-DROP ((Action1 action) (Action2 action))
  (let ((Sequence (part-of Action1)))
    ;; update value-editor value
    (setf (value Sequence) (move-before (value Sequence) Action2 Action1))
    ;; update slot value
    (setf (slot-value (part-of Sequence) (slot-name Sequence)) (value Sequence))
    (layout Sequence)
    (display (view Action1))))


  (print (part-of (part-of Action1))))

  ;;(setf (actions (part-of Action1)) (move-before (actions (part-of Action1)) Action2 Action1))
 ;;; (print (actions (part-of Action1))))



(defmethod DRAW ((Self forward))
 ;; 
  (call-next-method))


(defmethod SLOT-EDITOR-CLASSES ((Self forward))
  '((distance float-editor)))


(defmethod EDIT-SLOTS ((Self forward))
  '(distance))


(defmethod PRINT-SLOTS ((Self forward))
  (edit-slots Self))


(defmethod INITIALIZE-INSTANCE :after ((Self forward) &rest Args)
  (declare (ignore Args))
  (layout Self))


(defmethod ADD-SUBOBJECT ((Self repeat) (Action action))
  (setf (actions Self) (append (actions Self) (list Action)))
  ;; make all sub agents views point to world
  (broadcast-to-agents Action #'(lambda (Agent) (setf (view Agent) Self)))
  (setf (part-of Action) Self))

(slot-editor <repeat/> 'actions)


(inspect <repeat/>)

(setq wi
<application-window margin="0">
  <agent-3d-view name="opengl">
   <repeat times="12" draggable="true">
     <forward distance="1.0" draggable="true"/>
     <forward distance="2000.0" draggable="false"/>
   </repeat>
   <repeat times="12" draggable="true" y="2.0">
     <forward distance="1.0" draggable="true"/>
     <forward distance="2.0" draggable="true"/>
     <forward distance="3.0" draggable="true"/>
   </repeat>
  </agent-3d-view>
</application-window> 

)

(inspect (first (agents (view-named wi "opengl"))))


<application-window>
  <agent-3d-view name="opengl">
   <forward distance="12.5"/>
  </agent-3d-view>
</application-window> 








|#