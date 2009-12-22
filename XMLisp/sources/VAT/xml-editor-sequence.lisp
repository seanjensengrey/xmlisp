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


(defgeneric INSERT-BEFORE (xml-editor-sequence new-xml-editor before-xml-editor)
  (:documentation "Insert <new-xml-editor> before <before-xml-editor>. Copy <new-xml-editor> if needed"))


(defgeneric INSERT-AFTER (xml-editor-sequence after-xml-editor new-xml-editor)
  (:documentation "Insert <new-xml-editor> after <after-xml-editor>. Copy <new-xml-editor> if needed"))


(defgeneric REMOVE-ITEM (xml-editor-sequence xml-editor)
  (:documentation "Remove <xml-editor> from <xml-editor-sequence>"))

;______________________________
; Printing                     |
;______________________________

(defmethod PRINT-SLOTS ((Self xml-editor-sequence))
  '(agents))


;______________________________
; Value accessors              |
;______________________________

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


;______________________________
; Display                      |
;______________________________

(defmethod STRETCHED-TEXTURE-DT ((Self xml-editor-sequence))
  0.3s0)


(defmethod STRETCHED-TEXTURE-DV ((Self xml-editor-sequence))
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


;______________________________
; layout                       |
;______________________________

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


;______________________________
; List Operations              |
;_______________________________

(defmethod INSERT-BEFORE ((List list) New-Item Before-Item) "
  in: List list; New-Item t; Before-Item t;
  Return a new list with <New-item> preceeding <Before-Item>.
  List may be nested"
 (mapcan 
  #'(lambda (Item)
      (cond 
       ((listp Item) (list (insert-before Item New-Item Before-Item)))
       ((eq Item Before-Item) (list New-Item Before-Item))
       (t (list Item))))
  List))

;; example: (insert-before '(1 2 3 4 5 6 7 8 9) 0 1)
;; example: (insert-before '(1 2 3 4 (5 6 7 8 9)) 8.5 9)


(defmethod INSERT-AFTER ((List list) After-Item New-Item) "
  in: List list; After-Item t; New-Item t;
  Return a new list with <New-item> following <After-Item>.
 List may be a nested list."
 (mapcan 
  #'(lambda (Item)
      (cond
       ((listp Item) (list (insert-after Item After-Item New-Item)))
       ((eq Item After-Item) (list After-Item New-Item))
       (t (list Item))))
  List))

;; (insert-after '(0 1 2 3 4 5 6 7 8 9) 6 6.5)
;; (insert-after '(0 1 2 (3 4 (5 6 7 8)) 9) 6 6.5)


(defmethod MOVE-BEFORE ((List list) Old-Item Before-Item) "
  in: List list; Old-Item t; Before-Item t; 
  Return a new list with <New-item> preceeding <Before-Item>.
 List may be a nested list."
 (mapcan 
  #'(lambda (Item)
      (cond
       ((listp Item) (list (move-before Item Old-Item Before-Item)))
       ((eq Item Before-Item) (list Old-Item Before-Item))
       ((eq Item Old-Item) nil)
       (t (list Item))))
  List))

;; example: (move-before '(1 2 3 4 5 6 7 8 9) 8 1)
;; example: (move-before '((1 2 3 4) 5 (6 7 8) 9) 8 1)
;; example: (move-before '(1 2 3 4 5 6 7 8 9) 1 8)

(defmethod MOVE-AFTER ((List list) After-Item Old-Item) "
  in: List list; After-Item t; Old-Item t; 
  Return a new list with <Old-item> following <after-Item>.
List may be a nested list."
 (mapcan 
  #'(lambda (Item)
      (cond
       ((listp Item) (list (move-after Item After-Item Old-Item)))
       ((eq Item After-Item) (list After-Item Old-Item))
       ((eq Item Old-Item) nil)
       (t (list Item))))
  List))

;; (move-after '(0 1 2 3 4 5 6 7 8 9) 3 0)
;; (move-after '(((0)) 1 (2) (3 4 5) 6 7 8 9) 3 0)



(defmethod INSERT-BEFORE ((Self xml-editor-sequence) (New-Xml-Editor xml-editor) (Before-Xml-Editor xml-editor))
  (multiple-value-bind (Acceptable Need-To-Copy Explanation)
                       (could-receive-drop Before-Xml-Editor New-Xml-Editor)
    (declare (ignore Explanation))
    ;; (format t "drop: accept=~A copy=~A explanation=~A~%" Acceptable Need-To-Copy Explanation)
    (unless Acceptable 
      (format t "drag failled: cannot accept dragging a \"~A\" on a \"~A\"~%" 
              (type-of New-Xml-Editor) (type-of Before-Xml-Editor))
      (return-from insert-before))
    ;; update value editor sequence
    (cond
     ;; copy
     (Need-To-Copy
      (let ((Editor-Copy (duplicate New-Xml-Editor (find-package :xlui))))
        ;; re-establish links to view, xml-editor could come from differnet window/view
        (broadcast-to-agents Editor-Copy #'(lambda (Agent) (setf (view Agent) (view Self))))
        (setf (part-of Editor-Copy) (part-of Self))
        (setf (value Self) (insert-before (value Self) Editor-Copy Before-Xml-Editor))))
     ;; move
     (t
      (setf (value Self) (move-before (value Self) New-Xml-Editor Before-Xml-Editor))))
    ;; update slot value
    (setf (slot-value (part-of Self) (slot-name Self)) (value Self))
    (dolist (Editor (agents (view Self)))
      (layout Editor))
;;    (layout (part-of Self))  ;; need to go to root probably
    (display (view Self))))




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
  '(times actions draggable))


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
   (not (eq (part-of Action1) (part-of Action2)))
   "why not"))


(defmethod COULD-RECEIVE-DROP ((Action1 action) (Action2 repeat))
  (values 
   t
   (not (eq (part-of Action1) (part-of Action2)))
   "nope"))


(defmethod RECEIVE-DROP ((Action1 action) (Action2 action))
  (insert-before (part-of Action1) Action2 Action1))


(defmethod RECEIVE-DROP ((Action1 action) (Repeat repeat))
  (insert-before (part-of Action1) Repeat Action1))


(defmethod DRAW ((Self forward))
 ;; 
  (call-next-method))


(defmethod SLOT-EDITOR-CLASSES ((Self forward))
  '((distance float-editor)))


(defmethod EDIT-SLOTS ((Self forward))
  '(distance))


(defmethod PRINT-SLOTS ((Self forward))
  '(distance draggable))


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

(duplicate  <forward distance="1.0" draggable="true"/>)

(setq wi
<application-window margin="0">
  <agent-3d-view name="opengl">
   <repeat times="12" draggable="true">
     <forward distance="1.0" draggable="true"/>
     <forward distance="2.0" draggable="true"/>
     <forward distance="3.0" draggable="true"/>
   </repeat>
  </agent-3d-view>
</application-window> 

)


(frame-rate (view-named wi "opengl"))


|#