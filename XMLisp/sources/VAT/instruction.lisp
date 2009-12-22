;;-*- Mode: Lisp; Package: xlui -*-
;*********************************************************************
;*                                                                   *
;*            I N S T R U C T I O N                                  *
;*                                                                   *
;*********************************************************************
;* Author       : Alexander Repenning, alexander@agentsheets.com     *
;*                http://www.agentsheets.com                         *
;* Copyright    : (c) 1996-2007, AgentSheets Inc.                    *
;* Filename     : Instruction.lisp                                   * 
;* Last Update  : 10/04/07                                           *
;* Version      :                                                    *
;*    1.0       : 12/08/04                                           *
;*    2.0       : 02/07/05 based on xml-editor                       *
;*    2.0.1     : 03/05/05 warn if redefine superclass slot          *
;*    2.1       : 07/21/05 &optional, format keyword,                *
;*                         annotation string                         *
;*    2.2       : 08/09/05 :superclass can be a list                 *
;*    2.3       : 09/02/05 initialize-slot-type-instance             *
;*    2.4       : 10/12/05 check                                     *
;*    2.4.1     : 12/31/05 avoid class name conflict with Allegro    *
;*    2.5       : 03/01/07 "disclosed" attribute, wrapping, camera   *
;*    2.5.1     : 08/30/07 shortcut: double click to test            *
;*    2.5.2     : 10/04/07 use meta information comment as tooltip   *
;*    3.0       : 11/05/09 Clozure CL: work with type checking MOP   *
;* Systems      : Intel-Mac, OS X 10.6.1, CCL 1.4                    *
;* Abstract     : Interface to VAT instructions                      *
;*                                                                   *
;*********************************************************************

(in-package :xlui)


(export '(instruction execute expand))


;*********************************
; Instruction-Meta-Information   *
;*********************************

(defvar *Instructions* (make-hash-table) "table of all the instruction meta information")

(defclass INSTRUCTION-META-INFORMATION ()
  ((&optional-position :accessor &optional-position :initform nil :initarg :&optional-position 
                       :documentation "position after which arguments are optional")
   (format-list :accessor format-list :initform nil :initarg :format-list :documentation "contains keyword or nil for each argument agent")
   (arguments :accessor arguments :initform nil :initarg :arguments 
              :documentation "list of (<name> <type> <editor-class-name> <init-value>) lists")
   (instruction-superlass :accessor instruction-superclass :initform 'instruction :initarg :superclass)
   (instruction-class :accessor instruction-class :initarg :class :documentation "default to instruction name")
   (comments :accessor comments :initform "Instruction" :initarg :comments)
   (macro :accessor macro :initform nil :initarg :macro :documentation "macro converting an instruction into a Lisp executable form")
   (edit-slots :accessor edit-slots :initform nil :initarg :edit-slots :documentation "Name and sequence of slots that can be edited"))
  (:documentation "meta information about an instruction"))


;************************************
; Instruction-Disclosure-Editor     *
;************************************

(defclass INSTRUCTION-DISCLOSURE-EDITOR (disclosure-editor)
  ()
  (:documentation "Show/Hide optional instruction attributes"))


(defmethod EDIT ((Self instruction-disclosure-editor))
  (setf (value Self) (not (value Self)))
  (if (value Self)
    (show-optional-arguments (part-of Self))
    (hide-optional-arguments (part-of Self))))

;********************************
;* Instruction                  *
;********************************

(defclass INSTRUCTION (xml-editor)
  ((disclosure-editor :accessor disclosure-editor :initform nil :documentation "used for &optional attributes")
   (unclosable-argument-agents :accessor unclosable-argument-agents :initform nil))
  (:documentation "A piece of code that can be executed"))


;________________________
; Specification          |
;________________________

(defgeneric EXECUTE (Instruction)
  (:documentation "Execute instruction with or without side effect"))

(defgeneric CHECK (Instruction)
  (:documentation "Check instruction for syntactic and semantic problems (e.g., behavior with multiple methods of identical name. Generate error if there are problems"))

(defgeneric EXPAND (Instruction)
  (:documentation "Return expanded version of instruction"))

(defgeneric NAME (Instruction)
  (:documentation "name of instruction: symbol"))


(defgeneric TEST-WITH-AGENT (Instruction Agent) 
  (:documentation "Test instruction with an agent. Test may result in side effects and deal with return values"))


(defgeneric INSTRUCTION-META-INFORMATION (Instruction)
  (:documentation "Return instuction meta information"))


(defgeneric SHOW-OPTIONAL-ARGUMENTS (Instruction)
  (:documentation "Show all arguments including the optional ones"))


(defgeneric HIDE-OPTIONAL-ARGUMENTS (Instruction)
  (:documentation "Hide optional arguments"))


(defgeneric DISCLOSED-ARGUMENT-AGENTS (Instruction)
  (:documentation "List of all the currently disclosed argument agents."))


(defgeneric ARGUMENT-AGENT-BY-NAME (Instruction Name)
  (:documentation "Find argument agent by argument name"))


(defgeneric VARIANTS (Instruction)
  (:documentation "return a list of instruction variants as XML strings e.g., '\"<see/>\" \"<see-a/>\")"))

(defgeneric NEED-TO-WRAP-HORIZONTALLY (Instruction Agent X)
  (:documentation "return t is adding <agent> at position <x> to <instruction> should be wrapped to next line"))


;________________________
; Initialization         |
;________________________

#| Too eager for now: creating instances of vector arrays without a window can result in crash


(defmethod INITIALIZE-SLOT-TYPE-INSTANCE (Slot-Type-Instance Instruction Slot-Definition)
  ;; This generic implementation should only be called accidentally
  (format t "~%Instruction ~A has created a slot-type-instance ~A for slot ~A" 
          Instruction 
          Slot-Type-Instance
          (slot-definition-name Slot-Definition)))


(defmethod INITIALIZE-SLOT-TYPE-INSTANCE ((Self agent-3d) (Instruction instruction) Slot-Definition)
  (let ((Slot-Name (slot-definition-name Slot-Definition)))
    ;; link value editor to instruction containing it
    (setf (part-of Self) Instruction)
    (setf (name Self) Slot-Name)
    (setf (view Self) (view Instruction))
    ;; set instruction slot value 
    (unless (slot-value Instruction Slot-Name)
      (setf (slot-value Instruction Slot-Name)
            (value Self)))
    ;; some value editors remember which slots they represent
    (when (slot-exists-p Self 'slot-name)
      (setf (slot-value Self 'slot-name) Slot-Name))))



(defmethod INITIALIZE-INSTANCE :after ((Self instruction) &rest Args) 
  (declare (ignore Args))
  (when (instruction-meta-information Self)
    ;; special treatment for standard-class slot types
    (dolist (Argument (arguments (instruction-meta-information Self)))
      ;; if there is a typed slot without <initform>
      (when (and (listp Argument) (= 2 (length Argument)))
        (let ((Slot-Name (first Argument))
              (Type (second Argument)))
          (typecase (find-class Type)
            (standard-class
             ;; don't mess with existing slot values
             (unless (slot-value Self Slot-Name)
               (let ((Slot-Type-Instance (make-instance Type)))
                 (initialize-slot-type-instance Slot-Type-Instance Self (find-slot-definition Self Slot-Name)))))
            (built-in-class
             ;; nothing obvious to do here
             )))))))

|#

;________________________
; default implementation |
;________________________

(defmethod EXECUTE ((Self instruction))
  (eval (expand Self)))


(defmethod CHECK ((Self instruction))
  ;; by default things look well ;-)
  nil)


(defmethod TEST-WITH-AGENT ((Instruction instruction) (Agent agent-3d))
  (progv '(self) (list Agent)
    (execute Instruction))
  ;; animations may have been triggered
  (finish-animations (window Agent))
  (view-draw-contents (view Agent)))


(defmethod MOUSE-CLICK-EVENT-HANDLER ((Self instruction))
  (when (and (double-click-p) (globally-selected-agent-in-world))
    (test-with-agent Self (globally-selected-agent-in-world)))
  (call-next-method))
    

(defmethod NAME ((Self instruction))
  (type-of Self))


(defmethod INSTRUCTION-META-INFORMATION ((Self instruction))
  (gethash (xml-tag-name-symbol Self) *Instructions*))


(defmethod SHOW-OPTIONAL-ARGUMENTS ((Self instruction))
  (layout Self)
  (dolist (Agent (agents (view Self)))
    (layout Agent)))

#| no notion of document view

  (let ((View (view (part-of Self)))
        (Camera (camera (view Self)))
        (Height (height (document-root (window Self)))))
    (dolist (Agent (agents View))
      (layout Agent))
    ;; repostion camera to keep the disclosure at same location
    (decf Height (height (document-root (window Self))))
    (when (vertical-scrollbar (window Self))
      (decf (eye-y Camera) Height)
      (decf (center-y Camera) Height))
    (display View)))  |#


(defmethod HIDE-OPTIONAL-ARGUMENTS ((Self instruction))
  (layout Self)
  (dolist (Agent (agents (view Self)))
    (layout Agent)))

#| no notion of document view
  (let ((View (view (part-of Self)))
        (Camera (camera (view Self)))
        (Height (height (document-root (window Self)))))
    (dolist (Agent (agents View))
      (layout Agent))
    ;; repostion camera to keep the disclosure at same location
    (decf Height (height (document-root (window Self))))
    (when (vertical-scrollbar (window Self))
      (decf (eye-y Camera) Height)
      (decf (center-y Camera) Height))
    (display View)))  |#


(defmethod DISCLOSED-ARGUMENT-AGENTS ((Self instruction))
  (if (disclosure-editor Self)
    (if (value (disclosure-editor Self))
      (argument-agents Self)
      (unclosable-argument-agents Self))
    (argument-agents Self)))


(defmethod ARGUMENT-AGENT-BY-NAME ((Self instruction) Name)
  (nth 
   (+ (or (position Name (arguments (instruction-meta-information Self)) :key #'first)
          (error "no argument called ~A" Name))
      (if (display-name-p Self) 1 0)
      (if (disclosure-editor Self) 1 0))
   (argument-agents Self)))


(defmethod VARIANTS ((Self instruction))
  ;; no default variants
  nil)


(defmethod BACKGROUND-COLOR ((Self instruction))
  ;; some document windows may have instructions as roots
  ;; instructions should not return a color
  nil)


(defmethod NEED-TO-WRAP-HORIZONTALLY ((Instruction instruction) (Agent agent-3d) X)
  ;; usually don't wrap
  nil)


(defmethod PROCESSED-TOOLTIP-TEXT ((Self instruction))
  ;; if there is meta information use that as tooltip
  (let ((Meta-Information (gethash (xml-tag-name-symbol Self) *Instructions*)))
    (when Meta-Information
      (comments Meta-Information))))
          
;_____________________________
; xml-editor extensions       |
;_____________________________

(defmethod MAP-DISCLOSED-ARGUMENT-AGENTS ((Self instruction) Function &rest Args)
  (dolist (Agent (disclosed-argument-agents Self))
    (if (listp Agent)
      (dolist (Agent Agent)
        (apply Function Agent Args))
      (apply Function Agent Args))))


(defmethod BROADCAST-TO-DISCLOSED-AGENTS ((Self instruction) Function &rest Args)
  (call-next-method)
  ;; also to my argument views: careful: if agent are in both lists they will receive 
  ;; the message twice!!!
  (apply #'map-disclosed-argument-agents Self #'broadcast-to-agents Function Args))


(defmethod DISCLOSABLE-ARGUMENTS-POSITION ((Self instruction))
  (when (disclosure-editor Self)
    (+ (&optional-position (instruction-meta-information Self))
       1 ;; disclosure-editor: myself
       (if (display-name-p Self) 1 0))))


(defmethod MAKE-ARGUMENT-AGENTS ((Self instruction))
  ;; instructions have additional features including discloure controls and
  ;; meta information including &optional attributes, text annotations and formating directives
  (unless (instruction-meta-information Self)
    (error "instruction of type ~A does not have meta information" (type-of Self)))
  (setf (argument-agents Self) nil)
  ;; Disclosure
  (when (&optional-position (instruction-meta-information Self))
    (cond 
     ;; if we already have one use it
     ((disclosure-editor Self)
      (setf (part-of (disclosure-editor Self)) Self)
      (setf (view (disclosure-editor Self)) (view Self)))
     ;; make a new one
     (t 
      (setf (disclosure-editor Self) 
            (make-instance 'instruction-disclosure-editor
              :value nil 
              :part-of Self
              :view (view Self)))))
    (setf (argument-agents Self) (append (argument-agents Self) (list (disclosure-editor Self)))))
  ;; Name
  (when (display-name-p Self)
    (setf (argument-agents Self) 
          (append (argument-agents Self)
                  (list (if (variants Self)
                          (make-value-editor
                           'variant-name-editor
                           (xml-tag-name-string Self)
                           :variants (variants Self)
                           :part-of Self
                           :view (view Self))
                          (make-value-editor
                           'static-string-editor
                           (xml-tag-name-string Self)
                           :part-of Self
                           :view (view Self)))))))
  ;; Arguments: create them if necessary and make sure their view is same as ours
  (setf (argument-agents Self) 
        (append 
         (argument-agents Self)
         (mapcar 
          #'(lambda (Argument)
              (etypecase Argument
                ;; static text
                (string 
                 (make-value-editor 'static-string-editor Argument :part-of Self :view (view Self)))
                ;; attribute (name type ...)
                (list 
                 (let* ((Slot-Name (first Argument))
                        (Value (slot-value Self Slot-Name)))
                   (typecase Value
                    ;; an agent-3d is ready to go
                    (agent-3d
                     (unless (view Value) (setf (view Value) (view Self))) ;; agent may be missing it's view
                     Value)
                    ;; nil -> make value editor
                    (null (make-value-editor-from-slot-definition Self (find-slot-definition Self Slot-Name)))
                    ;; a list: assume it's a list of agent-3ds
                    (list 
                     (dolist (Agent Value) 
                       (unless (view Agent) (setf (view Agent) (view Self))))
                     Value)
                    ;; a non agent value that needs to be mapped to a value-editor
                    (t  (make-value-editor-from-slot-definition Self (find-slot-definition Self Slot-Name))))))))
          (arguments (instruction-meta-information Self)))))
  ;;  unclosable-argument-agents
  (setf (unclosable-argument-agents Self) (subseq (argument-agents Self) 0 (disclosable-arguments-position Self))))



(defmethod LAYOUT-HORIZONTAL ((Self instruction) Agents X Y &key (Align :bottom) (Recursivep t))
  (declare (ignore Align))
  (let ((X0 x)
        (Height-Max 0)
        (Xmax 0))
    (dolist (Agent Agents)
      (when Recursivep (layout Agent))
      ;; wrap according to agent?
      (when (layout-wrap-before-p Agent)
        (setq x x0)
        (incf y (+ Height-Max *Spacing*))
        (setq Height-Max 0))
      ;; wrap because of &optional
      (when (and (disclosable-arguments-position Self) 
                 (eq Agent (nth (disclosable-arguments-position Self) (argument-agents Self))))
        (setq x (+ x0 (width (disclosure-editor Self)) *Spacing* *Spacing*))
        (incf y (+ Height-Max *Spacing*))
        (setq Height-Max 0))
      ;; wrap because of horizontal position
      (when (need-to-wrap-horizontally Self Agent x)
        (setq x (+ x0 (if (disclosure-editor Self) (width (disclosure-editor Self)) 0s0) *Spacing* *Spacing*))
        (incf y (+ Height-Max *Spacing*))
        (setq Height-Max 0))
      ;; wrap before of format list
      (let ((Position (- (position Agent (argument-agents Self))
                         (if (display-name-p Self) 1 0)
                         (if (disclosure-editor Self) 1 0))))
        (when (>= Position 0)
          (case (nth Position (format-list (instruction-meta-information Self)))
            (:return 
             (setq x (+ x0 (if (disclosure-editor Self) (width (disclosure-editor Self)) 0s0) *Spacing* *Spacing*))
             (incf y (+ Height-Max *Spacing*))
             (setq Height-Max 0)))))
      ;; position agent
      (setf (x Agent) x)
      (setf (y Agent) y)
      (setf (z Agent) 0.01s0) ;; avoid deepth culling
      (setq Height-Max (max Height-Max (height Agent)))
      (incf x (+ (width Agent) *Spacing*))
      (setq Xmax (max Xmax x)))
    (values (- XMax *Spacing*) (+ y Height-Max))))


(defmethod LAYOUT ((Self instruction))
  ;; like XML-EDITOR but loop over disclosed-argument-agents
  (unless (argument-agents Self) (make-argument-agents Self))
  (let* ((X *Spacing*)
         (Y *Spacing*)
         (Xmax X)
         (Horizontal-Agents nil))
    (dolist (Agent (disclosed-argument-agents Self))
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


(defmethod DRAW ((Self instruction))
  (when (is-visible Self) 
    (draw-background Self)
    (when (is-drag-entered Self) (draw-insertion-arrows Self)))
  ;; argument agents
  (glcolor4f 1s0 1s0 1s0 1s0)
  (map-disclosed-argument-agents Self #'draw)
  ;; regular agents
  (dolist (Agent (agents Self))
    (draw Agent)))

;________________________
; Disclosure Management  |
;________________________
;; instead of printing the disclosure editor as subelement treat it as boolean attribute called "disclosed"
;; and only print it when true to cut down overhead


(defmethod SET-ATTRIBUTE-VALUE ((Self instruction) (Name (eql 'disclosed)) Value)
  ;; catch fake attribute "disclosed" and set instruction-disclosure-editor
  (when (string-equal Value "true")
    (cond
     ((disclosure-editor Self) 
      (setf (value (disclosure-editor Self)) t))
     (t
      (setf (disclosure-editor Self) (make-instance 'instruction-disclosure-editor))
      (setf (value (disclosure-editor Self)) t)))))


(defmethod PRINT-SLOTS :around ((Self instruction))
  ;; all instructions could have disclosure
  (cons 'disclosure-editor (call-next-method)))


(defmethod XML-PRINTABLE-AS-ATTRIBUTE-VALUE-P ((Self disclosure-editor)) 
  ;; pretend the disclosure-editor is an attribute
  t)


(defmethod XML-PRINTABLE-AS-SUBELEMENT-P ((Self disclosure-editor)) 
  ;; pretend the disclosure-editor is NOT a sub element
  nil)


(defmethod PRINT-SLOT-NAME-VALUE-TYPE-AS-ATTRIBUTE ((Self instruction) (Name (eql 'disclosure-editor)) Value Type Stream)
  (declare (ignore Type))
  ;; only print when true
  (when (value Value)
    (format Stream " disclosed=\"true\"")))


;________________________
; Instruction macro      |
;________________________

(defun EXTRACT-FORMAT-LIST (Arguments)
  (let ((Keyword nil))
    (mapcan 
     #'(lambda (Item) 
         (typecase Item
           (string
            ;; only extend list if not preceeded by keyword
            (if Keyword
              (progn
                (setq Keyword nil)
                nil)
              (list nil)))
           (list 
            ;; only extend list if not preceeded by keyword
            (if Keyword
              (progn
                (setq Keyword nil)
                nil)
              (list nil)))
           (keyword 
            (setq Keyword Item)
            (list Item))
           (symbol nil)))
     Arguments)))
  

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro INSTRUCTION (Name (&Rest arguments) &body Properties) "
  in: Name symbol; 
  &rest Arguments list of <Name> <Type> [<initform>[<documentation>]];
  &body Properties property-list;
  Create an instruction sub class including expander method and instance of instruction-meta-information
  to manage VAT instructions.
  If there is an <initform> then make-instance will set the slot/attribute value to <initform>.
  If there is NO <initform> and slot type is a buit-in-class then slot/attribute will be set to nil
  If there is NO <initform> and slot type is a standard-class then
  - an instance of slot type will be created with make-instance
  - instance will be initialized with (initialize-slot-type-instance <instance> <instruction> <slot-definition>)
  - slot value will be set to (value <instance>)

  If instruction is drawn or layed out then all argument agents need to be created based on the slot-value:
 1) if the slot-value is an agent-3d or a list of agent-3d then it will be used and no editor will be created. 
 2)  if <type> is a standard-class then an instance will be created of it and used as editor. 
  3) if <type> is built-in-class then no editor is created and slot-value remains unchanged."
  (when (stringp (first Properties)) (push :comments Properties))
  (let ((Superclass (or (getf Properties :superclass) 'instruction))
        (Class (or (getf Properties :class) Name))
        (&Optional-Position (position '&optional (remove-if #'keywordp Arguments)))
        (Macro (getf Properties :macro))
        (Comments (or (getf Properties :comments) "generated by INSTRUCTION macro"))
        (Format-List (extract-format-list Arguments))
        (Edit-Slots (mapcar #'first (remove-if #'atom Arguments))))
    (let ((Arguments (remove-if #'symbolp Arguments)))
      ;; error checking
      ;; if the class already exists but is not a instruction this could be trouble
      (when (and (find-class Class nil) (not (gethash Name *Instructions*))) (error "cannot create instruction ~A because class ~A exists and is not an instruction" Name Class))
      ;; redefining a slot definition could be trouble: warn
      (dolist (Superclass (if (listp Superclass) Superclass (list Superclass)))
        (let ((Slot-Definitions (and (find-class Superclass nil) (class-slots (find-class Superclass)))))
          (dolist (Name Edit-Slots)
            (when (find Name Slot-Definitions :key #'slot-definition-name)
              (warn "redefining slot \"~A\" of superclass \"~A\" in class \"~A\"" Name Superclass Class)))))
      (let ((Meta-Var (gensym)))
        ;; collect meta information
        `(let ((,Meta-Var
                (make-instance 'instruction-meta-information
                  :&optional-position ,&Optional-Position
                  :arguments ',Arguments
                  :format-list ',Format-List
                  :superclass ',Superclass
                  :class ',Class
                  :comments ,Comments
                  :edit-slots ',Edit-Slots
                  :macro ',Macro)))
           ;; keep in table
           (setf (gethash ',Name *Instructions*) ,Meta-Var)
           ;; update element class name table if necessary
           ,@(unless (equal Name Class) `((def-element-class-name ,Name ,Class)))
           ;; generate class
           (prog1
             (defclass ,CLASS (,@(if (listp superclass) superclass (list superclass)))
               ,(mapcar
                 #'(lambda (argument)
                     `(,(first argument) 
                       :accessor ,(first argument)
                       :initform ,(fourth argument)
                       :type ,(second argument)
                       ,@(when (fourth argument) `(:documentation ,(fifth argument)))))
                 (remove-if #'atom Arguments))
               (:documentation ,Comments))
             ;; generate slot-editor-classes definition
             (defmethod SLOT-EDITOR-CLASSES ((Self ,class))
               ',(mapcar
                   #'(lambda (Argument) (list (first Argument) (third Argument)))
                   (remove-if #'atom Arguments)))
             ;; generate expander method
             ;; if there is no macro expand into nil
             (defmethod EXPAND ((,(If Macro (First (First Macro)) 'Self) ,class))
               ,(when Macro (first (rest Macro))))
             ;; generate method to control which slots can be edited
             (defmethod EDIT-SLOTS ((Self ,class))
               ',Edit-Slots)
             ;; same print slots as edit slots
             (defmethod PRINT-SLOTS ((Self ,class))
               ',Edit-Slots)))))))
        )



#| Examples:



(pprint (macroexpand-1 
         '(instruction DOUBLE-ME ((Value float float-editor 3.14))
                       )))


(instruction DOUBLE-ME ((Value float float-editor 3.14) &optional "V2" (Value2 float float-editor 2.7))
  "double the value"
  :macro ((Self) `(* 2 ,(value Self))))


<double-me value="3.0"/>

(inspect <double-me value="3.0"/>)

(defmethod DRAW ((Self double-me))
  (layout Self)  ;; excessive
  (print
  (truncate (/ (hemlock::time-to-run   (call-next-method)) 1000  ))))


<application-window>
  <agent-3d-view name="opengl">
   <double-me value="3.0"/>
   <double-me value="3.0" y="1.0"/>
  </agent-3d-view>
</application-window> 


(defmethod PRINT-SELECTION-ACTION ((w application-window) (Button bevel-button))
  (print (agents-selected (view-named w "opengl"))))


(defmethod EXPAND-SELECTION-ACTION ((w application-window) (Button bevel-button))
  (print (expand (first (agents-selected (view-named w "opengl"))))))


<application-window>
  <column align="stretch" valign="stretch">
  <agent-3d-view name="opengl" vflex="1">
   <double-me value="1.0" draggable="true"/>
   <double-me value="2.0" y="1.0" draggable="true"/>
  </agent-3d-view>
  <row minimize="vertical" align="center">
   <bevel-button text="print" action="print-selection-action" width="55"/>
   <bevel-button text="expand" action="expand-selection-action" width="70"/>
  </row>
  </column>
</application-window>








(instruction FOR ((Var symbol) (From integer) (to integer) (instructions list))
  "Iterate <var> from <from> to <to> and execute <instructions>"
  :macro 
  ((Self) 
   `(let ((,(Var Self) ,(if (> (to Self) (from Self)) (1- (from Self)) (1+ (from Self)))))
      (loop
        ;; count towards end
        ,(if (> (to Self) (from Self))
           `(incf ,(var Self))
           `(decf ,(var Self)))
        ;; run all sub instructions
        ,@(mapcar #'expand (instructions Self))
        ;; done?
        (when (= ,(var Self) ,(to Self)) (return))))))


(defmethod ADD-SUBOBJECT ((Self for) (Instruction instruction))
  (add-object-to-slot Self Instruction 'instructions))



(instruction PRINT ((Value string))
  :macro ((Self) `(print ,(read-from-string (value Self)))))



(expand <print value="3"/>)

(expand
<for var="I" from="1" to="100">
  <print value="I"/>
</for>)


(pprint
(expand
<for var="I" from="-1" to="+1">
  <for var="J" from="-1" to="+1">
    <print value="(+ i j)"/>
  </for>
</for>  ))




(instruction show-slide ("from" (number integer-editor) 
                         "to" (next-number integer-editor) 
                         &optional "use border" (border check-box-editor) 
                         :return "play sound" (play check-box-editor) 
                         :return "sound" (Sound sound-name-editor)) )


(inspect (instruction-meta-information <show-slide number="1" next-number="2"/>))


(defparameter *In* <show-slide number="1" next-number="20" border="false" play="false" sound="click"/>)


(edit *In*)
(inspect *in*)


(value (fourth (argument-agents *in*)))


(edit <show-slide number="1" next-number="20" border="false" play="false" sound="click"/>)

(inspect (gethash 'show-slide *Instructions*))

|#