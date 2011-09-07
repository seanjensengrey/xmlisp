;;-*- Mode: Lisp; Package: XLUI -*-
;*********************************************************************
;*                                                                   *
;*           O P E N   A G E N T   E N G I N E                       *
;*                                                                   *
;*********************************************************************
;* Author       : Alexander Repenning, alexander@agentsheets.com     *
;*                http://www.agentsheets.com                         *
;* Copyright    : (c) 1996-2009, AgentSheets Inc.                    *
;* Filename     : Open-Agent-Engine.lisp                             * 
;* Last Update  : 02/07/07                                           *
;* Version      :                                                    *
;*    1.0       : 11/20/02                                           *
;*    1.1       : 11/01/04 find-agent-at Selection & Picking support *
;*    1.1.1     : 12/29/04 removed glpointsize 9.0 -> speed          *
;*    1.2       : 12/29/04 display-shade                             *
;*    1.2.1     : 03/05/05 less GC: don't rotate if not necessary    *
;*    1.3       : 05/17/05 moved agent methods to opengl-simple-view *
;*    1.4       : 09/23/05 selection-priority                        *
;*    1.4.1     : 02/09/06 :type all slots                           *
;*    1.4.2     : 07/03/06 texture slot for sky-dome                 *
;*    1.4.3     : 08/25/06 type fixes, e.g., double-float 100        *
;*    1.5       : 02/07/07 tooltips support                          *
;*    1.6       : 03/20/07 projected-window-position reference-point *
;*    2.0       : 05/02/09 XLUI: based on game-engine.lisp           *
;*    2.1       : 07/24/09 drag and drop                             *
;*    2.1.1     : 03/03/10 AI: print-slots for tile                  *
;* todos:       : implement withhout using glBegin/End for OpenGL 3x *
;* Systems      : G4, CCL 1.3, OS X 10.5.7                           *
;* Abstract     : engine with agents, animation, rendering, picking  *
;*                scene management, drag and drop.                   *
;*                                                                   *
;*********************************************************************

(in-package :xlui)


(export '(sky-dome texture tooltip-event-handler tooltip-text processed-tooltip-text 
          reference-point projected-window-position))

(defvar *Projected-Agent* nil "agent that is currently being projected to window coordinates")

(defvar *Projected-Agent-Window-Position* nil "window coordinates of agent that is currently being projected ")


;*******************************
;* DRAG-AND-PROXY-WINDOW       *
;*******************************

(defclass DRAG-PROXY-WINDOW (transparent-opengl-window)
  ((drag-and-drop-handler :accessor drag-and-drop-handler :initarg :drag-and-drop-handler :documentation "drag and drop handler"))
  (:documentation "window rendering proxy representation of agent to be dragged"))


(defmethod PREPARE-OPENGL ((Self drag-proxy-window))
  ;; no lighting 
  (glClearColor 1.0 1.0 1.0 0.0)
  ;; enablers
  (glDisable GL_LIGHTING)
  (glEnable GL_DEPTH_TEST)
  ;; Textures
  (glEnable GL_TEXTURE_2D)
  ;; Alpha
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  ;; notice we are NOT messing with the camera
  )


(defmethod LUI::DRAW-RECT ((Self drag-proxy-window))
  ;; source view and proxy window share agents -> grab source view lock to avoid
  ;; asynchronous access issues
  (with-glcontext (source-view (drag-and-drop-handler Self))
    (with-glcontext Self
      (clear-background Self)
      (draw Self)
      )))



(defmethod DISPLAY ((Self drag-proxy-window)) 
  ;; source view and proxy window share agents -> grab source view lock to avoid
  ;; asynchronous access issues
  (with-glcontext (source-view (drag-and-drop-handler Self))
    (call-next-method)))


(defmethod DRAW ((Self drag-proxy-window)) 
  (let ((View (source-view (drag-and-drop-handler Self))))
    ;; mask: draw agent dragged only
    (broadcast-to-agents View #'(lambda (Agent) (setf (is-visible Agent) nil)))
    (broadcast-to-agents (source-agent (drag-and-drop-handler Self))  #'(lambda (Agent) (setf (is-visible Agent) t)))
    (draw View)
    ;; unmask: draw all agents again
    (broadcast-to-agents View #'(lambda (Agent) (setf (is-visible Agent) t)))))


;*******************************
;* DRAG-AND-DROP-HANDLER       *
;*******************************

(defclass DRAG-AND-DROP-HANDLER ()
  ((source-agent :accessor source-agent :initarg :source-agent :documentation "the agent being dragged")
   (destination-agent :accessor destination-agent :initform nil :initarg :destination-agent :documentation "the agent potentially receiving a drop")
   (source-view :accessor source-view :initarg :source-view :documentation "the view containing the agent being dragged")
   (drag-proxy-window :accessor drag-proxy-window :initform nil :initarg :drag-proxy-window :documentation "window, typically transparent, rendering potentially animated proxy of agent ")
   (x-start :accessor x-start :initarg :x-start :documentation "window x coordinate for drag start")
   (y-start :accessor y-start :initarg :y-start :documentation "window y coordinate for drag start"))
  (:documentation "Drag and drop handler managing one drag and drop from mouse dragged event to mouse up"))


(defgeneric DRAGGED-TO (drag-and-drop-handler x y)
  (:documentation "called when mouse moved to new screen coordinate x, y in current drag"))


(defgeneric SNAP-BACK (drag-and-drop-handler)
  (:documentation "Deal with drag proxy window in case that drop was rejected"))


(defgeneric CONCLUDE-DRAG (drag-and-drop-handler)
  (:documentation "called then user releases mouse. Drag should be accepted or rejected"))


(defun FIND-AGENT-AT-SCREEN-POSITION (X Y) "
  in:  X, Y int.
  out: Agent agent; View view;
  Return the <agent> and the view containing it at screen coordinates <x,y>."
  (let ((View (find-view-at-screen-position x y)))
    (unless View (return-from find-agent-at-screen-position))
    (when (subtypep (type-of View) 'agent-3d-view)
      (values
       (find-agent-at
        View
        (- x (window-x View) (x (window View)))
        (- y (window-y View) (y (window View)))
        5 5)
       View))))


(defmethod DRAGGED-TO ((Self drag-and-drop-handler) X Y)
  ;; move proxy window
  (when (drag-proxy-window Self)
    (set-position (drag-proxy-window Self) (- x (x-start Self)) (- y (y-start Self))))
  ;; identify agent drop targets 
  (multiple-value-bind (Agent Destination-View)
                       (drop-target (find-agent-at-screen-position x y) (source-agent Self))
    (declare (ignore Destination-View))
    (cond
     ;; drag into new agent
     ((and Agent (not (eq Agent (source-agent Self)))  (not (eq Agent (destination-agent Self))))
       
      (multiple-value-bind (Acceptable Need-To-Copy Explanation)
                           (could-receive-drop Agent (source-agent Self))
        (declare (ignore Explanation))
        ;; found new valid target
        (cond
         ;; looking good
         (Acceptable
          (if Need-To-Copy
            (set-cursor "CopyArrow")
            (set-cursor "ArrowCursor"))
          (mouse-drag-enter-event-handler Agent (source-agent Self)))
         (t ;; not a valid target
          (set-cursor "NotAllowed")))
        (when (destination-agent Self) 
          (mouse-drag-leave-event-handler (destination-agent Self) (source-agent Self)))
        (setf (destination-agent Self) Agent)))
     ;; drag out
     ((and (not Agent) (destination-agent Self))
      (mouse-drag-leave-event-handler (destination-agent Self) (source-agent Self))
      (set-cursor "ArrowCursor")
      (setf (destination-agent Self) nil)))))


(defmethod SNAP-BACK ((Self drag-and-drop-handler))
  (when (drag-proxy-window Self)
    (window-close (drag-proxy-window Self))
    #+(and :ccl :darwin) (hemlock::beep)))


(defmethod CONCLUDE-DRAG ((Self drag-and-drop-handler))
  (cond
   ;; potential drop
   ((and (source-agent Self) (destination-agent Self))
    (setf (is-drag-entered (destination-agent Self)) nil)
    (multiple-value-bind (Acceptable Need-To-Copy Explanation)
                         (could-receive-drop (destination-agent Self) (source-agent Self))
      (declare (ignore Need-To-Copy Explanation))
      (cond
       ;; All is good
       (Acceptable
        (when (drag-proxy-window Self)
          (window-close (drag-proxy-window Self)))
        (receive-drop (destination-agent Self) (source-agent Self)))
       ;; problems
       (t
        (snap-back Self)))))
   (t
    ;; something missing
    (snap-back Self)))
  ;; cleanup
  (set-cursor "ArrowCursor"))

;*******************************
;* AGENT 3D VIEW               *
;*******************************

(defclass AGENT-3D-VIEW (opengl-dialog)
  ((agents :accessor agents :initform nil :initarg :agents :documentation "a scence graph based on agents")
   (is-visible :accessor is-visible :initform t :type boolean)
   (drag-and-drop-handler :accessor drag-and-drop-handler :initform nil :documentation "drag and drop handler")
   (agent-hovered :accessor agent-hovered :initform nil :documentation "the agent currently hovered over")
   (agents-selected :accessor agents-selected :initform nil :documentation "list of agents currently selected")
   (render-mode :accessor render-mode :initform gl_render :documentation "value: gl_render gl_select or gl_feedback")
   (content-changed-action :accessor content-changed-action :initform 'content-changed-default-action :initarg :content-changed-action-action :type symbol :documentation "method by this name will be called on the window containing control and the target of the control when the content of this view changed"))
  (:documentation "View containing agents"))


(defgeneric FIND-AGENT-AT (agent-3d-view X Y Width Height &optional Agent-Type Layer)
  (:documentation "Find agent at view coordinate <x, y> with a tolerance of <Width> <Height>. Used to select and pick agents"))


(defgeneric BROADCAST-TO-AGENTS (agent-3d-view Function &rest Args)
  (:documentation "Apply <Function> with <Args> first to me and then to all my sub agents"))


(defgeneric CONTENT-CHANGED (agent-3d-view)
  (:documentation "Content changed, some agents must have been edited"))


(defgeneric AGENT-SELECTED (agent-3d-view agent)
  (:documentation "Invoke after <agent> has been selected"))


(defgeneric SELECT-AGENTS (agent-3d-view &rest Agents)
  (:documentation "Deselect currently selected agents and then select <Agents>")) 


(defgeneric SELECT-AGENT (agent-3d-view Agent)
  (:documentation "Deselect currently selected agent and then select <Agent>"))


(defgeneric DESELECT-AGENT (agent-3d-view)
  (:documentation "Deselect currently selected agent"))


;________________________________
; implementations                |
;________________________________

(defmethod COMPOSE-SCENE ((Self agent-3d-view))
  (setf (agents Self) nil)
  ;; Sky dome for orientation
  (attach Self (make-instance 'sky-dome :name "The World around me" :pitch -90.0 :view Self)))


(defmethod ATTACH-SKY-DOME ((Self agent-3d-view) texture-path)
  "make and attach a sky-dome with the given texture"
  (remove-sky-dome self)
  (let ((sky-dome (make-instance 'sky-dome :pitch 0.0 :view Self :texture (concatenate 'string (pathname-name texture-path) "." (pathname-type texture-path)) :texture-path (make-pathname :directory (pathname-directory texture-path)))))
    (attach Self sky-dome)
    (display self)))


(defmethod REMOVE-SKY-DOME ((self agent-3d-view))
  (dolist (Agent (agents self))
    (when (equal (Type-of agent) 'xlui::sky-dome)
      (setf (agents Self) (remove  agent (agents Self) :test 'equal) )
      (setf (part-of agent) nil)
      (display self)))) 


(defmethod DRAW ((Self agent-3d-view))
  (dolist (Agent (agents Self))
    (draw Agent)))


(defmethod ANIMATE ((Self agent-3d-view) Time)
  (dolist (Agent (agents Self))
    (animate Agent Time)))


(defmethod PREPARE-OPENGL ((Self agent-3d-view))
  ;; GL setup
  (glClearColor 1.0 1.0 1.0 0.0)
  (glShadeModel GL_SMOOTH)
  (glPixelStorei GL_UNPACK_ALIGNMENT 1)
  ;; define material
  (glMaterialfv GL_FRONT_AND_BACK GL_SPECULAR { 0.5 0.5 0.5 0.0 })
  (glMaterialf GL_FRONT_AND_BACK GL_SHININESS 120.0)
  (glMaterialfv GL_FRONT_AND_BACK GL_AMBIENT_AND_DIFFUSE { 1.0 1.0 1.0 1.0 })
  ;; light
  (glLightfv GL_LIGHT0 GL_POSITION { 30.0 30.0 30.0 1.0 })
  (glLightfv GL_LIGHT0 GL_DIFFUSE { 1.0 1.0 1.0 1.0 })
  (glLightfv GL_LIGHT0 GL_SPECULAR { 1.0 1.0 1.0 1.0 })
  ;; use wood like material
  ;;(glmaterialfv gl_front_and_back gl_ambient_and_diffuse { 0.9 0.6 0.4 1.0 })
  ;; enablers
  (glEnable GL_LIGHTING)
  (glEnable GL_LIGHT0)
  (glEnable GL_DEPTH_TEST)
  (glLineWidth 5.0)
  (glEdgeFlag GL_FALSE)
  (glEnable GL_LINE_SMOOTH)             ;; anti aliased lines
  (glEnable GL_POINT_SMOOTH)
  ;; Textures
  (glEnable GL_TEXTURE_2D)
  (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_DECAL)
  ;; Alpha
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  ;; camera
  ;; (aim-camera (camera Self) :eye-z 10.0 :near 1.0)
  ;; compose the scene
  ;;(compose-scene Self)
  )


;; Content Changed events

(defmethod CONTENT-CHANGED ((Self agent-3d-view)) 
  (funcall (content-changed-action Self)  (window Self) Self))


(defmethod CONTENT-CHANGED-DEFAULT-ACTION ((Window window) (View agent-3d-view))
  ;; (format t "~%Content of view: ~A in window of type: ~A changed" View (type-of Window))
  )


;; accessing Agents

(defmethod BROADCAST-TO-AGENTS ((Self agent-3d-view) Function &rest Args)
  ;; me
  ;; (apply Function Self Args)
  ;; my agents
  (dolist (Agent (agents Self))
    (apply #'broadcast-to-agents Agent Function Args)))

;;; Selection & Picking ;;;;;;;;;;


(defconstant Bufsize 512 "OpenGL selection array size")


(defun NOT-IN-RENDER-MODE-ERROR-CHECK (&optional (Message ""))
  ;; dealing with the hit record buffer in the wrong render mode is a fatal problem
  ;;;(unless (get-current-opengl-simple-view)
  ;;;  (error "check for selection mode without valid context"))
  (ccl::rlet ((&mode :long))
    (glGetIntegerv GL_RENDER_MODE &mode)
    (when (= (ccl::%get-long &mode) GL_SELECT) (error "~A OpenGL is already in select mode" Message))))


(defun GET-SELECTION-ARRAY-HIT-RECORDS (Hit-Number Selection-Array) "
  in:   Hit-Number int, Selection-Array array of int.
  out: Hit-Records list of (<name-int-list> z-min z-maz).
  Get all the hit records from the selection array."
  ;; walk through the hit record buffer one int at a time
  ;; this is a VERY low level function: rewrite with memory.lisp functions
  (let ((Ptr (ccl::%inc-ptr Selection-Array 0)) ;; make copy of pointer to array
        (Hit-Records nil))
    (dotimes (I Hit-Number Hit-Records)
      (let ((Names (ccl::%get-long Ptr))
            Z-Min Z-Max)
        (ccl::%incf-ptr Ptr 4)
        (setq Z-Min (ccl::%get-long Ptr))
        (ccl::%incf-ptr Ptr 4)
        (setq Z-Max (ccl::%get-long Ptr))
        (ccl::%incf-ptr Ptr 4)
        (let ((Name-Int-List nil))
          (dotimes (Name Names)
            (push (ccl::%get-long Ptr) Name-Int-List)
            (ccl::%incf-ptr Ptr 4))
          (push (list Name-Int-List Z-Min Z-Max) Hit-Records))))))


(defmethod FIND-AGENT-BY-REFERENCE-ID ((Self agent-3d-view) Reference-Id)
  (dolist (Agent (agents Self))
    (let ((Result (find-agent-by-reference-id Agent Reference-Id)))
      (when Result (return Result)))))


(defmethod FIND-CLOSEST-AGENT-IN-HIT-RECORD ((Self agent-3d-view) Hit-Number Selection-Array &optional Agent-Type Layer)
  (let ((Closest-Agent nil)
        (Z-Min nil)
        (Priority-Max nil))
    ;; look at all hit records
    (dolist (Hit-Record (get-selection-array-hit-records Hit-Number Selection-Array))
      ;; indentify the agent that is closest
      (dolist (Agent (mapcar #'(lambda (Name) (find-agent-by-reference-id Self Name)) (first Hit-Record)))
        (declare (ftype function layer))
        (when (and Agent
                   (or (null Agent-Type)
                       (and Agent-Type (subtypep (type-of Agent) Agent-Type)))
                   (or (null Layer)
                       (equal layer (layer Agent)))
                   (or (null Priority-Max) (> (selection-priority Agent) Priority-Max)
                       (null Closest-Agent) (< (second Hit-Record) Z-Min)))
          ;;(format t "~%~A: ~A" (type-of Agent) (when Agent (selection-priority Agent)))
          (setf Closest-Agent agent)
          (setq Z-Min (second Hit-Record))
          (setq Priority-Max (selection-priority Agent)))))
    Closest-Agent))


(defmethod FIND-AGENT-AT ((Self agent-3d-view) X Y Width Height &optional Agent-Type Layer)
  ;; GL_SELECT is getting dated: very convenient but no longer working well
  ;; on some hardware. Beware of this. This could manifest itself it large overhead where
  ;; rendering time with GL_SELECT is significanly larger than rendering in GL_RENDER mode
  ;; http://www.opengl.org/discussion_boards/ubbthreads.php?ubb=showflat&Number=235603 
  (unless (agents Self) (return-from find-agent-at))
  (with-glcontext-no-flush Self
    (with-vector-of-size (&Selection-Array (* bufsize (ccl::foreign-size #>GLuint :bytes)))
      (not-in-render-mode-error-check) ;; this should never happen
      (let ((Agent nil))
        (unwind-protect
            (progn
              (glSelectBuffer Bufsize &Selection-Array)
              (glRenderMode GL_SELECT)
              (setf (render-mode Self) GL_SELECT)
              (glInitNames)
              ;; setup camera to zoom into mouse perspective
              (render-for-selection-mode (camera Self) X Y Width Height)
              ;; render agents
              (dolist (Agent (agents Self))
                (draw Agent))
              ;;(glFinish)  ;; flushing causes big overhead
              )
          ;; search buffer, get rid of buffer and switch mode back
          (let ((Hit-Number (glRenderMode GL_RENDER)))
            (when (= Hit-Number -1) (error "Too many objects to select close to the cursor (selection hit record overflow). Please zoom in or rotate your camera so that your cursor does not overlap with so many objects at once."))
            (setq Agent (unless (= Hit-Number 0) (find-closest-agent-in-hit-record Self Hit-Number &Selection-Array Agent-Type Layer))))
          ;; (glFinish) ;; flush causes big overhead ;; make sure this is done before we leave this section
          (setf (render-mode Self) GL_RENDER))
        Agent))))

;; Clicking

(defparameter *Selection-Tolerance* 5 "pixels")


(defmethod SELECT-AGENT ((Self agent-3d-view) Agent)
  (broadcast-to-agents Self #'(lambda (Agent) (deselect Agent)))
  (select Agent)
  ;; for now selection is a sigleton set
  (setf (agents-selected Self) (list Agent))
  (agent-selected Self Agent))


(defmethod DESELECT-AGENT ((Self agent-3d-view))
  (broadcast-to-agents Self #'(lambda (Agent) (deselect Agent)))
  (setf (agents-selected Self) nil))


(defmethod VIEW-LEFT-MOUSE-DOWN-EVENT-HANDLER ((Self agent-3d-view) X Y)
  (let ((Agent (click-target (find-agent-at Self x y *Selection-Tolerance* *Selection-Tolerance*))))
    (cond
     ;; clicked agent
     (Agent
      ;; adjust selection
      (select-agent Self Agent)
      ;; trigger event
      (mouse-click-event-handler Agent)
      ;; setup potential drag and drop
      (when (draggable Agent)
        (setf (drag-and-drop-handler Self)
              (make-instance 'drag-and-drop-handler
                :source-agent Agent
                :source-view Self
                :x-start x
                :y-start y))))
     ;; click into void
     (t (deselect-agent Self)))))


(defmethod VIEW-RIGHT-MOUSE-DOWN-EVENT-HANDLER ((Self agent-3d-view) X Y)
  (declare (ignore x y)))

;; Drag and Drop

(defvar *Drag-Beging-Distance* 3 "how far does mouse need to move after click to indicate drag and drop?")


(defmethod VIEW-LEFT-MOUSE-DRAGGED-EVENT-HANDLER ((Self agent-3d-view) X Y DX DY)
  (declare (ignore DX DY))
  (cond
   ;; dragging in progress
   ((drag-and-drop-handler Self)
    ;; JIT proxy window
    ;; *HACK* This doesn't really work for dragging but at least it doesn't crash when we are dragging in full screen mode, I need to fix this up and handle this differently. 
    (unless  (drag-proxy-window (drag-and-drop-handler Self))
      (when (or (>= (abs (- x (x-start (drag-and-drop-handler Self)))) *Drag-Beging-Distance*)
                (>= (abs (- y (y-start (drag-and-drop-handler Self)))) *Drag-Beging-Distance*))
        (setf (drag-proxy-window (drag-and-drop-handler Self))
              (make-instance 'drag-proxy-window
                :x (+ (window-x Self) (x (window Self)))
                :y (+ (window-y Self) (y (window Self)))
                :width (width Self)
                :height (height Self)
                :use-global-glcontext t
                :drag-and-drop-handler (drag-and-drop-handler Self)))))
    
    (when  (drag-proxy-window (drag-and-drop-handler Self))
      (dragged-to
       (drag-and-drop-handler Self)
       (+ x (window-x Self) (x (window Self)))
       (+ y (window-y Self) (y (window Self))))))
   ;; NOT dragging (probably camera control)
   (t
    (call-next-method))))


(defmethod VIEW-LEFT-MOUSE-UP-EVENT-HANDLER ((Self agent-3d-view) X Y)
  (declare (ignore x y))
  ;; conclude drag and terminate hander
  (when  (drag-and-drop-handler Self)  
    (conclude-drag (drag-and-drop-handler Self))
    (setf (drag-and-drop-handler Self) nil))
  ;(release-conversation-lock)
  ;(release-animation-lock)
  )


;; Hovering
(defmethod VIEW-MOUSE-MOVED-EVENT-HANDLER ((Self agent-3d-view) x y dx dy)
  ;; add some delay here to avoid taxing CPU with high frequency picking
  ;; for instance: picking every 50ms would be plenty fast
  (declare (ignore dx dy))
  (call-next-method)
  ;;(format t "~%hover: x=~A y=~A dx=~A dy=~A" x y dx dy)
  (let ((Agent (find-agent-at Self x y *Selection-Tolerance* *Selection-Tolerance*)))
    (when (or (not (eq Agent (agent-hovered Self))) 
              (and Agent 
                   (find-class 'matrix-background-agent nil)
                   (let ((Matrix-Background-Agent-Type 'matrix-background-agent)) ;; shut up the compiler warnings
                     (subtypep (type-of Agent) Matrix-Background-Agent-Type))))  ;; hack! matrix-background-agent are shared
      (when (and agent (is-visible agent))
        (with-animation-locked
            (when (agent-hovered Self)
              (setf (is-hovered (agent-hovered Self)) nil)
              (mouse-hover-leave-event-handler (agent-hovered Self)))
          (when Agent
            (setf (is-hovered Agent) t)
            (mouse-hover-enter-event-handler Agent))
          (setf (agent-hovered Self) Agent))))))


;****************************************
;  class Agent-3d                       *
;****************************************

(defvar *Reference-Id-Counter* 0 "Agent 3d reference id used for OpenGL selection")


(defclass AGENT-3D (xml-serializer)
  ((view :accessor view :initform nil :initarg :view)
   (name :accessor name :initform "untitled" :initarg :name)
   (part-of :accessor part-of :initform nil :initarg :part-of :documentation "agent containing me")
   (x :accessor x :initform 0.0 :initarg :x :type short-float)
   (y :accessor y :initform 0.0 :initarg :y :type short-float)
   (z :accessor z :initform 0.0 :initarg :z :type short-float)
   (scale-x :accessor scale-x :initform 1.0 :initarg :scale-x :type short-float)
   (scale-y :accessor scale-y :initform 1.0 :initarg :scale-y :type short-float)
   (scale-z :accessor scale-z :initform 1.0 :initarg :scale-z :type short-float)
   (x-turn :accessor x-turn :initform 0.0 :initarg :x-turn :type short-float)
   (y-turn :accessor y-turn :initform 0.0 :initarg :y-turn :type short-float)
   (z-turn :accessor z-turn :initform 0.0 :initarg :z-turn :type short-float)
   (velocity-x :accessor velocity-x :initform 0.0 :initarg :velocity-x :type short-float)
   (velocity-y :accessor velocity-y :initform 0.0 :initarg :velocity-y :type short-float)
   (velocity-z :accessor velocity-z :initform 0.0 :initarg :velocity-z :type short-float)
   (acceleration-x :accessor acceleration-x :initform 0.0 :initarg :acceleration-x :type short-float)
   (acceleration-y :accessor acceleration-y :initform 0.0 :initarg :acceleration-y :type short-float)
   (acceleration-z :accessor acceleration-z :initform 0.0 :initarg :acceleration-z :type short-float)
   (roll :accessor roll :initform 0.0 :initarg :roll :type short-float)
   (pitch :accessor pitch :initform 0.0 :initarg :pitch :type short-float)
   (heading :accessor heading :initform 0.0 :initarg :heading :type short-float)
   (reference-id :accessor reference-id :initform (incf *Reference-Id-Counter*) :type integer)
   (draggable :accessor draggable :initform nil :type boolean :initarg :draggable :documentation "if true agent can be dragged by user")
   (is-visible :accessor is-visible :initform t :type boolean)
   (is-hovered :accessor is-hovered :initform nil :type boolean :documentation "is mouse hovering over me?")
   (is-selected :accessor is-selected :initform nil :type boolean :documentation "am I one of the selected agents?")
   (is-annotated :accessor is-annotated :initform nil :documentation "either nil :true or :false")
   (is-drag-entered :accessor is-drag-entered :initform nil :type boolean :documentation "true if another agent is currently being dragged on my")
   (agents :accessor agents :initform nil :initarg :agents)
   (tooltip :accessor tooltip :initform nil :initarg :tooltip :documentation "If this accessor is set it will display this for the tool instead of the documentation")
   )
  (:documentation "Open Agent Engine agent base class"))

;_______________________________________
; Specification                         |
;_______________________________________

(defgeneric DRAW-SHADE (Agent-3D)
  (:documentation "Draw a shadow, e.g., blurred distorted gray scale texture independent of rotation"))


(defgeneric DRAW-BOUNDING-BOX (agent-3d Red Green Blue &optional Alpha)
  (:documentation "Draw a bounding box containing shape. Used for selection and hovering"))


(defgeneric HAS-SHADE (Agent-3D)
  (:documentation "Return true if agent has and can draw shade"))


(defgeneric BOUNDING-BOX-WIDTH (agent-3d)
  (:documentation "dx of bounding box"))


(defgeneric BOUNDING-BOX-HEIGHT (agent-3d)
  (:documentation "dy of bounding box"))


(defgeneric BOUNDING-BOX-DEPTH (agent-3d)
  (:documentation "dz of bounding box"))


(defgeneric WINDOW (Agent-3D)
  (:documentation "Window containing me"))


(defgeneric SELECTION-PRIORITY (Agent-3D)
  (:documentation "agents with higher selection priority will be selected before others"))

;; tooltips

(defgeneric TOOLTIP-EVENT-HANDLER (Agent-3D)
  (:documentation "invoked when mouse is hovering over agent for some time and help is enabled"))


(defgeneric MOUSECLICK-TRIGGER-EVENT-HANDLER (Agent-3D)
  (:documentation "invoked when mouse is clicked on an agent"))


(defgeneric MOUSEHOVER-TRIGGER-EVENT-HANDLER (Agent-3D)
  (:documentation "invoked when mouse is hovering over agent for some time"))


(defgeneric TOOLTIP-TEXT (Agent-3D)
  (:documentation "String returned will be displayed as tooltip if tooltip is tuned on"))


(defgeneric PROCESSED-TOOLTIP-TEXT (Agent-3D)
  (:documentation "String returned will be displayed as tooltip if tooltip is tuned on. Uses tooltip-text"))


(defgeneric REFERENCE-POINT (Agent-3D)
  (:documentation "Agent center is used as default projection point."))


(defgeneric PROJECTED-WINDOW-POSITION (Agent-3D)
  (:documentation "The agents current reference point position is projected to window coordinates. Used for anotations such as heads up display information."))

;; Mouse

(defgeneric MOUSE-CLICK-EVENT-HANDLER (agent-3d)
  (:documentation "Received a mouse click. Could be used to start drag and drop"))


(defgeneric MOUSE-HOVER-ENTER-EVENT-HANDLER (agent-3d)
  (:documentation "Mouse has entered hover zone above me"))


(defgeneric MOUSE-HOVER-LEAVE-EVENT-HANDLER (agent-3d)
  (:documentation "Mouse has left hover zone above me"))


(defgeneric MOUSE-HAS-BEEN-CLICKED (agent-3d)
  (:documentation "Called after this agent has been clicked on"))

;; Drag and Drop

(defgeneric IS-DRAG-SOURCE (agent-3d)
  (:documentation "return non nil if agent can be used as drag source"))


(defgeneric DRAG-TRACKING (agent-3d)
  (:documentation "Drag proxy representation of me on screen and provide drag and drop feedback as long as mouse is down. Return <x> and <y> of final position"))


(defgeneric CURRENTLY-DRAGGED-TO (Agent X Y)
  (:documentation " in: X Y integer.
  As part of ongoing drag I am currently dragged to screen position <x>, <y>.
  Send proper events."))


(defgeneric COULD-RECEIVE-DROP (Agent1 Agent2)
  (:documentation "in: Agent1, Agent2. 
out: Could: boolean; Need-to-Copy boolean; Explanation string. 
Return true if <Agent2> could be dropped onto <Agent1>. Provide optional explanation string for rejection or aception."))


(defgeneric RECEIVE-DROP (Agent1 Agent2)
  (:documentation "Drop <Agent2> onto <Agent1>"))


(defgeneric CLICK-TARGET (agent)
  (:documentation "The target of a mouse click. In most cases the receiver is the target. In some cases it may be a container of the receiver."))


(defgeneric DROP-TARGET (Target Source)
  (:documentation "What is the actual drop target when dragging source onto target? The default is target but it could be some other object such as perhaps a container of target"))


(defgeneric MOUSE-DRAG-ENTER-EVENT-HANDLER (Agent1 Agent2)
  (:documentation "<Agent2> was dragged onto me"))


(defgeneric MOUSE-DRAG-LEAVE-EVENT-HANDLER (Agent1 Agent2)
  (:documentation "<Agent2> was dragged away from me"))

;; Selection 

(defgeneric SELECT (agent-3d)
  (:documentation "Make me selected"))


(defgeneric DESELECT (agent-3d)
  (:documentation "Deselect me"))

;_______________________________________
; Implementation                        |
;_______________________________________

(defmethod DRAW-SHADE ((Self agent-3d))
  ;; do nothing
  )


(defmethod HAS-SHADE ((Self agent-3d))
  nil)  ;; no shade for me


(defmethod SHADE ((Self agent-3d))
  nil)  ;; could be a string


(defmethod WINDOW ((Self agent-3d))
  (when (view Self)
    (window (view Self))))


(defmethod SELECTION-PRIORITY ((Self agent-3d))
  0)


(defmethod BOUNDING-BOX-WIDTH ((Self agent-3d))
  ;; don't know what shape is: wild guess unit box
  1.0)


(defmethod BOUNDING-BOX-HEIGHT ((Self agent-3d))
  ;; don't know what shape is: wild guess unit box
  1.0)


(defmethod BOUNDING-BOX-DEPTH ((Self agent-3d))
  ;; don't know what shape is: wild guess unit box
  1.0)


(defmethod DRAW :before ((Self agent-3d))
  ;; deal with selection: may have to make sure we have a valid context
  (glPushName (reference-id Self))

  ;; BEGIN display with transformations
  (glPushmatrix)
  (cond
   ;; simple: no rotation
   ((and (not (has-shade Self)) (= (roll Self) 0.0) (= (heading Self) 0.0) (= (pitch Self) 0.0))
    ;; translate to world coordinate
    (glTranslatef (x Self) (y Self) (z Self))
    (glScalef (scale-x Self) (scale-y Self) (scale-z Self)))
   ;; rotation
   (t
    ;; translate to world coordinate
    (glTranslatef (x Self) (y Self) (z Self))
    (glTranslatef (x-turn Self) (y-turn Self) (z-turn Self))
    (draw-shade Self)
    ;; Rotate
    (glRotatef (roll Self) 0.0 0.0 1.0)
    (glRotatef (heading Self) 0.0 1.0 0.0)
    (glRotatef (pitch Self) 1.0 0.0 0.0)
    ;; Scale
    (glScalef (scale-x Self) (scale-y Self) (scale-z Self)) 
    ;; Translate to rotation center GARBAGE!! 24 bytes
    (glTranslatef (- (x-turn Self)) (- (y-turn Self)) (- (z-turn Self)))))
  ;; project agent?
  #| Projection does not work yet in Cocoa
  (when (eq Self *Projected-Agent*)
    (multiple-value-bind (x y z) (reference-point Self)
      (setq *Projected-Agent-Window-Position*
            (multiple-value-list (glu-project x y z))))) |#  )
  

(defmethod DRAW ((Self agent-3d))
  (cond
   ((is-selected Self) (draw-bounding-box Self (first *System-Selection-Color*) (second *System-Selection-Color*) (third *System-Selection-Color*)))
   ((and (is-visible Self) (is-drag-entered Self)) (draw-bounding-box Self 1.0 1.0 0.0))
   ((is-hovered Self) (draw-bounding-box Self 0.5 0.5 0.5)))
  ;; draw all subagents
  (dolist (Agent (agents Self))
    (draw Agent)))


(defmethod DRAW :after ((Self agent-3d))
  ;; Conclude transformations
  (glPopMatrix)
  ;; deal with selection: may have to check if valid context and render mode
  (glPopName))


(defmethod DRAW-BOUNDING-BOX ((Self agent-3d) Red Green Blue &optional (Alpha 1.0))
  (let* ((Dx (bounding-box-width Self))
         (Dy (bounding-box-height Self))
         (Dz (bounding-box-depth Self))
         (S (* 0.07 (min dx dy))))
    (glDisable GL_LIGHTING)
    (glDisable GL_TEXTURE_2D)
    ;; color and width
    (glColor4f Red Green Blue Alpha)
    (glLineWidth 3.0)
    (glBegin GL_LINES)
    (dotimes (I 2)
      (dotimes (J 2)
        (dotimes (K 2)
          (let ((X (if (= i 0) (- s) (+ (* i dx) s)))
                (Y (if (= j 0) (- s) (+ (* j dy) s)))
                (Z (if (= k 0) (- s) (+ (* k dz) s))))
            (glVertex3f x y z)
            (glVertex3f (if (= i 0) s (- dx s)) y z)
            (glVertex3f x y z)
            (glVertex3f x (if (= j 0) s (- dy s)) z)
            (glVertex3f x y z)
            (glVertex3f x y (if (= k 0) s (- dz s)))))))
    (glEnd)
    (glColor3f 1.0 1.0 1.0)
    (glEnable GL_LIGHTING)))


(defmethod ANIMATE ((Self agent-3d) Time)
  ;; animate attached agents
  (dolist (Agent (agents Self))
    (animate Agent Time))
  ;; velocity
  (incf (x Self) (* Time (velocity-x Self)))
  (incf (y Self) (* Time (velocity-y Self)))
  (incf (z Self) (* Time (velocity-z Self)))
  ;; acceleration
  (incf (velocity-x Self) (* Time (acceleration-x Self)))
  (incf (velocity-y Self) (* Time (acceleration-y Self)))
  (incf (velocity-z Self) (* Time (acceleration-z Self))))


(defmethod BROADCAST-TO-AGENTS ((Self agent-3d) Function &rest Args)
  ;; me
  (apply Function Self Args)
  ;; my sub agents
  (dolist (Agent (agents Self))
    (apply #'broadcast-to-agents Agent Function Args)))

;; events

(defmethod KEY-EVENT-HANDLER ((Self agent-3d) Key)
  ;; pass key events to subagents
  (dolist (Agent (agents Self))
    (key-event-handler Agent Key)))


(defmethod MOUSE-CLICK-EVENT-HANDLER ((Self agent-3d))
  ;;(format t "~%clicked: ~A" Self)
  (display (view Self)))


(defmethod MOUSE-HOVER-ENTER-EVENT-HANDLER ((Self agent-3d))
  ;; (format t "~%entering: ~A" Self)
  (display (view Self)))


(defmethod MOUSE-HOVER-LEAVE-EVENT-HANDLER ((Self agent-3d))
  ;; (format t "~%leaving: ~A" Self)
  (display (view Self)))


(defmethod MOUSE-HAS-BEEN-CLICKED ((Self agent-3d))
  ;; do nothing
  )

(defmethod AGENT-SELECTED ((Self agent-3d-view) (Agent agent-3d))
  ;; do notthing
  )


;; picking

(defmethod FIND-AGENT-BY-REFERENCE-ID ((Self agent-3d) Reference-Id)
  (if (eq Reference-Id (reference-id Self))
    Self
    (dolist (Agent (agents Self))
      (let ((Result (find-agent-by-reference-id Agent Reference-Id)))
        (when Result (return Result))))))


;; drag and drop

(defmethod COULD-RECEIVE-DROP ((Agent1 agent-3d) (Agent2 agent-3d))
  (values
   t
   nil
   "generic drag and drop"))


(defmethod MOUSE-DRAG-ENTER-EVENT-HANDLER ((Agent1 agent-3d) (Agent2 agent-3d))
  (setf (is-drag-entered Agent1) t)
  (unless (is-animated (view Agent1)) (display (view Agent1))))


(defmethod MOUSE-DRAG-LEAVE-EVENT-HANDLER ((Agent1 agent-3d) (Agent2 agent-3d))
  (setf (is-drag-entered Agent1) nil)
  (unless (is-animated (view Agent1)) (display (view Agent1))))


(defmethod RECEIVE-DROP ((Agent1 agent-3d) (Agent2 agent-3d))
  ;;; (format t "~%received drop: ~A onto ~A" (type-of Agent2) (type-of Agent1)))
  )


(defmethod CLICK-TARGET ((Self agent-3d))
  Self)


(defmethod CLICK-TARGET ((Self null))
  Self)


(defmethod DROP-TARGET ((Target t) (Source t))
  ;; if in doubt the target is really the target
  Target)


(defmethod DROP-TARGET ((Target null) (Source t))
  ;; no target no error but nil
  nil)


;; tooltips

(defmethod TOOLTIP-EVENT-HANDLER ((Self agent-3d))
  ;; do nothing
  nil)


(defmethod TOOLTIP-TEXT ((Self agent-3d))
  ;; nothing
  nil)


(defmethod PROCESSED-TOOLTIP-TEXT ((Self agent-3d))
  ;; nothing
  nil)


(defmethod MOUSECLICK-TRIGGER-EVENT-HANDLER ((Self agent-3d))
  ;; do nothing
  nil)


(defmethod MOUSEHOVER-TRIGGER-EVENT-HANDLER ((Self agent-3d))
  ;; do nothing
  nil)


(defmethod REFERENCE-POINT ((Self agent-3d))
  (values 
   (x-turn Self)
   (y-turn Self)
   (z-turn Self)))


(defmethod VIEW-RIGHT-MOUSE-DOWN-EVENT-HANDLER ((Self agent-3d) X Y)
  (declare (ignore x y))
  (call-next-method))


(defmethod PROJECTED-WINDOW-POSITION ((Self agent-3d))
  ;; this implementation is not very elegant as it includes all agents to be drawn as side effect.
  (setq *Projected-Agent* Self)
  (unwind-protect
    (progn
      (with-glcontext (view Self)
        (draw (view Self)))
      (values
       (first *Projected-Agent-Window-Position*)
       (second *Projected-Agent-Window-Position*)))
    ;; cleanup: reset 
    (setq *Projected-Agent* nil)))

;; Texture support

(defmethod USE-TEXTURE ((Self agent-3d) Texture)
  (use-texture (view Self) Texture))


(defmethod SELECTED-TEXTURE ((Self agent-3d))
  ;; no default texture
  nil)


(defmethod UNSELECTED-TEXTURE ((Self agent-3d))
  ;; no default texture
  nil)


(defmethod DRAW-TEXTURE ((Self agent-3d) Texture X Y Width Height)
  (glEnable gl_texture_2d) 
  (gltexenvi gl_texture_env gl_texture_env_mode gl_modulate)
  (use-texture (view Self) Texture)
  (glpushmatrix)
  (gltranslatef x y 0.01s0)  ;; sligthly towards the viewer
  (glbegin gl_quads)
  (glnormal3f 0s0 0s0 1s0)
  (gltexcoord2i 0 0) (glVertex2f 0s0 0s0)
  (gltexcoord2i 0 1) (glvertex2f 0s0 Height)
  (gltexcoord2i 1 1) (glvertex2f Width Height)
  (gltexcoord2i 1 0) (glvertex2f Width 0s0)
  (glend)
  (glPopMatrix))


(defmethod STRETCHED-TEXTURE-DT ((Self agent-3d))
  ;; what ratio of the texture is considered being margin: 0 - 0.5
  0.15s0)


(defmethod STRETCHED-TEXTURE-DV ((Self agent-3d))
  ;; what absolute size of a textured rectangle is considered margin
  0.06s0)


(defmethod DRAW-STRETCHED-TEXTURE ((Self agent-3d) Texture X Y Width Height &optional Dt Dv)
  (setq Dt (or Dt (stretched-texture-dt Self)))
  (setq Dv (or Dv (stretched-texture-dv Self)))
  (glEnable gl_texture_2d) 
  (gltexenvi gl_texture_env gl_texture_env_mode gl_modulate)
  (use-texture (view Self) Texture)
  (glpushmatrix)
  ;;(gltranslatef x y 0.01s0)  ;; sligthly towards the viewer
  (gltranslatef x y 0s0)
  (glnormal3f 0s0 0s0 1s0)
  ;; this is pretty inefficient
  ;; direct mode should be replaced with a vector array
  ;; precompute all the coordinate
  (let ((1-Dt (- 1s0 dt))
        (W-Dv (- Width Dv))
        (H-Dv (- Height dv)))
    (labels ((draw-strip (Yt0 Yt1 Yv0 Yv1)
               (glbegin gl_quad_strip)
                 (glTexCoord2f 0s0 yt0) (glVertex2f 0s0 Yv0)
               (glTexCoord2f 0s0 Yt1) (glVertex2f 0s0 Yv1)
               (glTexCoord2f dt yt0) (glVertex2f dv Yv0)
               (glTexCoord2f dt Yt1) (glVertex2f dv Yv1)
               (glTexCoord2f 1-dt yt0) (glVertex2f w-dv Yv0)
               (glTexCoord2f 1-dt Yt1) (glVertex2f w-dv Yv1)
               (glTexCoord2f 1s0 yt0) (glVertex2f Width Yv0)
               (glTexCoord2f 1s0 Yt1) (glVertex2f Width Yv1)
               (glend)))
      (draw-strip 0s0 dt 0s0 dv)
      (draw-strip dt 1-dt dv h-dv)
      (draw-strip 1-dt 1s0 h-dv Height)))
  (glPopMatrix))

;; clayton: 0.1
;; Andri 0.12, 0.05
;; ronald 0.12, 0.05
;; bob: 0.04


;; Selection

(defmethod SELECT ((Self agent-3d))
  (setf (is-selected Self) t))


(defmethod DESELECT ((Self agent-3d))
  (setf (is-selected Self) nil))


(defmethod FIND-CONTAINER-BY-TYPE ((Self agent-3d) Type)
  (let ((Agent Self))
    (loop
      (when (subtypep (type-of Agent) Type) (return Agent))
      (setq Agent (part-of Agent))
      (unless Agent (return nil)))))

;; Stubb Functions

(defmethod LAYER ((Self agent-3d))
  ;; stub method: agent-3d do not really have layers
  nil)


;______________________________
; Layout                       |
;______________________________

(defmethod LAYOUT ((Self agent-3d))
  ;; do nothing
  )

(defmethod LAYOUT-WRAP-BEFORE-P ((Self agent-3d))
  ;; no wrapping as default
  nil)


;; XML

(defmethod ADD-SUBOBJECT ((View agent-3d-view) (Agent agent-3d))
  (attach View Agent))


(defmethod ADD-SUBOBJECT ((Agent agent-3d) (Sub-Agent agent-3d))
  (attach Agent Sub-Agent))


(defmethod ATTACH ((Self agent-3d) Agent)
  ;; attach subgent to this agent
  (push Agent (agents Self))
  (setf (part-of Agent) Agent)
  (setf (view Agent) (view Self)))


(defmethod ATTACH ((Self agent-3d-view) (Agent agent-3d))
  (setf (agents Self) (append (agents Self) (list Agent)))
  ;; make all sub agents views point to world
  (broadcast-to-agents Agent #'(lambda (Agent) (setf (view Agent) Self)))
  (setf (part-of Agent) Self))


(defmethod ATTACH ((Agent agent-3d) (Sub-Agent agent-3d))
  (setf (agents Agent) (append (agents Agent) (list Sub-Agent)))
  (setf (part-of Sub-Agent) Agent)
  (setf (view Sub-Agent) (view Agent)))



;****************************************
;  Main Agent-3d subclasses             *
;****************************************


;_______________________________________
;  Group                                |
;_______________________________________

(defclass GROUP (agent-3d)
  ()
  (:documentation "A group of agents"))


(defmethod DRAW ((Self group))
  ;; has no visible manifestiation itself, just draw subagents
  (dolist (Agent (agents Self))
    (draw Agent)))



;_______________________________________
;  Text 3D                              |
;_______________________________________

(defclass TEXT-3D (agent-3d)
  ((text :accessor text :initform "untitled" :initarg :text :type string :documentation "text associated with control")
   (font :accessor font :initform "geneva" :documentation "font name")
   (size :accessor size :initform 1.0 :type float :documentation "relative size")
   (string-shape :accessor string-shape :initform nil))
  (:documentation "text agent"))


(defmethod INITIALIZE-INSTANCE ((Self text-3d) &rest Args)
  (declare (ignore Args))
  (call-next-method)
  (with-glcontext (shared-opengl-view)
    (setf (string-shape Self) (make-instance 'editable-string-shape 
                                :str (text Self)
                                :font (or (get-font *Font-Manager* (font Self))
                                          (error "missing font: \"~A\"" (font Self)))
                                :size (size Self)))))


(defmethod DRAW ((Self text-3d))
  (unless (is-visible Self) (return-from draw))
  (when (string-shape Self)
    (draw (string-shape Self))))


(defmethod PRINT-SLOTS ((Self text-3d))
  '(text x y size))



;*********************************
;* Agent Camera                  *
;*********************************

(defclass AGENT-CAMERA (xml-serializer camera)
  ()
  (:documentation "Main camera saveable to file"))


(def-element-class-name camera agent-camera)


(defmethod PRINT-DEFAULT-VALUE-ATTRIBUTES-P ((Self agent-camera))
  ;; better be explicit
  t)


(defmethod PRINT-SLOTS ((Self agent-camera))
  '(eye-x eye-y eye-z center-x center-y center-z  up-x up-y up-z fovy aspect near far
    azimuth zenith))  ;; these two should be derived: consider removing and recompute as needed


(defmethod ADD-SUBOBJECT ((View agent-3d-view) (Camera agent-camera)) 
  ;; just replace current camera
  (setf (camera View) Camera))


(defmethod GET-TOOLTIP ((Self agent-3d-view) x y)
  (let ((agent (find-agent-at-screen-position x y)))
    (if agent
      (progn
        (tooltip-event-handler agent)
        (get-tooltip agent x y))
      (tooltip self))))


(defmethod GET-TOOLTIP ((Self agent-3d) x y)
  (declare (ignore x y))
  (if (tooltip-text self)
    (tooltip-text self)
    (or (tooltip self) (documentation (type-of self) 'type))))

#| Examples:


<application-window title="What is this?" margin="0">
  <agent-3d-view>
    <camera eye-z="2.0"/>
    <cube texture="metal1.jpg"/>
    <cylinder top-radius="0.2" texture="metal1.jpg"/>
    <cube x="-1" y="-1" texture="metal1.jpg"/>
  </agent-3d-view>
</application-window>


<application-window title="What is this?" track-mouse="true" margin="0">
  <agent-3d-view>
    <cube z="-1.5" texture="metal1.jpg"/>
    <text-3d text="hello world"/>
    <text-3d heading="-90" text="hello world"/>
    <text-3d y="0.5" text="hello world 2" size="4.0" font="comic sans ms"/>
  </agent-3d-view>
</application-window>



<application-window title="Scene" margin="0">
  <agent-3d-view>
    <cube texture="crate.png"/>
    <sphere x="2.0"/>
    <sphere x="3.0" texture="earth.png"/>
    <cube size="3.0" z="-3.5" texture="crate.png"/>
    <cube x="5" roll="45.0"/>
    <sky-dome pitch="90"/>
  </agent-3d-view>
</application-window>  


|#