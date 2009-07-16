;;-*- Mode: Lisp; Package: XLUI -*-
;*********************************************************************
;*                                                                   *
;*            A G E N T   3 D                                        *
;*                                                                   *
;*********************************************************************
;* Author       : Alexander Repenning, alexander@agentsheets.com     *
;*                http://www.agentsheets.com                         *
;* Copyright    : (c) 1996-2007, AgentSheets Inc.                    *
;* Filename     : game-engine.lisp                                   * 
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
;* Systems      : G4, MCL 5.2, OS X 10.4.9                           *
;* Abstract     : Animation and scene management                     *
;*                                                                   *
;*********************************************************************

(in-package :xlui)

(export '(sky-dome texture tooltip-event-handler tooltip-text processed-tooltip-text 
          reference-point projected-window-position))

(defvar *Projected-Agent* nil "agent that is currently being projected to window coordinates")

(defvar *Projected-Agent-Window-Position* nil "window coordinates of agent that is currently being projected ")


;*******************************
;* AGENT 3D VIEW               *
;*******************************

(defclass AGENT-3D-VIEW (opengl-dialog)
  ((agents :accessor agents :initform nil :initarg :agents :documentation "a scence graph based on agents")
   (is-visible :accessor is-visible :initform t :type boolean)
   (agent-hovered :accessor agent-hovered :initform nil :documentation "the agent currently hovered over")
   (agents-selected :accessor agents-selected :initform nil :documentation "list of agents currenly selected")
   (render-mode :accessor render-mode :initform gl_render :documentation "value: gl_render gl_select or gl_feedback"))
  (:documentation "Contains agents"))


(defgeneric FIND-AGENT-AT (agent-3d-view X Y Width Height &optional Agent-Type)
  (:documentation "Find agent at window coordinate <x, y> with a tolerance of <Width> <Height>. Used to select and pick agents"))


(defgeneric BROADCAST-TO-AGENTS (agent-3d-view Function &rest Args)
  (:documentation "Apply <Function> with <Args> first to me and then to all my sub agents"))

;________________________________
; implementations                |
;________________________________

(defmethod COMPOSE-SCENE ((Self agent-3d-view))
  (setf (agents Self) nil)
  ;; Sky dome for orientation
  (attach Self (make-instance 'sky-dome :name "The World around me" :pitch -90.0 :view Self)))


(defmethod DRAW ((Self agent-3d-view))
  (dolist (Agent (agents Self))
    (draw Agent)))


(defmethod ANIMATE ((Self agent-3d-view) Time)
  (dolist (Agent (agents Self))
    (animate Agent Time)))


(defmethod INIT ((Self agent-3d-view))
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
  (aim-camera (camera Self) :eye-z 10.0 :near 1.0)
  ;; compose the scene
  ;;(compose-scene Self)
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


(defmethod FIND-CLOSEST-AGENT-IN-HIT-RECORD ((Self agent-3d-view) Hit-Number Selection-Array &optional Agent-Type)
  (let ((Closest-Agent nil)
        (Z-Min nil)
        (Priority-Max nil))
    ;; look at all hit records
    (dolist (Hit-Record (get-selection-array-hit-records Hit-Number Selection-Array))
      ;; indentify the agent that is closest
      (dolist (Agent (mapcar #'(lambda (Name) (find-agent-by-reference-id Self Name)) (first Hit-Record)))
        (when (and Agent
                   (or (null Agent-Type)
                       (and Agent-Type (subtypep (type-of Agent) Agent-Type)))
                   (or (null Priority-Max) (> (selection-priority Agent) Priority-Max)
                       (null Closest-Agent) (< (second Hit-Record) Z-Min)))
          ;;(format t "~%~A: ~A" (type-of Agent) (when Agent (selection-priority Agent)))
          (setq Closest-Agent Agent)
          (setq Z-Min (second Hit-Record))
          (setq Priority-Max (selection-priority Agent)))))
    Closest-Agent))


(defmethod FIND-AGENT-AT ((Self agent-3d-view) X Y Width Height &optional Agent-Type)
  (unless (agents Self) (return-from find-agent-at))
  (with-glcontext Self
    (with-vector-of-size (&Selection-Array bufsize)
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
              (glFinish))
          ;; search buffer, get rid of buffer and switch mode back
          (let ((Hit-Number (glRenderMode GL_RENDER)))
            (when (= Hit-Number -1) (error "Selection hit record overflow"))
            (setq Agent (unless (= Hit-Number 0) (find-closest-agent-in-hit-record Self Hit-Number &Selection-Array Agent-Type))))
          (glFinish) ;; make sure this is done before we leave this section
          (setf (render-mode Self) GL_RENDER))
        Agent))))

;; Clicking

(defparameter *Selection-Tolerance* 5 "pixels")


(defmethod VIEW-LEFT-MOUSE-DOWN-EVENT-HANDLER ((Self agent-3d-view) X Y)
  (let ((Agent (find-agent-at Self x y *Selection-Tolerance* *Selection-Tolerance*)))
    (cond
     ;; clicked agent
     (Agent
      ;; adjust selection
      (broadcast-to-agents Self #'(lambda (Agent) (setf (is-selected Agent) nil)))
      (setf (is-selected Agent) t)
      ;; for now selection is a sigleton set
      (setf (agents-selected Self) (list Agent))
      ;; trigger event
      (mouse-click-event-handler Agent))
     ;; click into void
     (t
      (broadcast-to-agents Self #'(lambda (Agent) (setf (is-selected Agent) nil)))
      (setf (agents-selected Self) nil)))))


;; Hovering

(defmethod VIEW-MOUSE-MOVED-EVENT-HANDLER ((Self agent-3d-view) x y dx dy)
  ;; add some delay here to avoid taxing CPU with high frequency picking
  ;; for instance: picking every 50ms would be plenty fast
  (declare (ignore dx dy))
  ;;(format t "~%hover: x=~A y=~A dx=~A dy=~A" x y dx dy)
  (let ((Agent (find-agent-at Self x y *Selection-Tolerance* *Selection-Tolerance*)))
    (unless (equal Agent (agent-hovered Self))
      (when (agent-hovered Self)
        (setf (is-hovered (agent-hovered Self)) nil)
        (mouse-hover-leave-event-handler (agent-hovered Self)))
      (when Agent
        (setf (is-hovered Agent) t)
        (mouse-hover-enter-event-handler Agent))
      (setf (agent-hovered Self) Agent))))

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
   (is-visible :accessor is-visible :initform t :type boolean)
   (is-hovered :accessor is-hovered :initform nil :type boolean :documentation "is mouse hovering over me?")
   (is-selected :accessor is-selected :initform nil :type boolean :documentation "am I one of the selected agents?")
   (is-drag-entered :accessor is-drag-entered :initform nil :type boolean :documentation "true if another agent is currently being dragged on my")
   (agents :accessor agents :initform nil :initarg :agents)))


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

;_______________________________________
; Implementation                        |
;_______________________________________

(defmethod DRAW-SHADE ((Self agent-3d))
  ;; do nothing
  )


(defmethod HAS-SHADE ((Self agent-3d))
  nil)  ;; no shade for me


(defmethod WINDOW ((Self agent-3d))
  (window (or (view Self) (error "agent's view is nil"))))


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
    (when (eq Self *Projected-Agent*)
      (multiple-value-bind (x y z) (reference-point Self)
        (setq *Projected-Agent-Window-Position*
              (multiple-value-list (glu-project x y z))))))


(defmethod DRAW ((Self agent-3d))
  (cond
   ((is-selected Self) (draw-bounding-box Self 1.0 1.0 0.0))
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


(defmethod ATTACH ((Self agent-3d) Agent)
  ;; attach subgent to this agent
  (push Agent (agents Self))
  (setf (part-of Agent) Agent))


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

;; picking

(defmethod FIND-AGENT-BY-REFERENCE-ID ((Self agent-3d) Reference-Id)
  (if (eq Reference-Id (reference-id Self))
    Self
    (dolist (Agent (agents Self))
      (let ((Result (find-agent-by-reference-id Agent Reference-Id)))
        (when Result (return Result))))))

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


(defmethod REFERENCE-POINT ((Self agent-3d))
  (values 
   (x-turn Self)
   (y-turn Self)
   (z-turn Self)))


(defmethod PROJECTED-WINDOW-POSITION ((Self agent-3d))
  ;; this implementation is not very elegant as it includes all agents to be drawn as side effect.
  (setq *Projected-Agent* Self)
  (unwind-protect
    (progn
      (make-me-the-current-context (view Self))
      (draw (view Self))
      (values
       (first *Projected-Agent-Window-Position*)
       (second *Projected-Agent-Window-Position*)))
    ;; cleanup: reset 
    (setq *Projected-Agent* nil)))

;; Texture support

(defmethod USE-TEXTURE ((Self agent-3d) Texture)
  (use-texture (view Self) Texture))

;; XML

(defmethod ADD-SUBOBJECT ((View agent-3d-view) (Agent agent-3d))
  (attach View Agent))


(defmethod ADD-SUBOBJECT ((Agent agent-3d) (Sub-Agent agent-3d))
  (attach Agent Sub-Agent))


(defmethod ATTACH ((Self agent-3d-view) (Agent agent-3d))
  (setf (agents Self) (append (agents Self) (list Agent)))
  ;; make all sub agents views point to world
  (broadcast-to-agents Agent #'(lambda (Agent) (setf (view Agent) Self)))
  (setf (part-of Agent) Self))


(defmethod ATTACH ((Agent agent-3d) (Sub-Agent agent-3d))
  (setf (agents Agent) (append (agents Agent) (list Sub-Agent)))
  (setf (part-of Sub-Agent) Agent))

;_______________________________________
;  Sphere                               |
;_______________________________________

(defclass SPHERE (agent-3d)
  ((size :accessor size :initform 0.5d0 :initarg :size :type double-float :documentation "radius")
   (texture :accessor texture :initform nil :initarg :texture :documentation "texture file name")
   (quadric :accessor quadric :initform nil)))


(defmethod INITIALIZE-QUADRIC ((Self sphere))
  ;; not a good idea to make a quadric if there is no window
  (setf (quadric Self) (gluNewQuadric))
  (gluQuadricDrawstyle (quadric Self) GLU_FILL)
  (gluQuadricOrientation (quadric Self) GLU_OUTSIDE)
  (gluQuadricNormals (quadric Self) GLU_SMOOTH)
  (gluQuadricTexture (quadric Self) GL_TRUE))


(defmethod PRINT-SLOTS ((Self sphere))
  `(x y z roll pitch heading size texture))


(defmethod DRAW ((Self sphere))
  (glEnable GL_LIGHTING)
  (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
  (unless (quadric Self) (initialize-quadric Self))  ;; just in time
  (cond
   ((texture Self) 
    (glEnable GL_TEXTURE_2D)
    (use-texture Self (texture Self)))
   (t 
    (glDisable gl_texture_2d)))
  (gluSphere (quadric Self) (size Self) 20 20)
  (call-next-method))


(defmethod DRAW-BOUNDING-BOX ((Self sphere) Red Green Blue &optional Alpha)
  (glPushMatrix)
  (let ((Offset (float (- (size Self)) 0.0)))
    (glTranslatef Offset Offset Offset))
  (call-next-method)
  (glPopMatrix))

;_______________________________________
;  Sky Dome                             |
;_______________________________________

(defclass SKY-DOME (sphere)
  ()
  (:default-initargs 
    :texture "skyIsland.png")
  (:documentation "A huge sphere containing the world"))


(defmethod INITIALIZE-INSTANCE ((Self sky-dome) &rest Initargs)
  (declare (ignore Initargs))
  ;; sky domes must be very LARGE
  (setf (size Self) 100d0)
  (call-next-method))


(defmethod DRAW ((Self sky-dome))
  (glDisable gl_lighting)
  (glEnable gl_texture_2d)
  (glTexenvi gl_texture_env gl_texture_env_mode gl_decal)
  (unless (quadric Self) (initialize-quadric Self))  ;; just in time
  (gluQuadricOrientation (quadric Self) glu_inside)
  (use-texture (view Self) (texture Self))
  (call-next-method))

;_______________________________________
;  Cube                                 |
;_______________________________________

(defclass CUBE (agent-3d)
  ((size :accessor size :initform 1.0 :type float)
   (texture :accessor texture :initform nil :initarg :texture :documentation "texture file name"))
  (:documentation "Cube agent"))


(defmethod PRINT-SLOTS ((Self cube))
  `(x y z roll pitch heading size texture))


(defmethod BOUNDING-BOX-WIDTH ((Self cube))
  (size Self))


(defmethod BOUNDING-BOX-HEIGHT ((Self cube))
  (size Self))


(defmethod BOUNDING-BOX-DEPTH ((Self cube))
  (size Self))


(defmethod DRAW ((Self cube))
  (call-next-method)
  (cond
   ((texture Self)
    (glenable gl_texture_2d)
    (gltexenvi gl_texture_env gl_texture_env_mode gl_modulate)
    (use-texture Self (texture Self)))
   (t
    (glDisable gl_texture_2d)))
  ;; slow immediate mode to render
  (glbegin gl_quads)
  (let ((s (size Self)))
    ;; front 
    (glnormal3f 0.0 0.0 1.0)
    (gltexcoord2f 0.0 0.0) (glvertex3f 0.0 0.0 s)
    (gltexcoord2f 0.0 1.0) (glvertex3f 0.0  s s)
    (gltexcoord2f 1.0 1.0) (glvertex3f  s  s s)
    (gltexcoord2f 1.0 0.0) (glvertex3f  s 0.0 s)
    ;; back
    (glnormal3f 0.0 0.0 -1.0)
    (gltexcoord2f 0.0 0.0) (glvertex3f 0.0 0.0 0.0)
    (gltexcoord2f 0.0 1.0) (glvertex3f 0.0  s 0.0)
    (gltexcoord2f 1.0 1.0) (glvertex3f  s  s 0.0)
    (gltexcoord2f 1.0 0.0) (glvertex3f  s 0.0 0.0)
    ;; right
    (glnormal3f  1.0 0.0 0.0)
    (gltexcoord2f 0.0 0.0) (glvertex3f s 0.0 0.0)
    (gltexcoord2f 0.0 1.0) (glvertex3f s 0.0  s)
    (gltexcoord2f 1.0 1.0) (glvertex3f s  s  s)
    (gltexcoord2f 1.0 0.0) (glvertex3f s  s 0.0)
    ;; left
    (glnormal3f -1.0 0.0 0.0)
    (gltexcoord2f 0.0 0.0) (glvertex3f 0.0 0.0 0.0)
    (gltexcoord2f 0.0 1.0) (glvertex3f 0.0 0.0  s)
    (gltexcoord2f 1.0 1.0) (glvertex3f 0.0  s  s)
    (gltexcoord2f 1.0 0.0) (glvertex3f 0.0  s 0.0)
    ;; top
    (glnormal3f 0.0 1.0 0.0)
    (gltexcoord2f 0.0 0.0) (glvertex3f 0.0  s 0.0)
    (gltexcoord2f 0.0 1.0) (glvertex3f 0.0  s s)
    (gltexcoord2f 1.0 1.0) (glvertex3f  s  s s)
    (gltexcoord2f 1.0 0.0) (glvertex3f  s  s 0.0)
    ;; bottom
    (glnormal3f 0.0 -1.0 0.0)
    (gltexcoord2f 0.0 0.0) (glvertex3f 0.0 0.0 0.0)
    (gltexcoord2f 0.0 1.0) (glvertex3f 0.0 0.0 s)
    (gltexcoord2f 1.0 1.0) (glvertex3f  s 0.0 s)
    (gltexcoord2f 1.0 0.0) (glvertex3f  s 0.0 0.0)
    (glend)))

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
;  Cylinder                             |
;_______________________________________

(defclass CYLINDER (agent-3d)
  ((base-radius :accessor base-radius :initform 1.0d0 :type double-float :documentation "The radius of the cylinder at z = 0")
   (top-radius :accessor top-radius :initform 0.0d0 :type double-float :documentation "The radius of the cylinder at z = height")
   (height :accessor height :initform 1.0d0 :type double-float :documentation "The height of the cylinder")
   (texture :accessor texture :initform nil :initarg :texture :documentation "texture file name")
   (quadric :accessor quadric :initform nil))
  (:documentation "Cylinder"))


(defmethod PRINT-SLOTS ((Self cylinder))
  `(x y z roll pitch heading base-radius top-radius height texture))


(defmethod BOUNDING-BOX-WIDTH ((Self cylinder))
  (float (* 2 (base-radius Self)) 0.0))


(defmethod BOUNDING-BOX-HEIGHT ((Self cylinder))
  (float (* 2 (base-radius Self)) 0.0))


(defmethod BOUNDING-BOX-DEPTH ((Self cylinder))
  (float (height Self) 0.0))


(defmethod DRAW-BOUNDING-BOX ((Self cylinder) Red Green Blue &optional Alpha)
  (glPushMatrix)
  (let ((Offset (float (- (base-radius Self)) 0.0)))
    (glTranslatef Offset Offset 0.0))
  (call-next-method)
  (glPopMatrix))


(defmethod DRAW ((Self cylinder))
  ;; setup
  (unless (quadric Self) 
    (setf (quadric Self) (gluNewQuadric))
    (gluQuadricOrientation (quadric Self) glu_outside)
    (gluQuadricNormals (quadric Self) glu_smooth)
    (gluQuadricTexture (quadric Self) gl_true)
    (gluQuadricDrawstyle (quadric Self) glu_fill))
  (call-next-method)
  ;; texture
  (cond
   ((texture Self)
    (glenable gl_texture_2d)
    (gltexenvi gl_texture_env gl_texture_env_mode gl_modulate)
    (use-texture Self (texture Self)))
   (t
    (glDisable gl_texture_2d)))
  ;; render cylinder
  (gluCylinder (quadric Self) (base-radius Self) (top-radius Self) (height Self) 40 4))

;_______________________________________
;  Disk                                 |
;_______________________________________

(defclass DISK (agent-3d)
  ((inner-radius :accessor inner-radius :initform 0.0d0 :type double-float :documentation "The inner radius of the disk (defaults to zero)")
   (outer-radius :accessor outer-radius :initform 1.0d0 :type double-float :documentation "The outer radius of the disk")
   (texture :accessor texture :initform nil :initarg :texture :documentation "texture file name")
   (quadric :accessor quadric :initform nil))
  (:documentation "Disk"))


(defmethod PRINT-SLOTS ((Self disk))
  `(x y z roll pitch heading inner-radius outer-radius texture))


(defmethod BOUNDING-BOX-WIDTH ((Self disk))
  (float (* 2 (outer-radius Self)) 0.0))


(defmethod BOUNDING-BOX-HEIGHT ((Self disk))
  (float (* 2 (outer-radius Self)) 0.0))


(defmethod BOUNDING-BOX-DEPTH ((Self disk))
  0.0)


(defmethod DRAW-BOUNDING-BOX ((Self disk) Red Green Blue &optional Alpha)
  (glPushMatrix)
  (let ((Offset (float (- (outer-radius Self)) 0.0)))
    (glTranslatef Offset Offset 0.0))
  (call-next-method)
  (glPopMatrix))


(defmethod DRAW ((Self disk))
  ;; setup
  (unless (quadric Self) 
    (setf (quadric Self) (gluNewQuadric))
    (gluQuadricOrientation (quadric Self) glu_outside)
    (gluQuadricNormals (quadric Self) glu_smooth)
    (gluQuadricTexture (quadric Self) gl_true)
    (gluQuadricDrawstyle (quadric Self) glu_fill))
  (call-next-method)
  ;; texture
  (cond
   ((texture Self)
    (glenable gl_texture_2d)
    (gltexenvi gl_texture_env gl_texture_env_mode gl_modulate)
    (use-texture Self (texture Self)))
   (t
    (glDisable gl_texture_2d)))
  ;; render cylinder
  (gluDisk (quadric Self) (inner-radius Self) (outer-radius Self) 40 2))


;_______________________________________
;  Tile                                 |
;_______________________________________

(defclass TILE (agent-3d)
  ((width :accessor width :initform 1.0 :type float :documentation "width")
   (height :accessor height :initform 1.0 :type float :documentation "height")
   (texture :accessor texture :initform nil :initarg :texture :documentation "texture file name"))
  (:documentation "Tile"))


(defmethod BOUNDING-BOX-WIDTH ((Self tile))
  (width Self))


(defmethod BOUNDING-BOX-HEIGHT ((Self tile))
  (height Self))


(defmethod BOUNDING-BOX-DEPTH ((Self tile))
  0.2)


(defmethod DRAW ((Self tile))
  (call-next-method)
  (cond
   ((texture Self)
    (glenable gl_texture_2d)
    (gltexenvi gl_texture_env gl_texture_env_mode gl_modulate)
    (use-texture Self (texture Self)))
   (t
    (glDisable GL_TEXTURE_2D)))
  ;; slow immediate mode to render
  (glBegin GL_QUADS)
  (glNormal3f 0.0 0.0 1.0)
  (glTexCoord2f 0.0 0.0) (glVertex2f 0.0 0.0)
  (glTexCoord2f 0.0 1.0) (glVertex2f 0.0 (height Self))
  (glTexCoord2f 1.0 1.0) (glVertex2f (width Self) (height Self))
  (glTexCoord2f 1.0 0.0) (glVertex2f (width Self) 0.0)
  (glEnd))


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
  (when (string-shape Self)
    (draw (string-shape Self))))


#| Examples:


<application-window title="What is this?" margin="0">
  <agent-3d-view>
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