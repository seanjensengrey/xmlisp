;;; LUI: The open source portable Lisp User Interface
;;; Platform specificity: generic
;;; Alexander Repenning and Andri Ioannidou
;;; Version: 0.3 04/10/09
;;;  04/26/09 use logical-pathname
;;;  05/20/09 mouse-moved events
;;;  07/15/09 do not confine mouse events to view content to get dragged events outside of window frame

(in-package :LUI)

;;********************************
;; Event Classes                 *
;;********************************

(defvar *View-Last-Clicked* nil "the view that got clicked last")


(defvar *Current-Event* nil "event")

(defvar *LUI-Event-Types* 
  '(:left-mouse-down 
    :left-mouse-up
    :rignt-mouse-down
    :right-mouse-up
    :other-mouse-down
    :other-mouse-up
    :mouse-moved
    :left-mouse-dragged
    :right-mouse-dragged
    :other-mouse-dragged
    :mouse-entered
    :mouse-exited
    ;; key events
    :key-down
    :key-up)
  "Main LUI event types")


(defclass EVENT () 
  ((event-type :accessor event-type :initarg :event-type :initform :mouse-down)
   (native-event :accessor native-event :initarg :native-event :initform nil :documentation "native event object"))
  (:documentation "LUI crossplatform event"))


(defclass MOUSE-EVENT (event)
  ((x :accessor x :initarg :x :type fixnum :documentation "pixel coordinate, increasing left to right")
   (y :accessor y :initarg :y :type fixnum :documentation "pixel coordinate, increasing top to bottom")
   (dx :accessor dx :initarg :dx :type fixnum :documentation "delta x, >0 for move right")
   (dy :accessor dy :initarg :dy :type fixnum :documentation "delta y, >0 for move down"))
  (:default-initargs 
      :dx 0
    :dy 0)
  (:documentation "LUI mouse crossplatform event"))


(defclass KEY-EVENT (event)
  ((key-code :accessor key-code :initarg :key-code :type fixnum :documentation "hardware-independent code"))
  (:documentation "LUI key crossplatform event"))


(defgeneric NATIVE-TO-LUI-EVENT-TYPE (t)
  (:documentation "return LUI event type"))

;; Modifier Keys

(defgeneric COMMAND-KEY-P ()
  (:documentation "is Command key pressed?"))

(defgeneric SHIFT-KEY-P ()
  (:documentation "is Shift key pressed?"))

(defgeneric ALT-KEY-P ()
  (:documentation "is Alt/Option key pressed?"))

(defgeneric CONTROL-KEY-P ()
  (:documentation "is Control key pressed?"))

;; Special Clicks

(defgeneric DOUBLE-CLICK-P ()
  (:documentation "True if clicked a second time not too long ago"))

;**********************************
;* SUBVIEW-MANAGER-INTERFACE      *
;**********************************

(defclass SUBVIEW-MANAGER-INTERFACE ()
  ()
  (:documentation "Provide access to and process subviews"))


(defgeneric ADD-SUBVIEW (subview-manager-interface subview)
  (:documentation "Add a subview"))

(defgeneric ADD-SUBVIEWS (subview-manager-interface &rest subviews)
  (:documentation "Add subviews. Preserve order for display"))

(defgeneric MAP-SUBVIEWS (subview-manager-interface Function &rest Args)
  (:documentation "Call function in drawing order with each subview"))

(defgeneric RECURSIVE-MAP-SUBVIEWS (subview-manager-interface Function &rest Args)
  (:documentation "Call function in drawing order with each subview and all of its subviews"))

(defgeneric SUBVIEWS (subview-manager-interface)
  (:documentation "Return subviews as list"))

(defgeneric SUPERVIEW (subview-manager-interface)
  (:documentation "Return my superview"))

(defgeneric SWAP-SUBVIEW (subview-manager-interface old-subview new-subview)
  (:documentation "Swap <old-subview> of subview manager with <new-subview>. Prepare <new-subview> by making size and position compatible with <old-subview>"))


(defmethod RECURSIVE-MAP-SUBVIEWS ((Self subview-manager-interface) Function &rest Args)
  (apply Function Self Args)
  (apply #'map-subviews Self #'recursive-map-subviews Function Args))


(defmacro DO-SUBVIEWS ((Subview-Var View) &body Body)
  (let ((View-Var (gensym)))
    `(let ((,View-Var ,View))
       (map-subviews ,View-Var
        #'(lambda (,Subview-Var)
            ,@Body)))))

;**********************************
;* EVENT-LISTENER-INTERFACE       *
;**********************************

(defclass EVENT-LISTENER-INTERFACE ()
  ()
  (:documentation "Receives and handles LUI events"))


(defgeneric VIEW-EVENT-HANDLER (event-listener-interface Event)
  (:documentation "Generic event handler: dispatch event types to methods. 
Call with most important parameters. Make other paramters accessible through *Current-Event*"))


(defgeneric VIEW-LEFT-MOUSE-DOWN-EVENT-HANDLER (event-listener-interface X Y)
  (:documentation "Mouse Click Event handler"))


(defgeneric VIEW-LEFT-MOUSE-DRAGGED-EVENT-HANDLER (event-listener-interface X Y DX DY)
  (:documentation "Mouse dragged event handler"))

;; more mouse events here...

;**********************************
;* VIEW                           *
;**********************************

(defclass VIEW (subview-manager-interface event-listener-interface)
  ((x :accessor x :initform 0 :initarg :x :documentation "relative x-position to container, pixels")
   (y :accessor y :initform 0 :initarg :y :documentation "relative y-position to container, pixels")
   (width :accessor width :initform 170 :initarg :width :documentation "width in pixels")
   (height :accessor height :initform 90 :initarg :height :documentation "height in pixels")
   (part-of :accessor part-of :initform nil :initarg :part-of :documentation "link to container view or window")
   (native-view :accessor native-view :initform nil :documentation "native OS view object"))
  (:documentation "a view, control or window with position and size"))

;;_______________________________
;; Generic Methods               |
;;_______________________________

(defgeneric WINDOW (view-or-window)
  (:documentation "Return the window containing this view"))

(defgeneric SET-FRAME (view &key x y width height)
  (:documentation "set position and size"))

(defgeneric SET-SIZE (view-or-window Width Height)
  (:documentation "Set the size"))

(defgeneric SET-POSITION (view-or-window X Y)
  (:documentation "Set the position"))

(defgeneric WINDOW-X (view)
  (:documentation "x offset in window containing view"))

(defgeneric WINDOW-Y (view)
  (:documentation "y offset in window containing view"))

(defgeneric DISPLAY (view-or-window)
  (:documentation "Make the view draw: prepare view (e.g., locking, focusing), draw, finish up (e.g., unlocking)"))

(defgeneric DRAW (view-or-window)
  (:documentation "Draw view. Assume view is focused. Only issue render commands, e.g., OpenGL glBegin, and no preparation or double buffer flushing"))

(defgeneric LAYOUT (view-or-window)
  (:documentation "Adjust size and potentially position to container, adjust size and position of content if necesary"))

(defgeneric MAKE-NATIVE-OBJECT (view-or-window)
  (:documentation "Make and return a native view object"))

;;_______________________________
;; Default implementation        |
;;_______________________________

(defmethod SET-SIZE ((Self view) Width Height)
  (setf (width Self) Width)
  (setf (height Self) Height))


(defmethod SET-POSITION ((Self view) X Y)
  (setf (x Self) X)
  (setf (y Self) Y))


(defmethod WINDOW-X ((Self view))
  (let ((x 0) (v Self))
    (loop
      (let ((Container (part-of v)))
        (unless Container (return x))
        (incf x (x v))
        (setq v Container)))))


(defmethod WINDOW-Y ((Self view))
  (let ((y 0) (v Self))
    (loop
      (let ((Container (part-of v)))
        (unless Container (return y))
        (incf y (y v))
        (setq v Container)))))


(defmethod INITIALIZE-INSTANCE ((Self view) &rest Args)
  (declare (ignore Args))
  (call-next-method)
  (setf (native-view Self) (make-native-object Self)))


(defmethod DRAW ((Self view))
  ;; nothing
  )


(defmethod LAYOUT ((Self view))
  ;; nothing
  )


(defun FIND-VIEW-AT-SCREEN-POSITION (screen-x screen-y &key Window-Type)
  (let ((Window (find-window-at-screen-position screen-x screen-y :type Window-Type)))
    (when Window
      (find-view-containing-point Window (- Screen-x (x Window)) (- Screen-y (y Window))))))

;; Events

(defmethod MOUSE-EVENT-HANDLER ((Self view) X Y DX DY Event)
  (do-subviews (Subview Self)
    ;; a left mouse down may be the start of a drag, remember the view starting this
    (when (and (member (event-type Event) '(:left-mouse-down))
               (<= (x Subview) x (+ (x Subview) (width Subview)))
               (<= (y Subview) y (+ (y Subview) (height Subview))))
      (setf *View-Last-Clicked* Subview))
    ;; forward event with relative coordinates to ALL subviews overlapping click position
    (mouse-event-handler Subview (- x (x Subview)) (- y (y Subview)) DX DY Event))
  ;; and dispatch event to view
  (case (event-type Event)
    (:left-mouse-down 
     (when (and (<= 0 x (width Self))
                (<= 0 y (height Self)))
       (view-left-mouse-down-event-handler Self x y)))
    (:left-mouse-up (view-left-mouse-up-event-handler Self x y))
    (:left-mouse-dragged
     (when (equal Self *View-Last-Clicked*)
       (view-left-mouse-dragged-event-handler Self x y dx dy)))
    (:mouse-moved 
     (when (and (<= 0 x (width Self))
                (<= 0 y (height Self)))
       (view-mouse-moved-event-handler Self x y dx dy)))
    (t (format t "not handling ~A event yet~%" (event-type Event)))))


(defmethod VIEW-LEFT-MOUSE-DOWN-EVENT-HANDLER ((Self view) X Y)
  (declare (ignore X Y))
  ;; nada
  )


(defmethod VIEW-LEFT-MOUSE-UP-EVENT-HANDLER ((Self view) X Y)
  (declare (ignore X Y))
  ;; nada
  )


(defmethod VIEW-LEFT-MOUSE-DRAGGED-EVENT-HANDLER ((Self view) X Y DX DY)
  (declare (ignore X Y DX DY))
  ;; nada
  )


(defmethod VIEW-MOUSE-MOVED-EVENT-HANDLER ((Self view) x y dx dy)
  (declare (ignore X Y DX DY))
  ;; nothing
  )

(defmethod SIZE-CHANGED-EVENT-HANDLER ((Self view) Width Height)
  (declare (ignore Width Height))
  ;; nothing
  )


;**********************************
;* SCROLL-VIEW                    *
;**********************************

(defclass SCROLL-VIEW (view)
  ((has-horizontal-scroller :accessor has-horizontal-scroller :initform t :type boolean)
   (has-vertical-scroller :accessor has-vertical-scroller :initform t :type boolean))
  (:documentation "A scrollable view containing a view"))


(defmethod LAYOUT ((Self scroll-view))
  (set-size Self (width Self) (height Self)))


;**********************************
;* RECTANGLE-VIEW                 *
;**********************************

(defclass RECTANGLE-VIEW (view)
  ((native-color :accessor native-color :initform nil))
  (:documentation "colored rectangle")
  (:default-initargs 
    :x 10
    :y 10))

(defgeneric SET-COLOR (rectangle-view &key Red Green Blue Alpha)
  (:documentation "set RGBA fill color. Color values [0.0..1.0]. Default RGB to 0.0 and A to 1.0"))

;**********************************
;* WINDOW                         *
;**********************************

(defclass WINDOW (subview-manager-interface event-listener-interface)
  ((title :accessor title :initform "untitled" :initarg :title :documentation "text in title bar")
   (x :accessor x :initform 0 :initarg :x :documentation "screen position, pixels")
   (y :accessor y :initform 0 :initarg :y :documentation "screen position, pixels")
   (width :accessor width :initform 170 :initarg :width :documentation "width in pixels")
   (height :accessor height :initform 90 :initarg :height :documentation "height in pixels")
   (track-mouse :accessor track-mouse :initform nil :type boolean :documentation "If true then window will receive mouse moved events")
   (zoomable :accessor zoomable :initform t :initarg :zoomable :type boolean :documentation "has a control to zoom to largest size needed")
   (minimizable :accessor minimizable :initform t :initarg :minimizable :type boolean :documentation "has control to minimize into dock/taskbar")
   (resizable :accessor resizable :initform t :initarg :resizable :type boolean :documentation "has resize control")
   (closeable :accessor closeable :initform t :initarg :closeable :type boolean :documentation "has close control")
   (borderless :accessor borderless :initform nil :initarg :borderless :type boolean :documentation "has border including title and other decoration")
   (window-needs-saving-p :accessor window-needs-saving-p :initform nil :type boolean :documentation "true if window contains objects that neeed saving")
   (full-screen :accessor full-screen :initform nil :initarg :full-screen :type boolean :documentation "is in full screen mode")
   (do-show-immediately :accessor do-show-immediately :initarg :do-show-immediately :initform t :documentation "if true will show window when creating instance")
   (native-window :accessor native-window :initform nil :documentation "native OS window object")
   (native-view :accessor native-view :initform nil :documentation "native OS view object"))
  (:documentation "a window that can contain views, coordinate system: topleft = 0, 0")
  (:default-initargs 
      :x 100
      :y 100  
      :width 340
      :height 180))

;;_______________________________
;; Generic Methods               |
;;_______________________________

(defgeneric DISPLAY (Window)
  (:documentation "View draws its contents: needs to do all the focusing, locking, etc, necesssary"))

(defgeneric SHOW (Window)
  (:documentation "Make visible on screen"))

(defgeneric HIDE (Window)
  (:documentation "Hide on screen"))

(defgeneric SCREEN-HEIGHT (Window)
  (:documentation "height of the screen that contains window. If window is not shown yet used main screen height"))

(defgeneric SCREEN-WIDTH (Window)
  (:documentation "width of the screen that contains window. If window is not shown yet used main screen width"))

(defgeneric SIZE-CHANGED-EVENT-HANDLER (Window Width Height)
  (:documentation "Size changes through user interaction or programmatically. Could trigger layout of content"))

(defgeneric SHOW-AND-RUN-MODAL (Window)
  (:documentation "show window, only process events for this window, 
after any of the window controls calls stop-modal close window and return value." ))

(defgeneric STOP-MODAL (Window return-value)
  (:documentation "Stop modal mode. Return return-value."))

(defgeneric CANCEL-MODAL (Window)
  (:documentation "Cancel modal mode, close window. Make throw :cancel"))

(defgeneric SWITCH-TO-FULL-SCREEN-MODE (window)
  (:documentation "Window becomes full screen. Menubar and dock are hidden"))

(defgeneric SWITCH-TO-WINDOW-MODE (window)
  (:documentation "Reduce full screen to window. Menubar returns. Dock, if enabled, comes back."))

(defgeneric FIND-VIEW-CONTAINING-POINT (window x y)
  (:documentation "Return the most deeply nested view containing point x, y"))

(defgeneric SUBVIEWS-SWAPPED (window Old-View New-View)
  (:documentation "Called then when subview <Old-View> of window got replaced with <New-View>"))

(defgeneric HAS-BECOME-MAIN-WINDOW (window)
  (:documentation "Called after the window has become the main, i.e., the foremost, window"))

(defgeneric MOUSE-EVENT-HANDLER (Window X Y DX DY Event)
  (:documentation "Invoked on mouse event"))

(defgeneric KEY-EVENT-HANDLER (Window Event)
  (:documentation "Invoked on key event"))

;;_______________________________
;; default implementations       |
;;_______________________________

(defmethod INITIALIZE-INSTANCE ((Self window) &rest Args)
  (declare (ignore Args))
  (call-next-method)
  (setf (native-window Self) (make-native-object Self))
  (when (do-show-immediately Self)
    (show Self)))


(defmethod DISPLAY ((Self Window)) 
  ;; nada
  )


(defmethod DRAW ((Self Window)) 
  ;; nada
  )


(defmethod WINDOW ((Self Window))
  ;; that would be me
  Self)


(defmethod SET-SIZE ((Self Window) Width Height)
  (setf (width Self) Width)
  (setf (height Self) Height)
  (size-changed-event-handler Self Width Height))


(defmethod SET-POSITION ((Self Window) X Y)
  (setf (x Self) X)
  (setf (y Self) Y))


(defmethod FIND-VIEW-CONTAINING-POINT ((Self window) x y)
  (labels ((find-view (View Superview x y)
             (do-subviews (Subview View)
               (when (and (<= (x Subview) x (+ (x Subview) (width Subview)))
                          (<= (y Subview) y (+ (y Subview) (height Subview))))
                 (find-view Subview View (- x (x Subview)) (- y (y Subview)))))
             ;; I contain point but not my subviews
             (return-from FIND-VIEW-CONTAINING-POINT View)))
    (find-view Self nil x y)))


(defmethod PART-OF ((Self window))
  ;; walking the hierarchy up from agent -> view -> window 
  ;; Window is not a part of anything
  nil)


(defmethod SUBVIEWS-SWAPPED ((Self Window) (Old-View view) (New-View view))
  (declare (ignore Old-View New-View))
  ;; do nothing
  )


;; Events

(defmethod MOUSE-EVENT-HANDLER ((Self window) X Y DX DY Event)
  (do-subviews (Subview Self)
    ;; a left mouse down may be the start of a drag, remember the view starting this
    (when (and (member (event-type Event) '(:left-mouse-down))
               (<= (x Subview) x (+ (x Subview) (width Subview)))
               (<= (y Subview) y (+ (y Subview) (height Subview))))
      (setf *View-Last-Clicked* Subview) )
    ;; forward event with relative coordinates to ALL subviews overlapping click position
    (mouse-event-handler Subview (- x (x Subview)) (- y (y Subview)) DX DY Event))
  ;; and dispatch event to window
  (case (event-type Event)
    (:left-mouse-down (view-left-mouse-down-event-handler Self x y))
    (:left-mouse-up (view-left-mouse-up-event-handler Self x y))
    (:left-mouse-dragged
     (when (equal Self *View-Last-Clicked*)
       (view-left-mouse-dragged-event-handler Self x y dx dy)))
    (:mouse-moved (view-mouse-moved-event-handler Self x y dx dy))
    (t (format t "not handling ~A event yet~%" (event-type Event)))))


(defmethod KEY-EVENT-HANDLER ((Self window) Event)
  (format t "~%window key event ~A" (native-event Event)))


(defmethod VIEW-EVENT-HANDLER ((Self Window) Event)
  ;; generic event hander
  (let ((*Current-Event* Event))
    (format t "not handling ~A event yet~%" (event-type Event))))


(defmethod VIEW-EVENT-HANDLER ((Self Window) (Event mouse-event))
  (let ((*Current-Event* Event))
    (with-simple-restart (abandon-view-event-handler "Stop event handling of event ~S of window ~S" Event Self)
      (mouse-event-handler Self (x Event) (y Event) (dx Event) (dy Event) Event))))


(defmethod VIEW-EVENT-HANDLER ((Self Window) (Event key-event))
  (let ((*Current-Event* Event))
    (with-simple-restart (abandon-view-event-handler "Stop event handling of event ~S of window ~S" Event Self)
      (key-event-handler Self Event))))


(defmethod VIEW-LEFT-MOUSE-DOWN-EVENT-HANDLER ((Self window) X Y)
  (declare (ignore X Y))
  ;; nada
  )


(defmethod VIEW-LEFT-MOUSE-UP-EVENT-HANDLER ((Self window) X Y)
  (declare (ignore X Y))
  ;; nada
  )


(defmethod VIEW-LEFT-MOUSE-DRAGGED-EVENT-HANDLER ((Self window) X Y DX DY)
  (declare (ignore X Y DX DY))
  ;; nada
  )


(defmethod VIEW-MOUSE-MOVED-EVENT-HANDLER ((Self window) x y dx dy)
  (declare (ignore X Y DX DY))
  ;(format t "~%mouse moved x=~A y=~A dx=~A dy=~A" x y dx dy)
  ;; nothing
  )


(defmethod SIZE-CHANGED-EVENT-HANDLER ((Self Window) Width Height)
  (declare (ignore Width Height))
  ;; nothing
  )

;; notifications

(defmethod HAS-BECOME-MAIN-WINDOW ((Self window))
  ;;(format t "~%new main window: ~A" Self)
  )


;****************************************************
; CONTROL                                           *
;****************************************************

(defclass CONTROL (view)
  ((target :accessor target :initform nil :initarg :target :documentation "the receiver of a action message when control is clicked. Default to self.")
   (action :accessor action :initform 'control-default-action :initarg :action :type symbol :documentation "method by this name will be called on the window containing control and the target of the control")
   (text :accessor text :initform "untitled" :initarg :text :type string :documentation "text associated with control"))
  (:documentation "LUI Control: when clicked will call the action method of its target, maintains a value"))


(defgeneric INVOKE-ACTION (control)
  (:documentation "invoke to main control action. Called via events or user"))

(defgeneric VALUE (control)
  (:documentation "Return the control value"))

(defgeneric DISABLE (control)
  (:documentation "Disable: control is faded out"))

(defgeneric ENABLE (control)
  (:documentation "Enable: completely visible"))

(defgeneric IS-ENABLED (control)
  (:documentation "true if control is enabled"))

(defgeneric INITIALIZE-EVENT-HANDLING (control)
  (:documentation "setup control that it invoke its action method when clicked"))

;__________________________________
; default implementation            |
;__________________________________/

(defmethod INITIALIZE-INSTANCE ((Self control) &rest Args)
  (declare (ignore Args))
  (call-next-method)
  (unless (target Self) (setf (target Self) Self)) ;; make me default target
  (initialize-event-handling Self))


(defmethod VALUE ((self control))
  (text self))


(defmethod control-default-action ((Window window) (Target Control))
  (format t "~%control default action: window=~A, target=~A" Window Target))


(defmethod control-default-action ((Window null) (Target Control))
  ;; control may not be properly installed in window
  (format t "~%control default action: window=~A, target=~A" Window Target))


(defmethod MAKE-NATIVE-CONTROL ((Self control))
  ;; keep the view
  (native-view Self))


(defmethod INVOKE-ACTION ((Self control))  
  ;;(format t "~%action=~A window=~A target=~A" (action Self) (window Self) (target Self))
  (funcall (action Self) (window Self) (target Self)))


;****************************************************
; Control Library                                   *
;****************************************************
;__________________________________
; Buttons                          |
;__________________________________/


(defclass BUTTON-CONTROL (control)
  ((default-button :accessor default-button :initform nil :type boolean :documentation "if true button is selectable with return key"))
  (:documentation "Button: fixed height")
  (:default-initargs 
    :width 72
    :height 32))


(defclass BEVEL-BUTTON-CONTROL (control)
  ()
  (:documentation "Bevel Button: any height and width")
  (:default-initargs 
    :width 72
    :height 32))


(defclass CHECKBOX-CONTROL (control)
  ((start-checked :accessor start-checked :initform nil :initarg :start-checked )
   (image-on-right :accessor image-on-right :initform nil :initarg :image-on-right))
  (:documentation "Checkbox")
  (:default-initargs 
      :action 'default-action))


(defmethod DEFAULT-ACTION ((window window) (self checkbox-control))
  (declare (ignore window))
  (declare (ignore self)))


(defclass IMAGE-BUTTON-CONTROL (control)
  ((image :accessor image :initform nil :initarg :image :documentation "filename")
   (container :accessor container :initform nil)
   (in-cluster :accessor in-cluster :initform nil)
   (user-action :accessor user-action :initform nil)
   (selected-in-cluster :accessor selected-in-cluster :initform nil)
   (key-equivalent :accessor key-equivalent :initform "")
   )
  (:default-initargs
      :text "")
  (:documentation "Button Image"))


(defmethod DEFAULT-ACTION ((window window) (self image-button-control))
  (declare (ignore window))
  (declare (ignore self)))





(defclass POPUP-BUTTON-CONTROL (control)
  ((actions :initarg :actions :accessor actions  :initform ()
            :documentation "list of actions")
   (container :initarg :container :accessor container  :initform nil
            :documentation "the container of the control"))
  (:default-initargs
      :text ""
    :action 'popup-action)
  (:documentation "Popup Button"))



(defclass POPUP-IMAGE-BUTTON-CONTROL (control)
  ((image :accessor image :initform nil :initarg :image :documentation "filename")
   (popup-menu-cell :accessor popup-menu-cell :initform nil :documentation "popup menu cell for this button")
   (menu :accessor menu :initform nil :documentation "popup menu cell for this button")
   (popup-button :accessor popup-button :documentation "an invisible popup butoton")
   (items :accessor items :initform nil  :documentation " a list of the items of control")
   (disclosure-image :accessor disclosure-image :initform nil )
   (draw-disclosure :accessor draw-disclosure :initform t :type boolean)
   )
  (:default-initargs
      :text ""
    :action 'popup-image-button-action)
  (:documentation "Popup Image Button"))






(defclass POPUP-IMAGE-BUTTON-ITEM-CONTROL (control)
  ((popup-image-button :accessor popup-image-button :initform nil :initarg :popup-image-button :documentation "the window this item is contained inside")
   (enable-predicate :accessor enable-predicate :initarg :enable-predicate :initform 'default-enable-predicate)
   )
  (:default-initargs
      :text ""
    :action 'popup-action)
  (:documentation "Popup Image Button"))


(defclass POPUP-IMAGE-BUTTON-SUBMENU-CONTROL (control)
  ((popup-image-button :accessor popup-image-button :initform nil :initarg :popup-image-button :documentation "the window this item is contained inside")
   (enable-predicate :accessor enable-predicate :initarg :enable-predicate :initform 'default-enable-predicate))
  (:default-initargs
      :text ""
    :action 'popup-action)
  (:documentation "Popup Image Button Submenu"))


(defmethod DEFAULT-ENABLE-PREDICATE ((window window) (self popup-image-button-item-control))
  t)


(defclass RADIO-BUTTON-CONTROL (control)
  ((elements :initarg :elements :accessor elements :initform nil)
   (actions :initarg :actions :accessor actions  :initform ()
            :documentation "list of actions"))
  (:default-initargs
      :text ""
   :action 'radio-action)
  (:documentation "radio-button"))





(defclass CHOICE-BUTTON-CONTROL (control)
  ((actions :initarg :actions :accessor actions  :initform ()
            :documentation "list of actions"))
  (:default-initargs
      :text ""
    :action 'choice-button-action)
  (:documentation "Popup Button"))


(defclass IMAGE-BUTTON-CLUSTER-CONTROL (control)
  ((elements :initarg :elements :accessor elements :initform nil)
   (actions :initarg :actions :accessor actions  :initform ()
            :documentation "list of actions"))
  (:default-initargs
      :text ""
   :action 'image-cluster-action)
  (:documentation "radio-button"))



(defmethod IMAGE-CLUSTER-ACTION ((window window) (self Radio-Button-Control))
  (print "CLUSTER"))


;__________________________________
; String List                      |
;__________________________________/


(defclass STRING-LIST-CONTROL (control)
  ((lui-view :accessor lui-view :initform nil)
   (selected-string :accessor selected-string :initform nil)
   )
  (:default-initargs
      :text ""
    :action 'popup-action)
  (:documentation "Popup Image Button Submenu"))


;__________________________________
; String List View                  |
;__________________________________/


(defclass STRING-LIST-VIEW-CONTROL (control)
  ((list-items :accessor list-items :initform nil :documentation "The list of strings displayed in this list")
   (item-height :accessor  item-height :initform 16 :documentation "The height of an item in the list")
   (selected-string :accessor selected-string :initform nil)
   )
  (:default-initargs
      :text ""
    :action 'popup-action)
  (:documentation "Popup Image Button Submenu"))




;__________________________________
; Scroller                         |
;__________________________________/


(defclass SCROLLER-CONTROL (control)
  ((knob-proportion :accessor knob-proportion :initform .2 :documentation "This value must a float between 0.0 and 1.0 and is used to determine what portion of the scroller the knob takes up.  ")
   (small-scroller-size :accessor small-scroller-size :initform nil :type boolean :documentation "if this accessor is set to true the scroller will be created with the smaller size"))
  (:documentation "Scroller")
  (:default-initargs 
      :action 'scroll-action))


(defmethod SCROLL-ACTION ((window window) (self scroller-control))
  (declare (ignore window))
  (declare (ignore self)))


;__________________________________
; Seperator                        |
;__________________________________/

(defclass SEPERATOR-CONTROL (control)
  ()
  (:documentation "Seperator")
  (:default-initargs 
 
    :width 20
    :height 1))


(defmethod INITIALIZE-INSTANCE :after ((Self seperator-control) &rest Args)
  (declare (ignore Args)))

               
(defmethod initialize-event-handling ((Self seperator-control))
  ;; not clickable
  )


;__________________________________
; Sliders                          |
;__________________________________/

(defclass SLIDER-CONTROL (control)
  ((min-value :accessor min-value :initform 0.0 :initarg :min-value :type float :documentation "minimal value")
   (max-value :accessor max-value :initform 100.0 :initarg :max-value :type float :documentation "maximal value")
   (value :accessor value :initform 0.0 :initarg :value :type float :documentation "current value")
   (tick-marks :accessor tick-marks :initform 0 :initarg :tick-marks :type integer :documentation "number of tick marks, 0=no tick marks"))
  (:documentation "slider")
  (:default-initargs 
    :width 100
    :height 30))

;__________________________________
; Text                             |
;__________________________________/

(defclass LABEL-CONTROL (control)
  ((align :accessor align :initform :left :initarg :align :type keyword :documentation ":left, :center , :right, :justified"))
  (:documentation "static text label")
  (:default-initargs 
    :text ""
    :width 100
    :height 20))


(defmethod initialize-event-handling ((Self label-control))
  ;; not clickable
  )


(defclass EDITABLE-TEXT-CONTROL (control)
  ((align :accessor align :initform :left :initarg :align :type keyword :documentation ":left, :center , :right, :justified"))
  (:documentation "editable text")
  (:default-initargs
    :text ""
    :width 100
    :height 20))


(defmethod initialize-event-handling ((Self editable-text-control))
  ;; not clickable
  )


(defclass STATUS-BAR-CONTROL (control)
  ((align :accessor align :initform :left :initarg :align :type keyword :documentation ":left, :center , :right, :justified"))
  (:documentation "status bar")
  (:default-initargs
    :text ""
    :width 100
    :height 20))


(defmethod initialize-event-handling ((Self editable-text-control))
  ;; not clickable
  )

;__________________________________
; Progress Indicator               |
;__________________________________/

(defclass PROGRESS-INDICATOR-CONTROL (control)
  ((align :accessor align :initform :center :initarg :align :type keyword :documentation ":left, :center , :right, :justified"))
  (:documentation "This is a basic indeterminate progress indicator that will just show a busy progress bar.  This indicator is much simpler then the determinate progress indicator and must only be turned on and off.  ")
  (:default-initargs 
    :text ""
    :width 100
    :height 20))

;__________________________________
; Determinate Progress Indicator   |
;__________________________________/

(defclass DETERMINATE-PROGRESS-INDICATOR-CONTROL (control)
  ((align :accessor align :initform :center :initarg :align :type keyword :documentation ":left, :center , :right, :justified")
   (min-value :accessor min-value :initform 0.d0 :initarg :min-value :documentation "The min value of the determinate progress indicator")
   (max-value :accessor max-value :initform 100.d0 :initarg :max-value :documentation "The max value of the determinate progress indicator")
   )
  (:documentation "The determinate progress indicator must by incrimented from min-value to max-value using the increment-by method.  ")
  (:default-initargs 
    :text ""
    :width 100
    :height 20))

;__________________________________
; Image                             |
;__________________________________/

(defclass IMAGE-CONTROL (control)
  ((src :accessor src :initform nil :initarg :src :documentation "URL: so far only filename")
   (scale-proportionally :accessor scale-proportionally :initarg :scale-proportionally :type boolean :initform nil :documentation "If true the image will scale proportionally instead of scale to fit")
  (image-path :accessor image-path :initform nil :initarg :image-path :documentation "if this accesor is nil then the image-control will looks for 
        the image in the resources;images directory, if not it will look for the src image at the location specified by this accessor"))
  (:documentation "image. If size is 0,0 use original image size")
  (:default-initargs 
    :width 0
    :height 0))


(defgeneric SOURCE (image-control)
  (:documentation "If the src is local return a file specification"))


(defmethod SOURCE ((Self image-control))
  (if (image-path self)
    (native-path (image-path self) (src Self))
    (native-path "lui:resources;images;" (src Self))))


(defmethod initialize-event-handling ((Self image-control))
  ;; not clickable
  )


;__________________________________
; Color Well                       |
;__________________________________/

(defclass COLOR-WELL-CONTROL (control)
  ((color :accessor color :initform "FF0000" :type string :documentation "Hex RBB value for color e.g. FF0000 = RED")
   (show-alpha :accessor show-alpha :initform t :type boolean :documentation "If true include alpha controls in color picker")
   (user-action :accessor user-action :initform 'default-action2 :type symbol :documentation "The action that will be called once the default-action adjusts the color accessor"))
  (:default-initargs
      :text ""
    :action 'color-well-action)
  (:documentation "Color Well"))


(defmethod INITIALIZE-INSTANCE ((Self color-well-control) &rest Args)
  (declare (ignore Args))
  (call-next-method)
  (setf (user-action self) (action self))
  (setf (action self) 'color-well-action)
  (unless (target Self) (setf (target Self) Self)) ;; make me default target
  (initialize-event-handling Self))


(defgeneric GET-RED (color-well-control)
  (:documentation "Returns the red value of the RGB stored as a byte"))


(defgeneric GET-BLUE (color-well-control)
  (:documentation "Returns the blue value of the RGB stored as a byte"))


(defgeneric GET-GREEN (color-well-control)
  (:documentation "Returns the green value of the RGB stored as a byte"))


(defgeneric GET-ALPHA (color-well-control)
  (:documentation "Returns the alpha value stored as a byte"))


(defgeneric SET-COLOR (color-well-control &key Red Green Blue Alpha)
  (:documentation "Set the color of the color well"))


(defmethod COLOR-WELL-ACTION ((window window) (self COLOR-WELL-Control))
  (setf (color self) (concatenate 'string (write-to-string (get-red self) :base 16) (concatenate 'string (write-to-string (get-green self):base 16) (write-to-string (get-blue self):base 16))))
  (funcall (user-action self) window self))
  
;__________________________________
; Web                              |
;__________________________________/

(defclass WEB-BROWSER-CONTROL (control)
  ((URL :accessor URL :initform "http://www.agentsheets.com" :initarg :url :documentation "URL"))
  (:documentation "Web browser"))


(defmethod initialize-event-handling ((Self web-browser-control))
  ;; not clickable
  )

#| Examples:

;;***  EXAMPLE 1: a click and drag window containing a mouse controlled view

(defclass click-and-drag-window (window)
  ((blob :accessor blob :initform (make-instance 'rectangle-view))
   (drag-lag-rect :accessor drag-lag-rect :initform (make-instance 'rectangle-view))))


(defmethod initialize-instance :after ((Self click-and-drag-window) &rest Args)
  (declare (ignore Args))
  (set-color (blob Self) :red 0.5 :green 0.1)
  (set-frame (blob Self) :width 100 :height 100)
  (set-color (drag-lag-rect Self) :green 1.0)
  (set-frame (drag-lag-rect Self) :width 1 :height 1)
  (add-subviews Self (blob Self) (drag-lag-rect Self)))


(defmethod VIEW-LEFT-MOUSE-DOWN-EVENT-HANDLER ((Self click-and-drag-window) X Y)
  ;; (format t "click: x=~A, y=~A~%"  x y)
  (set-frame (blob Self) :x (- x 50) :y (- y 50)))


(defmethod  VIEW-LEFT-MOUSE-DRAGGED-EVENT-HANDLER ((Self click-and-drag-window) X Y dx dy)
  ;; (format t "click: x=~A, y=~A~%"  x y)
  (set-frame (blob Self) :x (- x 50) :y (- y 50))
  (set-frame (drag-lag-rect Self) :x (- x (abs dx) 3) :y (- y (abs dy) 3) 
             :width (abs dx) :height (abs dy)))


(defparameter *Window* (make-instance 'click-and-drag-window))


;;***  EXAMPLE 2: controls and targets
;; 3 buttons, red, green, and blue to switch the color of a view

;; define window and view subclasses

(defclass color-selection-window (window)
  ())


(defmethod initialize-instance :after ((Self color-selection-window) &rest Args)
  (declare (ignore Args))
  ;; make all views and link up the targets
  (let* ((Color-View (make-instance 'color-selection-view :y 50))
         (Red-Button (make-instance 'button-control :text "red" :target Color-View :action 'turn-red))
         (Green-Button (make-instance 'button-control :text "green" :x 100 :target Color-View :action 'turn-green))
         (Blue-Button (make-instance 'button-control :text "blue" :x 200 :target Color-View :action 'turn-blue)))
    (add-subviews Self Color-View Red-Button Green-Button Blue-Button)))


(defclass color-selection-view (rectangle-view)
  ())

;; actions

(defmethod turn-red ((window color-selection-window) (view color-selection-view))
  (set-color View :red 1.0 :green 0.0 :blue 0.0)
  (display window))

(defmethod turn-green ((window color-selection-window) (view color-selection-view))
  (set-color View :red 0.0 :green 1.0 :blue 0.0)
  (display window))


(defmethod turn-blue ((window color-selection-window) (view color-selection-view))
  (set-color View :red 0.0 :green 0.0 :blue 1.0)
  (display window))

;; done 

(defparameter *ColorWindow* (make-instance 'color-selection-window))

(do-subviews (View *ColorWindow*)
  (print View))

(map-subviews *ColorWindow* #'(lambda (view) (print View)))


;;*** EXAMPLE 3: sliders
;; Window with RGB sliders to adjust color

(defclass RGB-WINDOW (window)
  ((color-view :accessor color-view)
   (red-slider :accessor red-slider)
   (green-slider :accessor green-slider)
   (blue-slider :accessor blue-slider)
   (red-label :accessor red-label)
   (green-label :accessor green-label)
   (blue-label :accessor blue-label))
  (:default-initargs
      :width 200
    :height 200))


(defmethod initialize-instance :after ((Self rgb-window) &rest Args)
  (declare (ignore Args))
  ;; make slider views, target color view and use shared action method
  (setf (color-view Self) (make-instance 'color-view :y 100))
  (setf (red-slider Self) (make-instance 'slider-control :target (color-view Self) :x 50 :action 'adjust-color :max-value 1.0))
  (setf (green-slider Self) (make-instance 'slider-control :target (color-view Self) :x 50 :y 30 :action 'adjust-color :max-value 1.0))
  (setf (blue-slider Self) (make-instance 'slider-control :target (color-view Self) :x 50 :y 60 :action 'adjust-color :max-value 1.0))
  (add-subviews Self (color-view Self) (red-slider Self) (green-slider Self) (blue-slider Self))
  ;; add static labels
  (add-subviews 
   Self
   (make-instance 'label-control :text "red" :width 45)
   (make-instance 'label-control :text "green" :y 30 :width 45)
   (make-instance 'label-control :text "blue" :y 60 :width 45))
  ;; dynamic labels
  (setf (red-label Self) (make-instance 'label-control :text (format nil "~4,2F" (value (red-slider Self))) :x 160 :width 50))
  (setf (green-label Self) (make-instance 'label-control :text (format nil "~4,2F" (value (green-slider Self))) :x 160 :y 30 :width 50))
  (setf (blue-label Self) (make-instance 'label-control :text (format nil "~4,2F" (value (blue-slider Self))) :x 160 :y 60 :width 50))
  (add-subviews Self (red-label Self) (green-label Self) (blue-label Self)))


(defclass color-view (rectangle-view)
  ())

;; actions

(defmethod adjust-color ((window rgb-window) (view color-view))
  (set-color 
   view 
   :red (value (red-slider Window))
   :green (value (green-slider Window))
   :blue (value (blue-slider Window)))
  ;; update value labels
  (setf (text (red-label Window)) (format nil "~4,2F" (value (red-slider Window))))
  (setf (text (green-label Window)) (format nil "~4,2F" (value (green-slider Window))))
  (setf (text (blue-label Window)) (format nil "~4,2F" (value (blue-slider Window))))
  (display window))


(defparameter *RGB-Window* (make-instance 'rgb-window))


;*** EXAMPLE 4: Text

(defclass TEXT-WINDOW (window)
  ())


(defmethod initialize-instance :after ((Self text-window) &rest Args)
  (declare (ignore Args))
  (add-subviews Self (make-instance 'label-control :text "The quick brown fox jumps over the lazy dog" :width 500)))


(defparameter *text-window* (make-instance 'text-window))


;*** EXAMPLE 5: Modal Dialog

(defclass modal-window (window)
  ())


(defmethod initialize-instance :after ((Self modal-window) &rest Args)
  (declare (ignore Args))
  (add-subviews 
   Self
   (make-instance 'button-control :text "Cancel" :action 'cancel-action)
   (make-instance 'button-control :text "OK" :action 'OK-action :x 100)))


;; actions

(defmethod OK-action ((window modal-window) (button button-control))
  (stop-modal Window :ok))


(defmethod cancel-action ((window modal-window) (button button-control))
  (cancel-modal Window))



;; done 

(show-and-run-modal (make-instance 'modal-window :do-show-immediately nil :closeable nil))

|#