;;; MAC CCL Window
;;; LUI: Lisp User Interface
;;; Andri Ioannidou and Alexander Repenning
;;; Version: 0.2 11/28/08
;;;          0.3 03/12/09 CCL 1.3: do not rely on NSstring auto conversion
;;;          0.3.1 04/27/09 Raffael Cavallaro, raffaelcavallaro@mac.com web-browser-control fixed
;;;          0.4   05/30/09 Full Screen Mode

(in-package :LUI)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (ccl:use-interface-dir :carbon) 
  (open-shared-library "/System/Library/Frameworks/Carbon.framework/Carbon"))

;;*********************************
;; Native -> LUI name converters  *
;;*********************************

(defmethod NATIVE-TO-LUI-EVENT-TYPE (NS-Type)
  (case NS-Type
    (#.#$NSLeftMouseDown :left-mouse-down)
    (#.#$NSLeftMouseUp :left-mouse-up)
    (#.#$NSRightMouseDown :rignt-mouse-down)
    (#.#$NSRightMouseUp :right-mouse-up)
    (#.#$NSOtherMouseDown :other-mouse-down)
    (#.#$NSOtherMouseUp :other-mouse-up)
    (#.#$NSMouseMoved :mouse-moved)
    (#.#$NSLeftMouseDragged :left-mouse-dragged)
    (#.#$NSRightMouseDragged :right-mouse-dragged)
    (#.#$NSOtherMouseDragged :other-mouse-dragged)
    (#.#$NSMouseEntered :mouse-entered)
    (#.#$NSMouseExited :mouse-exited)
    (#.#$NSKeyDown :key-down)
    (#.#$NSKeyUp :key-up)
    (#.#$NSFlagsChanged :flags-changed)
    (#.#$NSAppKitDefined :app-kit-defined)
    (#.#$NSSystemDefined :system-defined)
    (#.#$NSApplicationDefined :application-defined)
    (#.#$NSPeriodic :periodic)
    (#.#$NSCursorUpdate :cursor-update)
    (#.#$NSScrollWheel :scroll-wheel)
    (t :undefined-event)))

;;*********************************
;; user defined System parameters *
;;*********************************

(defvar *System-Selection-Color*
  (let ((Color (#/colorUsingColorSpaceName: 
                (#/selectedTextBackgroundColor ns::ns-color)
                #@"NSCalibratedRGBColorSpace")))
    (list (float (#/redComponent Color) 0.0) (float (#/greenComponent Color) 0.0) (float (#/blueComponent Color) 0.0)))
  "system color defined by user used for selections, e.g., selected text background color")

;;*********************************
;; Native -> LUI Coordinates      *
;;*********************************

(defmethod LUI-SCREEN-COORDINATE (x y)
  (values
   x
   (truncate (- (pref (#/frame (#/mainScreen ns:ns-screen)) <NSR>ect.size.height) y)))) 

;;*********************************
;; Native Strings                 *
;;*********************************

(defun NATIVE-STRING (String) "
  Return a native string"
  (#/autorelease (ccl::%make-nsstring String)))

;**********************************
;* EVENT                          *
;**********************************

(defmethod COMMAND-KEY-P ()
  (when *Current-Event*
    (not (zerop (logand (#/modifierFlags (native-event *Current-Event*)) #$NSCommandKeyMask)))))


(defmethod ALT-KEY-P ()
  (when *Current-Event*
    (not (zerop (logand (#/modifierFlags (native-event *Current-Event*)) #$NSAlternateKeyMask)))))


(defmethod SHIFT-KEY-P ()
  (when *Current-Event*
    (not (zerop (logand (#/modifierFlags (native-event *Current-Event*)) #$NSShiftKeyMask)))))


(defmethod CONTROL-KEY-P ()
  (when *Current-Event*
    (not (zerop (logand (#/modifierFlags (native-event *Current-Event*)) #$NSControlKeyMask)))))

;**********************************
;* SUBVIEW-MANAGER-INTERFACE      *
;**********************************

(defmethod ADD-SUBVIEW ((View subview-manager-interface) (Subview subview-manager-interface))
  (#/addSubview: (native-view View) (native-view Subview))
  (setf (part-of Subview) View))
  

(defmethod ADD-SUBVIEWS ((Self subview-manager-interface) &rest Subviews)
  (dolist (Subview Subviews)
    (add-subview Self Subview)))


(defmethod MAP-SUBVIEWS ((Self subview-manager-interface) Function &rest Args)
  (let ((Subviews (#/subviews (native-view Self))))
    (dotimes (i (#/count Subviews))
      (apply Function (lui-view (#/objectAtIndex: Subviews i)) Args))))


(defmethod SUBVIEWS ((Self subview-manager-interface))
  (let* ((Subviews (#/subviews (native-view Self)))
         (Count (#/count Subviews))
         (Subview-List nil))
    (dotimes (i Count Subview-List)
      (push (lui-view (#/objectAtIndex: Subviews (- Count 1 i))) Subview-List))))


;**********************************
;* VIEW                           *
;**********************************

(defmethod SET-FRAME ((Self view) &key x y width height)
  (setf (x Self) (or x (x Self)))
  (setf (y Self) (or y (y Self)))
  (setf (width Self) (or width (width Self)))
  (setf (height Self) (or height (height Self)))
  (ns:with-ns-rect (Frame (x Self) (y Self) (width Self) (height Self))
    (#/setFrame: (native-view Self) Frame)))


(defmethod SET-SIZE :after ((Self view) Width Height)
  (ns:with-ns-size (Size Width Height)
    (#/setFrameSize: (native-view Self) Size)))


(defmethod SET-POSITION :after ((Self view) X Y)
  (ns:with-ns-point (Point X Y)
    (#/setFrameOrigin: (native-view Self) Point)))


(defmethod MAKE-NATIVE-OBJECT ((Self view))
  (let ((View (make-instance 'native-view :lui-view Self)))
    (ns:with-ns-rect (Frame (x Self) (y Self) (width Self) (height Self))
      (#/setFrame: View Frame))
    View))


(defmethod WINDOW ((Self view))
  (lui-window (#/window (native-view Self))))


(defmethod DISPLAY ((Self view))
  ;; will not work without flushing window
  (#/display (native-view Self)))

;__________________________________
; NATIVE-VIEW                      |
;__________________________________/

(defclass NATIVE-VIEW (ns:ns-view)
  ((lui-view :accessor lui-view :initform nil :initarg :lui-view))
  (:metaclass ns:+ns-object
	      :documentation "the native NSView associated with the LUI view"))


(objc:defmethod (#/drawRect: :void) ((self native-view) (rect :<NSR>ect))
  ;; (format t "~%drawRect simple view")
  (draw (lui-view Self)))


(objc:defmethod (#/display :void) ((self native-view))
  ;; (format t "~%display simple view")
  )


(objc:defmethod (#/isFlipped :<BOOL>) ((self native-view))
  ;; Flip to coordinate system to 0, 0 = upper left corner
  #$YES)

;**********************************
;* SCROLL-VIEW                    *
;**********************************

(defclass NATIVE-SCROLL-VIEW (ns:ns-scroll-view)
  ((lui-view :accessor lui-view :initform nil :initarg :lui-view))
  (:metaclass ns:+ns-object
	      :documentation "the native NSScrollView associated with the LUI view"))


(defmethod make-native-object ((Self scroll-view))
  (let ((Native-Control (make-instance 'native-scroll-view :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame)
      (#/setHasHorizontalScroller: Native-Control (has-horizontal-scroller Self))
      (#/setHasVerticalScroller: Native-Control (has-vertical-scroller Self))
      (#/setAutohidesScrollers: Native-Control #$YES)
      (#/setBorderType: Native-Control #$NSNoBorder)  ;;; #$NSLineBorder)
      (#/setDrawsBackground: Native-Control #$NO)
    Native-Control)))


(defmethod ADD-SUBVIEWS ((view scroll-view)  &rest Subviews)
  (call-next-method)
  ;; it only really makes sense to have one subview
  (#/setDocumentView: (native-view View) (native-view (first Subviews))))


(defmethod MAP-SUBVIEWS ((Self scroll-view) Function &rest Args)
  ;; no digging: only apply to document view
  (let ((Document-View (#/documentView (native-view Self))))
    (when Document-View 
      (apply Function (lui-view Document-View) Args))))


(defmethod SET-SIZE ((Self scroll-view) W H)
  (call-next-method)
  ;;  (format t "~%size ~A, ~A" W H)
  ;; need to propagate sizing to subviews: not likely to effect their sizes but needs to be done at least once
  (map-subviews Self #'(lambda (View) (set-size View (width View) (height View)))))

;**********************************
;* RECTANGLE-VIEW                 *
;**********************************

(defmethod SET-COLOR ((Self rectangle-view) &key (Red 0.0) (Green 0.0) (Blue 0.0) (Alpha 1.0))
  ;; keep a native color instead of creating a new one for each display
  (when (native-color Self) (#/release (native-color Self)))
  (setf (native-color Self) (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color Red Green Blue Alpha))
  (#/retain (native-color Self)))


(defmethod DRAW ((Self rectangle-view))
  (when (native-color Self)
    (#/set (native-color Self)))
  (ns:with-ns-rect (Frame 0.0 0.0 (width Self) (height Self))
    (#_NSRectFill Frame)))

;;************************************
;; WINDOW                            *
;;************************************
;__________________________________
; NATIVE-WINDOW                     |
;__________________________________/

(defclass NATIVE-WINDOW (ns:ns-window)
  ((lui-window :accessor lui-window :initarg :lui-window)
   (delegate :accessor delegate :initform nil :initarg :delegate :documentation "event delegate"))
  (:metaclass ns:+ns-object
	      :documentation "Native window"))


(objc:defmethod (#/sendEvent: :void) ((Self native-window) Event)
  ;; (print (native-to-lui-event-type (#/type Event)))
  (call-next-method Event))


(objc:defmethod (#/mouseMoved: :void) ((self native-window) Event)
  (let ((mouse-loc (#/locationInWindow event)))
    (view-event-handler (lui-window Self) 
                        (make-instance 'mouse-event
                          :x (truncate (pref mouse-loc :<NSP>oint.x))
                          :y (truncate (- (height (lui-window Self)) (pref mouse-loc :<NSP>oint.y)))
                          :dx (truncate (#/deltaX Event))
                          :dy (truncate (#/deltaY Event))
                          :event-type (native-to-lui-event-type (#/type event))
                          :native-event Event))))


(objc:defmethod (#/constrainFrameRect:toScreen: :<NSR>ect) ((Self native-window) (Rect :<NSR>ect) Screen)
  (declare (ignore Screen))
  ;; a nasty hack to be able move windows above the menu bar
  (if (full-screen (lui-window Self))
    (let ((Window-Title-Bar-Height 22))
      (ns:make-ns-rect 
       0
       0
       (pref (#/frame (#/screen Self)) <NSR>ect.size.width)
       (+ (pref (#/frame (#/screen Self)) <NSR>ect.size.height) Window-Title-Bar-Height)))
    Rect))


#| 
(objc:defmethod (#/displayIfNeeded :void) ((Self native-window))
  (call-next-method)
  (display (lui-window Self))
  (print "displayIfNeeded"))

|#

;__________________________________
; Window-delegate                   |
;__________________________________/

(defclass WINDOW-DELEGATE (ns:ns-object)
  ((lui-window :accessor lui-window :initarg :lui-window))
  (:metaclass ns:+ns-object
	      :documentation "delegate object receiving window events"))


(objc:defmethod (#/windowDidResize: :void) ((self window-delegate) Notification)
  (declare (ignore Notification))
  ;; only the size of the content view
  (let ((Content-View (#/contentView (native-window (lui-window Self)))))
    (setf (width (lui-window Self)) (truncate (pref (#/frame Content-View) <NSR>ect.size.width)))
    (setf (height (lui-window Self)) (truncate (pref (#/frame Content-View) <NSR>ect.size.height)))
    (size-changed-event-handler (lui-window Self) (width (lui-window Self)) (height (lui-window Self)))))


(objc:defmethod (#/windowDidMove: :void) ((self window-delegate) Notification)
  (declare (ignore Notification))
  (let ((Window (lui-window Self)))
    (setf (x Window) (truncate (pref (#/frame (native-window (lui-window Self))) <NSR>ect.origin.x)))
    (setf (y Window) 
          (- (screen-height (lui-window Self)) 
             (height (lui-window Self))
             (truncate (pref (#/frame (native-window (lui-window Self))) <NSR>ect.origin.y))))))

;__________________________________
; window methods                   |
;__________________________________/

(defmethod MAKE-NATIVE-OBJECT ((Self window))
  (ccl::with-autorelease-pool
      (let ((Window (make-instance 'native-window
                      :lui-window Self
                      :with-content-rect (ns:make-ns-rect 0 0 (width Self) (height Self))
                      :style-mask (logior (if (title Self) #$NSTitledWindowMask 0)
                                          (if (closeable Self) #$NSClosableWindowMask 0)
                                          (if (resizable Self) #$NSResizableWindowMask 0)
                                          (if (minimizable Self) #$NSMiniaturizableWindowMask 0))
                      :backing #$NSBackingStoreBuffered
                      :defer t)))
        (setf (native-window Self) Window)  ;; need to have this reference for the delegate to be in place
        (setf (native-view Self) (make-instance 'native-window-view :lui-window Self))
        ;; setup delegate
        (setf (delegate Window) (make-instance 'window-delegate :lui-window Self))
        (#/setDelegate: Window (delegate Window))
        ;; content view
        (#/setContentView: Window (#/autorelease (native-view Self)))
        (#/setTitle: Window (native-string (title Self)))
        (ns:with-ns-size (Position (x Self) (- (screen-height Self)  (y Self)))
          (#/setFrameTopLeftPoint: (native-window Self) Position))
        (when (track-mouse Self) (#/setAcceptsMouseMovedEvents: (native-window Self) #$YES))
        Window)))


(defmethod DISPLAY ((Self window))
  ;; excessive?  
  (#/display (native-view Self)))


(defmethod SET-SIZE :after ((Self window) Width Height)
  (ns:with-ns-size (Size Width Height)
    (#/setContentSize: (native-window Self) Size)))


(defmethod SET-POSITION :after ((Self window) x y)
  (ns:with-ns-size (Position x (- (screen-height Self)  y))
    (#/setFrameTopLeftPoint: (native-window Self) Position)))


(defmethod SHOW ((Self window))
  (let ((y (truncate (- (pref (#/frame (#/mainScreen ns:ns-screen)) <NSR>ect.size.height) (y Self) (height Self)))))
    ;; (ns:with-ns-rect (Frame (x Self) y (width Self) (height Self))
    ;;  (#/setFrame:display: (native-window Self) Frame t)))
  (#/orderFront: (native-window Self) nil))  )


(defmethod HIDE ((Self window))
  (#/orderOut: (native-window Self) nil))


(defmethod SCREEN-WIDTH ((Self window))
  (truncate (pref (#/frame (or (#/screen (native-window Self))
                               (#/mainScreen ns:ns-screen))) 
                  <NSR>ect.size.width)))


(defmethod SCREEN-HEIGHT ((Self window))
  (truncate (pref (#/frame (or (#/screen (native-window Self))
                               (#/mainScreen ns:ns-screen))) 
                  <NSR>ect.size.height)))


(defvar *Run-Modal-Return-Value* nil "shared valued used to return the run modal values")

;; Modal windows

(defmethod SHOW-AND-RUN-MODAL ((Self window))
  (declare (special *Run-Modal-Return-Value*))
  (setq *Run-Modal-Return-Value* nil)
  (when (#/isVisible (native-window Self))
    (error "cannot run modal a window that is already visible"))
  (let ((Code (#/runModalForWindow: (#/sharedApplication ns:ns-application) (native-window Self))))
    ;; ignore Code for now
    (#/close (native-window Self))
    (case *Run-Modal-Return-Value*
      (:cancel (throw :cancel nil))
      (t *Run-Modal-Return-Value*))))


(defmethod STOP-MODAL ((Self window) Return-Value)
  (setq *Run-Modal-Return-Value* Return-Value)
  (#/stopModal (#/sharedApplication ns:ns-application)))


(defmethod CANCEL-MODAL ((Self window))
  (setq *Run-Modal-Return-Value* :cancel)
  (#/stopModal (#/sharedApplication ns:ns-application)))


;; screen mode

(defvar *Window-Full-Screen-Restore-Sizes* (make-hash-table))


(defmethod SWITCH-TO-FULL-SCREEN-MODE ((Self window))
  (setf (gethash Self *Window-Full-Screen-Restore-Sizes*) (#/frame (native-window Self)))
  (#_SetSystemUIMode #$kUIModeAllSuppressed #$kUIOptionAutoShowMenuBar)
  (setf (full-screen Self) t)
  ;;; random sizing to trigger #/constrainFrameRect:toScreen
  ;;; (set-size Self 100 100)
  (#/orderFront: (native-window Self) (native-window Self))
  (#/makeKeyWindow (native-window Self)))


(defmethod SWITCH-TO-WINDOW-MODE ((Self window))
  (#_SetSystemUIMode #$kUIModeNormal 0)
  (setf (full-screen Self) nil)
  (let ((Frame (gethash Self *Window-Full-Screen-Restore-Sizes*)))
    (when Frame
      (#/setFrame:display:animate: (native-window Self) Frame #$YES #$NO))))

;__________________________________
; Window query functions            |
;__________________________________/

(defun FIND-WINDOW-AT-SCREEN-POSITION (screen-x screen-y &key Type) "
  Return a LUI window at screen position x, y.
  If there is no window return nil
  If there are multiple windows return the topmost one"
  (multiple-value-bind (x y) (lui-screen-coordinate screen-x screen-y)
    (let ((Lui-Windows nil)
          (All-Windows (#/windows (#/sharedApplication ns::ns-application))))
      (dotimes (i (#/count All-Windows) (first (sort Lui-Windows #'< :key #'(lambda (w) (#/orderedIndex (native-window w))))))
        (let ((Window (#/objectAtIndex: All-Windows i)))
          (when (and 
                 (#/isVisible Window)
                 (slot-exists-p Window 'lui-window)
                 (if Type 
                   (subtypep (type-of (lui-window Window)) (find-class Type))
                   t))
            (let ((Frame (#/frame Window)))
              (when (and (<= (pref Frame <NSR>ect.origin.x) x (+ (pref Frame <NSR>ect.origin.x) (pref Frame <NSR>ect.size.width)))
                         (<= (pref Frame <NSR>ect.origin.y) y (+ (pref Frame <NSR>ect.origin.y) (pref Frame <NSR>ect.size.height))))
                (push (lui-window Window) Lui-Windows)))))))))

;; (find-window-at-screen-position 10 100)




;__________________________________
; NATIVE-WINDOW-VIEW                |
;__________________________________/

(defclass native-window-view (ns:ns-view)
  ((lui-window :accessor lui-window :initarg :lui-window))
  (:metaclass ns:+ns-object
	      :documentation "dispatch NS events to LUI events. A LUI window needs to contain on dispatch view"))


(objc:defmethod (#/mouseDown: :void) ((self native-window-view) event)
  (let ((mouse-loc (#/locationInWindow event)))
    (view-event-handler (lui-window Self) 
                        (make-instance 'mouse-event
                          :x (truncate (pref mouse-loc :<NSP>oint.x))
                          :y (truncate (- (height (lui-window Self)) (pref mouse-loc :<NSP>oint.y)))
                          :event-type (native-to-lui-event-type (#/type event))
                          :native-event Event))))


(objc:defmethod (#/mouseUp: :void) ((self native-window-view) event)
  (let ((mouse-loc (#/locationInWindow event)))
    (view-event-handler (lui-window Self) 
                        (make-instance 'mouse-event
                          :x (truncate (pref mouse-loc :<NSP>oint.x))
                          :y (truncate (- (height (lui-window Self)) (pref mouse-loc :<NSP>oint.y)))
                          :event-type (native-to-lui-event-type (#/type event))
                          :native-event Event))))


(objc:defmethod (#/mouseDragged: :void) ((self native-window-view) event)
  (let ((mouse-loc (#/locationInWindow event)))
    ;;(format t "~%dragged to ~A, ~A," (pref mouse-loc :<NSP>oint.x) (- (height (lui-window Self)) (pref mouse-loc :<NSP>oint.y)))
    (view-event-handler (lui-window Self) 
                        (make-instance 'mouse-event
                          :x (truncate (pref mouse-loc :<NSP>oint.x))
                          :y (truncate (- (height (lui-window Self)) (pref mouse-loc :<NSP>oint.y)))
                          :dx (truncate (#/deltaX Event))
                          :dy (truncate (#/deltaY Event))
                          :event-type (native-to-lui-event-type (#/type event))
                          :native-event Event))))


(objc:defmethod (#/isFlipped :<BOOL>) ((self native-window-view))
  ;; Flip to coordinate system to 0, 0 = upper left corner
  #$YES)

;****************************************************
; CONTROLs                                          *
;****************************************************

(defclass native-target (ns:ns-object)
  ((native-control :accessor native-control :initarg :native-control)
   (lui-control :accessor lui-control :initarg :lui-control))
  (:metaclass ns:+ns-object)
  (:documentation "receives action events and forwards them to lui control"))


(objc:defmethod (#/activateAction :void) ((self native-target))
  ;; dispatch action to window + target
  (funcall (action (lui-control Self)) (window (lui-control Self)) (target (lui-control Self))))


(defmethod initialize-event-handling ((Self control))
  (#/setTarget: (native-view Self) 
                (make-instance 'native-target 
                  :native-control (native-view Self)
                  :lui-control Self))
  (#/setAction: (native-view Self) (objc::@selector #/activateAction)))


(objc:defmethod (#/isFlipped :<BOOL>) ((self ns:ns-control))
  ;; ALL controls: Flip to coordinate system to 0, 0 = upper left corner
  #$YES)


;__________________________________
; BUTTON                           |
;__________________________________/

(defclass native-button (ns:ns-button)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod make-native-object ((Self button-control))
  (let ((Native-Control (make-instance 'native-button :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame)
      (#/setButtonType: Native-Control #$NSMomentaryPushInButton)
      (when (default-button Self)
        (#/setKeyEquivalent: Native-Control  #@#.(string #\return)))
      (#/setImagePosition: Native-Control #$NSNoImage)
      (#/setBezelStyle: Native-Control #$NSRoundedBezelStyle)
      (#/setTitle: Native-Control (native-string (text Self))))
    Native-Control))


(defmethod (setf text) :after (Text (Self button-control))
  (#/setTitle: (native-view Self) (native-string Text)))

;__________________________________
; BEVEL BUTTON                      |
;__________________________________/

(defclass native-bevel-button (ns:ns-button)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod make-native-object ((Self bevel-button-control))
  (let ((Native-Control (make-instance 'native-bevel-button :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame)
      (#/setButtonType: Native-Control #$NSMomentaryPushInButton)
      (#/setImagePosition: Native-Control #$NSNoImage)
      (#/setBezelStyle: Native-Control #$NSThickerSquareBezelStyle)
      (#/setTitle: Native-Control (native-string (text Self))))
    Native-Control))


(defmethod (setf text) :after (Text (Self bevel-button-control))
  (#/setTitle: (native-view Self) (native-string Text)))

;__________________________________
; SLIDER                           |
;__________________________________/

(defclass native-slider (ns:ns-slider)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod make-native-object ((Self slider-control))
  (let ((Native-Control (make-instance 'native-slider :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame)
      (#/setMinValue: Native-Control (float (min-value Self) 0d0))
      (#/setMaxValue: Native-Control (float (max-value Self) 0d0))
      (#/setNumberOfTickMarks: Native-Control (truncate (tick-marks Self)))
      (#/setTitle: Native-Control (native-string (text Self))))  ;; depreciated: use separate label
    Native-Control))


(defmethod VALUE ((Self slider-control))
  (#/floatValue (native-view Self)))

;__________________________________
; LABEL                            |
;__________________________________/

(defclass native-label (ns:ns-text-view)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod make-native-object ((Self label-control))
  (let ((Native-Control (make-instance 'native-label :lui-view Self)))  ;; NSText is not actually a control, would NSTextField be better?
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame)
      (#/setDrawsBackground: Native-Control nil)
      (#/setString: Native-Control (native-string (text Self)))
      (ecase (align Self)
        (:left (#/alignLeft: Native-Control Native-Control))
        (:center (#/alignCenter: Native-Control Native-Control))
        (:right (#/alignRight: Native-Control Native-Control))
        (:justified (#/alignJustified: Native-Control Native-Control)))
      (#/setEditable: Native-Control #$NO)
      (#/setSelectable: Native-Control #$NO) )
    Native-Control))


(defmethod (setf text) :after (Text (Self label-control))
  (#/setString: (native-view Self) (native-string Text)))

;__________________________________
; Editable TEXT                    |
;__________________________________/

(defclass native-editable-text (ns:ns-text-field)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod make-native-object ((Self editable-text-control))
  (let ((Native-Control (make-instance 'native-editable-text :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame)
      (#/setDrawsBackground: Native-Control nil)
      (#/setStringValue: Native-Control (native-string (text Self)))
      #| (ecase (align Self)
        (:left (#/alignLeft: Native-Control Native-Control))
        (:center (#/alignCenter: Native-Control Native-Control))
        (:right (#/alignRight: Native-Control Native-Control))
        (:justified (#/alignJustified: Native-Control Native-Control))) |# )  
    Native-Control))


(defmethod (setf text) :after (Text (Self editable-text-control))
  (#/setStringValue: (native-view Self) (native-string Text)))


(defmethod VALUE ((Self editable-text-control))
  (ccl::lisp-string-from-nsstring 
   (#/stringValue (native-view Self))))

(defmethod (setf VALUE)  (Text (Self editable-text-control))
  (#/setStringValue: (native-view Self) (native-string Text)))


(defmethod MAP-SUBVIEWS ((Self editable-text-control) Function &rest Args)
  (declare (ignore Function Args))
  ;; no Cocoa digging
  )


(defmethod SUBVIEWS ((Self editable-text-control))
  ;; no Cocoa digging
  )


;__________________________________
; IMAGE                            |
;__________________________________/

(defclass native-image (ns:ns-image-view)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod make-native-object ((Self image-control))
  (let ((Native-Control (make-instance 'native-image :lui-view Self))) 
      ;; no problem if there is no source, just keep an empty view
      (cond
       ;; in most cases there should be an image source
       ((src Self)
        ;; consider caching image with the same file, there is a good chance
        ;; that some image files, e.g., buttons are used frequently
        (let ((Image (#/initByReferencingFile: (#/alloc ns:ns-image) (native-string (file Self)))))
          (unless (#/isValid Image) (error "cannot create image from file ~S" (file Self)))
          ;; if size 0,0 use original size
          (when (and (zerop (width Self)) (zerop (height Self)))
            (let ((Size (#/size Image)))
              (setf (width Self) (rref Size <NSS>ize.width))
              (setf (height Self) (rref Size <NSS>ize.height))))
          (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
            (#/initWithFrame: Native-Control Frame))
          (#/setImage: Native-Control Image)
          (#/setImageScaling: Native-Control #$NSScaleToFit)))
       (t
        (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
              (#/initWithFrame: Native-Control Frame))))
      Native-Control))


;__________________________________
; Web Browser                      |
;__________________________________/

(eval-when (:compile-toplevel :load-toplevel :execute)
  (objc:load-framework "WebKit" :webkit))


(defclass native-web-browser (ns:web-view)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod make-native-object% ((Self web-browser-control))
 (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
                        ;; code borrowed from Clozure/Webkit.lisp
                        (let ((Native-Control (make-instance 'native-web-browser
                                                :with-frame Frame
                                                :frame-name #@"frame"
                                                :group-name #@"group"
                                                :lui-view Self)))
                          ;; Start a URL request.  The request is processed
                          ;; asynchronously, but apparently needs to be initiated
                          ;; from the event-handling thread.
                          (let* ((webframe (#/mainFrame Native-Control))
                                 (request (#/requestWithURL:
                                           ns:ns-url-request
                                           (ccl::with-autorelease-pool
                                               (#/retain (#/URLWithString: ns:ns-url (ccl::%make-nsstring (string (url Self)))))))))
                            ;; Failing to wait until the main thread has
                            ;; initiated the request seems to cause
                            ;; view-locking errors.  Maybe that's just
                            ;; an artifact of some other problem.
                            (#/performSelectorOnMainThread:withObject:waitUntilDone:
                             webframe (ccl::@selector #/loadRequest:) request t)
                            Native-Control))))


(defmethod make-native-object ((Self web-browser-control))
 (let* ((ip ccl::*initial-process*))
   (if (eq ccl::*current-process* ip)
     (make-native-object% self)
     (let* ((s (make-semaphore))
            (v nil))
       (process-interrupt ip (lambda ()
                               (setq v (make-native-object% self))
                               (signal-semaphore s)))
       (wait-on-semaphore s)
       v))))


(defmethod MAP-SUBVIEWS ((Self web-browser-control) Function &rest Args)
  (declare (ignore Function Args))
  ;; no DOM digging
  )


(defmethod SUBVIEWS ((Self web-browser-control))
  ;; no DOM digging
  )

;; only for the sake of exploration: can't really get into the DOM: stuck at WebHTMLView

(defun print-dom (View &optional (Level 0))
  (let ((Subviews (#/subviews View)))
    (dotimes (i (#/count Subviews))
      (dotimes (i Level) (princ "  "))
      (let ((Subview (#/objectAtIndex: Subviews i)))
        (format t "~A~%" Subview)
        (print-dom Subview (1+ Level))))))

;; (lui::print-dom (lui::native-view <web-browser url="http://www.agentsheets.com"/>))


