;;; MAC CCL Windoinvow
;;; LUI: Lisp User Interface
;;; Andri Ioannidou and Alexander Repenning
;;; Version: 0.2 11/28/08
;;;          0.3 03/12/09 CCL 1.3: do not rely on NSstring auto conversion
;;;          0.3.1 04/27/09 Raffael Cavallaro, raffaelcavallaro@mac.com web-browser-control fixed
;;;          0.4   05/30/09 Full Screen Mode
;;;          0.4.1 02/19/10 add-subviews and add-subview for scroll-view: only allow one subview; fixed map-subiews for scroll-view
;;;          0.4.2 03/15/10 AI file->source for image-control

(in-package :LUI)


#-cocotron
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
    #-cocotron (#.#$NSOtherMouseDown :other-mouse-down)
    #-cocotron (#.#$NSOtherMouseUp :other-mouse-up)
    (#.#$NSMouseMoved :mouse-moved)
    (#.#$NSLeftMouseDragged :left-mouse-dragged)
    (#.#$NSRightMouseDragged :right-mouse-dragged)
    #-cocotron (#.#$NSOtherMouseDragged :other-mouse-dragged)
    (#.#$NSMouseEntered :mouse-entered)
    (#.#$NSMouseExited :mouse-exited)
    (#.#$NSKeyDown :key-down)
    (#.#$NSKeyUp :key-up)
    (#.#$NSFlagsChanged :flags-changed)
    #-cocotron (#.#$NSAppKitDefined :app-kit-defined)
    #-cocotron (#.#$NSSystemDefined :system-defined)
    (#.#$NSApplicationDefined :application-defined)
    (#.#$NSPeriodic :periodic)
    (#.#$NSCursorUpdate :cursor-update)
    (#.#$NSScrollWheel :scroll-wheel)
    (t :undefined-event)))

;;*********************************
;; user defined System parameters *
;;*********************************

(defvar *System-Selection-Color*
  #-cocotron
  (let ((Color (#/colorUsingColorSpaceName: 
                (#/selectedTextBackgroundColor ns::ns-color)
                #@"NSCalibratedRGBColorSpace")))
    (list (float (#/redComponent Color) 0.0) (float (#/greenComponent Color) 0.0) (float (#/blueComponent Color) 0.0)))
  ;; Cocotron doesn't implement #@"NSCalibratedRGBColorSpace" nor #/redComponent, etc.
  #+cocotron (list 0.654139 0.793225 0.9990845)
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


(defmethod DOUBLE-CLICK-P ()
  (when *Current-Event*
    (= (#/clickCount (native-event *Current-Event*)) 2)))


;;*********************************
;; Mouse Polling                  *
;;*********************************

(defun MOUSE-LOCATION () "
  out: x y 
  Return screen coordinates of the current mouse location"
  (let ((Location (#/mouseLocation ns:ns-event)))
    (values
     (truncate (pref Location :<NSP>oint.x))
     (truncate (- (screen-height nil) (pref Location :<NSP>oint.y))))))

;(mouse-location)

;**********************************
;* SUBVIEW-MANAGER-INTERFACE      *
;**********************************

(defmethod ADD-SUBVIEW ((View subview-manager-interface) (Subview subview-manager-interface))
  (#/addSubview: (native-view View) (native-view Subview))
  (setf (part-of Subview) View))
  

(defmethod ADD-SUBVIEWS ((Self subview-manager-interface) &rest Subviews)
  (dolist (Subview Subviews)
    (add-subview Self Subview)))


(defmethod SWAP-SUBVIEW ((View subview-manager-interface) (Old-Subview subview-manager-interface) (New-Subview subview-manager-interface))
  ;; make compatible: new and old compatible with respect to size and origin
  (set-size New-Subview (width Old-Subview) (height Old-Subview))
  ;; adjust structure
  (#/retain (native-view Old-Subview)) ;; no GC
  (#/replaceSubview:with: (native-view View) (native-view Old-Subview) (native-view New-Subview))
  (setf (part-of New-Subview) View))
  


(defmethod MAP-SUBVIEWS ((Self subview-manager-interface) Function &rest Args)
  (let ((Subviews (#/subviews (native-view Self))))
    (dotimes (i (#/count Subviews))
      (apply Function (lui-view (#/objectAtIndex: Subviews i)) Args))))


(defmethod SUBVIEWS ((Self subview-manager-interface))
  (when (native-view Self)              ;Allows tracing MAKE-NATIVE-OBJECT
    (let* ((Subviews (#/subviews (native-view Self)))
           (Count (#/count Subviews))
           (Subview-List nil))
      (dotimes (i Count Subview-List)
        (push (lui-view (#/objectAtIndex: Subviews (- Count 1 i))) Subview-List)))))


(defmethod SUPERVIEW ((Self subview-manager-interface))
  (let ((Superview (#/superview (native-view Self))))
    (and (not (%null-ptr-p Superview))
         (slot-exists-p Superview 'lui-view)
         (lui-view Superview))))


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


(defmethod SWAP-SUBVIEW ((View view) (Old-Subview view) (New-Subview view))
  ;; (#/disableScreenUpdatesUntilFlush (native-window (window View)))
  ;; make compatible: new and old compatible with respect to size and origin
  (setf (x New-Subview) (x Old-Subview))
  (setf (y New-Subview) (y Old-Subview))
  (set-size New-Subview (width Old-Subview) (height Old-Subview))
  (#/setFrame: (native-view New-Subview) (#/frame (native-view Old-Subview)))
  ;; adjust structure
  (#/retain (native-view Old-Subview)) ;; no GC
  (#/replaceSubview:with: (native-view View) (native-view Old-Subview) (native-view New-Subview))
  (setf (part-of New-Subview) View)
  (subviews-swapped (window View) Old-Subview New-Subview)
  ;; (#/flushWindow (native-window (window View)))
  )


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
  ;; if there is an error make this restartable from alternate console
  (with-simple-restart (abandon-drawing "Stop trying Cocoa to draw in ~s" Self)
    (draw (lui-view Self))))


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
      #-cocotron (#/setAutohidesScrollers: Native-Control #$YES)
      (#/setBorderType: Native-Control #$NSNoBorder)  ;;; #$NSLineBorder)
      (#/setDrawsBackground: Native-Control #$NO)
    Native-Control)))


(defmethod ADD-SUBVIEWS ((view scroll-view)  &rest Subviews)
  (call-next-method)
  ;; it only really makes sense to have one subview
  (#/setDocumentView: (native-view View) (native-view (first Subviews)))
  (warn "You are adding multiple views to a scroll-view. Only the first one will be visible."))


(defmethod ADD-SUBVIEW ((view scroll-view)  Subview)
  (call-next-method)
  (unless (%null-ptr-p (#/documentView (native-view View))) 
    (warn "Redefining document view of scroll view. Only one subview is allowed in a scroll view."))
  ;; make it the document view
  (#/setDocumentView: (native-view View) (native-view Subview)))


(defmethod MAP-SUBVIEWS ((Self scroll-view) Function &rest Args)
  ;; no digging: only apply to document view
  (let ((Document-View (#/documentView (native-view Self))))
    ;; can't use (when Document-View) -- empty document view is a null pointer 
    (unless (%null-ptr-p Document-View) 
      (apply Function (lui-view Document-View) Args))))


(defmethod SET-SIZE ((Self scroll-view) W H)
  (declare (ignore W H))
  (call-next-method)
  ;;  (format t "~%size ~A, ~A" W H)
  ;; need to propagate sizing to subviews: not likely to effect their sizes but needs to be done at least once
  (map-subviews Self #'(lambda (View) (set-size View (width View) (height View)))))


(defmethod SET-SIZE-ONCE ((Self scroll-view) W H)
  (declare (ignore W H))
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



(objc:defmethod (#/becomeMainWindow :void) ((self native-window))
  (call-next-method)
  (has-become-main-window (lui-window Self)))


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

(defmacro IN-MAIN-THREAD (() &body body)
  (let ((thunk (gensym))
        (done (gensym))
        (result (gensym)))
    `(let ((,done nil)
           (,result nil))
       (flet ((,thunk ()
                (setq ,result (multiple-value-list (progn ,@body))
                      ,done t)))
         (gui::execute-in-gui #',thunk)
         (process-wait "Main thread" #'(lambda () ,done))
         (values-list ,result)))))


(defmethod MAKE-NATIVE-OBJECT ((Self window))
  (in-main-thread ()
    (ccl::with-autorelease-pool
      (let ((Window (make-instance 'native-window
                        :lui-window Self
                        :with-content-rect (ns:make-ns-rect 0 0 (width Self) (height Self))
                        :style-mask (if (borderless Self)
                                      0
                                      (logior (if (title Self) #$NSTitledWindowMask 0)
                                              (if (closeable Self) #$NSClosableWindowMask 0)
                                              (if (resizable Self) #$NSResizableWindowMask 0)
                                              (if (minimizable Self) #$NSMiniaturizableWindowMask 0)))
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
        Window))))


(defmethod DISPLAY ((Self window))
  ;; excessive?  
  (in-main-thread ()
    (#/display (native-view Self))))


(defmethod SET-SIZE :after ((Self window) Width Height)
  (ns:with-ns-size (Size Width Height)
    (#/setContentSize: (native-window Self) Size)))


(defmethod SET-POSITION :after ((Self window) x y)
  (ns:with-ns-size (Position x (- (screen-height Self)  y))
    (#/setFrameTopLeftPoint: (native-window Self) Position)))


(defmethod SHOW ((Self window))
  (in-main-thread ()
    ;; (let ((y (truncate (- (pref (#/frame (#/mainScreen ns:ns-screen)) <NSR>ect.size.height) (y Self) (height Self)))))
    ;;   (ns:with-ns-rect (Frame (x Self) y (width Self) (height Self))
    ;;   (#/setFrame:display: (native-window Self) Frame t)))
    (#/orderFront: (native-window Self) nil)))


(defmethod HIDE ((Self window))
  (in-main-thread ()
    (#/orderOut: (native-window Self) nil)))


(defmethod SCREEN-WIDTH ((Self window))
  (truncate (pref (#/frame (or (#/screen (native-window Self))
                               (#/mainScreen ns:ns-screen))) 
                  <NSR>ect.size.width)))


(defmethod SCREEN-HEIGHT ((Self window))
  (truncate (pref (#/frame (or (#/screen (native-window Self))
                               (#/mainScreen ns:ns-screen))) 
                  <NSR>ect.size.height)))


(defmethod SCREEN-HEIGHT ((Self null))
  (truncate (pref (#/frame (#/mainScreen ns:ns-screen))
                  <NSR>ect.size.height)))


(defmethod (setf TITLE) :after (Title (self window))
  (#/setTitle: (native-window Self) (native-string Title)))


(defvar *Run-Modal-Return-Value* nil "shared valued used to return the run modal values")

;; Modal windows

(defmethod SHOW-AND-RUN-MODAL ((Self window))
  (declare (special *Run-Modal-Return-Value*))
  (setq *Run-Modal-Return-Value* nil)
  (when (#/isVisible (native-window Self))
    (error "cannot run modal a window that is already visible"))
  (let ((Code (in-main-thread () 
                (#/runModalForWindow: (#/sharedApplication ns:ns-application)
                                      (native-window Self)))))
    (declare (ignore Code))
    ;; ignore Code for now
    (in-main-thread () (#/close (native-window Self)))
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
  #-cocotron (#_SetSystemUIMode #$kUIModeAllSuppressed #$kUIOptionAutoShowMenuBar)
  (setf (full-screen Self) t)
  ;;; random sizing to trigger #/constrainFrameRect:toScreen
  ;;; (set-size Self 100 100)
  (#/orderFront: (native-window Self) (native-window Self))
  (#/makeKeyWindow (native-window Self)))


(defmethod SWITCH-TO-WINDOW-MODE ((Self window))
  #-cocotron (#_SetSystemUIMode #$kUIModeNormal 0)
  (setf (full-screen Self) nil)
  (let ((Frame (gethash Self *Window-Full-Screen-Restore-Sizes*)))
    (when Frame
      (#/setFrame:display:animate: (native-window Self) Frame #$YES #$NO))))

;__________________________________
; Window query functions            |
;__________________________________/
;;This method still has lots of issues:
;;First, I have to add one to i.  This seems to cause the return value to to match up with orderedIndex but also seems very dangerous, and unlikely
;;to be a universal solution.  This was tested by making four different application windows and commenting the first three lines of this method so 
;;that it always returned the cocotron value.  I tried arranging the windows in many different ways and comparing the return value of this method 
;;to the value return by (#/orderedIndex Window), and it was always the same.  Another issue is that it seems that the method 
;;(#/sharedApplication ns::ns-application) works differently in cocotron.  Used the method ordered-test I determined that (#/sharedApplication ns::ns-application)
;;returns the order in which the windows are stacked on the mac version but in cocotron it return the order in which they were created.  It seems that this may not
;;be a good solution after all.  
(defun ORDERED-WINDOW-INDEX (Window)
  #-:cocotron 
  (#/orderedIndex Window)
  #+:cocotron
   (let ((window-array  (#/orderedWindows (#/sharedApplication ns::ns-application)))) 
     (dotimes (i (#/count window-array))
       (let ((array-window (#/objectAtIndex: window-array i)))         
         (if (equal window array-window)
           (progn            
             (return-from ordered-window-index (+ 1 i))))))
     (return-from ordered-window-index nil)))


(defun ORDERED-TEST ()
  (let ((window-array  (#/orderedWindows (#/sharedApplication ns::ns-application)))) 
    (dotimes (i (#/count window-array))
      (let ((array-window (#/objectAtIndex: window-array i)))    
        (print (#/title array-window))))))


(defun FIND-WINDOW-AT-SCREEN-POSITION (screen-x screen-y &key Type) "
  Return a LUI window at screen position x, y.
  If there is no window return nil
  If there are multiple windows return the topmost one"
  (multiple-value-bind (x y) (lui-screen-coordinate screen-x screen-y)
    (let ((Lui-Windows nil)
          (All-Windows (#/windows (#/sharedApplication ns::ns-application))))
      (dotimes (i (#/count All-Windows) (first (sort Lui-Windows #'< :key #'(lambda (w) (ordered-window-index (native-window w))))))
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
  ;; catch errors to avoid total crash of CCL
  (handler-case (invoke-action (lui-control Self))
     (error (condition) (format t "~%invoke control action error ~%  control: ~A ~%  error: ~A" (lui-control Self) Condition))))


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
      ;; Until Cocotron issue 366 is fixed, don't set the bezel style
      ;; as it causes the button to be invisible
      #-cocotron (#/setBezelStyle: Native-Control #$NSRoundedBezelStyle)
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

#|

;__________________________________
; Tab-View                         |
;__________________________________/



(defclass native-tab-view (ns:ns-tab-view)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))

(defmethod make-native-object ((Self tab-view-control))
  (let ((Native-Control (make-instance 'native-tab-view :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame)

  
      )
    Native-Control))


(defmethod ADD-TAB-VIEW-ITEM ((Self tab-view-control) text)
  (let ((tabViewItem (make-instance 'ns:ns-tab-view-item
                       :with-identifier (native-string text))))
    (#/setLabel: tabViewItem (Native-String text))
    (#/addTabViewItem: (native-view self) tabViewItem)))

(defmethod initialize-event-handling ((Self tab-view-control))
  (declare (ignore self)))

(defmethod MAP-SUBVIEWS ((Self tab-view-control) Function &rest Args)
  (declare (ignore Function Args))
  ;; no Cocoa digging
  )

;__________________________________
; Tab-View-Item                    |
;__________________________________/

(defclass native-tab-view (ns:ns-tab-view-item)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))

(defmethod make-native-object ((Self tab-view-item-control))
  (let ((Native-Control (make-instance 'ns:ns-tab-view-item
                       :with-identifier (native-string (text self)))))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
     ; (#/initWithFrame: Native-Control Frame)
#|      
(let ((tab-view (make-instance 'ns:ns-view)))
        (#/initWithFrame: tab-view Frame)
        (#/setView: Native-Control tab-view)
      )
|#
    Native-Control)))

(defmethod initialize-event-handling ((Self tab-view-item-control))
  (declare (ignore self)))

(defmethod ADD-TAB-VIEW-ITEM-VIEW ((Self tab-view-item-control) view)
  (#/setView: (Native-View self) (native-view view)))

|#

;__________________________________
; CHECKBOX BUTTON                 |
;__________________________________/


(defclass native-checkbox (ns:ns-button)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod make-native-object ((Self checkbox-control))
  (let ((Native-Control (make-instance 'native-checkbox :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame)
      (#/setButtonType: Native-Control #$NSSwitchButton)
      (#/setImagePosition: Native-Control #$NSImageLeft)
      (if (start-checked self)
        (#/setState: Native-Control #$NSOnState))
      (if (image-on-right self)
        (#/setImagePosition: Native-Control #$NSImageRight)
        (#/setImagePosition: Native-Control #$NSImageLeft))
      ;(#/setBezelStyle: Native-Control #$NSRoundedBezelStyle)
      (#/setTitle: Native-Control (native-string (text Self))))
    Native-Control))


(defmethod VALUE ((self checkbox-control))
  (if (eql (#/state (Native-View self)) #$NSOnState)
    't
    nil))

(defmethod (setf text) :after (Text (Self checkbox-control))
  (#/setTitle: (native-view Self) (native-string Text)))


;__________________________________
;  Image Button                    |
;__________________________________/


(defclass native-button-image (ns:ns-button)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod make-native-object ((Self image-button-control))
  (let ((Native-Control (make-instance 'native-button-image :lui-view Self)))
    (let ((NS-Image (#/alloc ns:ns-image)))
      (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
        (let ((Path (native-path "lui:resources;buttons;" (image Self))))
          (unless (probe-file Path) (error "no such image file for button ~A" (image Self)))
          (#/initWithContentsOfFile: NS-Image  (native-string (native-path "lui:resources;buttons;" (image Self))))
          (#/initWithFrame: Native-Control Frame)
          (#/setButtonType: Native-Control #$NSOnOffButton)   ;;;NSMomentaryPushInButton)
          (#/setImagePosition: Native-Control #$NSImageOnly)
          (#/setImage: Native-Control NS-Image)
          (#/setBezelStyle: Native-Control #$NSShadowlessSquareBezelStyle)
          (#/setTitle: Native-Control (native-string (text Self)))))
      Native-Control)))


(defmethod (setf text) :after (Text (Self image-button-control))
  (#/setTitle: (native-view Self) (native-string Text)))


(defmethod SET-BUTTON-OFF ((Self image-button-control))
  (#/setState: (native-view self) #$NSOffState))


(defmethod SET-BUTTON-ON ((Self image-button-control))
  (#/setState: (native-view self) #$NSOnState))

;__________________________________
; RADIO BUTTON                     |
;__________________________________/


(defclass native-radio-button (ns:ns-matrix)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod make-native-object ((Self radio-button-control))
  (let ((Native-Control (make-instance 'native-radio-button :lui-view Self))
        (prototype (#/init (#/alloc ns:ns-button-cell))))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/setTitle: prototype (native-string "Option"))
      (#/setButtonType: prototype #$NSRadioButton)
      (#/initWithFrame:mode:prototype:numberOfRows:numberOfColumns: Native-Control Frame #$NSRadioModeMatrix prototype 3 1)
      (let ((cells (#/cells Native-Control))
            (cell (#/init (#/alloc ns:ns-button-cell))))
        (#/setTitle: cell (native-string "options3"))
        (#/setButtonType: cell #$NSRadioButton)       
        (#/setTitle: (#/objectAtIndex: cells '0) #@"Option1")
        (#/putCell:atRow:column: Native-Control cell '1 '0)
        ))
    Native-Control))


(defmethod (setf text) :after (Text (Self radio-button-control))
  (#/setTitle: (native-view Self) (native-string Text)))


#||
(defmethod INITIALIZE-INSTANCE :after ((Self radio-button-control) &rest Args)
  (declare (ignore Args))
  (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
    (let ((prototype (#/init (#/alloc ns:ns-button-cell))))
      (#/initWithFrame:mode:prototype:numberOfRows:numberOfColumns: (Native-Control Self) Frame #$NSRadioModeMatrix prototype '4 '1))))
||#


(defmethod GET-SELECTED-ACTION ((Self radio-button-control))
  (elt (actions self)  (#/indexOfObject: (elements Self) (#/selectedCell (native-view self)))))


(defmethod FINALIZE-CLUSTER ((Self radio-button-control))
  (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
    (let ((prototype (#/init (#/alloc ns:ns-button-cell))))
      (#/setTitle: prototype (native-string "Option"))
      (#/setButtonType: prototype #$NSRadioButton)
      (#/initWithFrame:mode:prototype:numberOfRows:numberOfColumns: (Native-View Self) Frame #$NSRadioModeMatrix prototype (#/count (elements Self)) 1)
      (dotimes (i (#/count (elements Self)))
        (let ((element (#/objectAtIndex: (elements Self) i)))
          (#/putCell:atRow:column: (Native-View Self) element i 0))))))


(defmethod ADD-ITEM ((Self radio-button-control) text action)
  (let ((item (#/init (#/alloc ns:ns-button-cell))))
    (#/setTitle: item (native-string text))
    (#/setButtonType: item  #$NSRadioButton)   
    (setf (actions Self) (append (actions Self) (list action)))
    (setf (elements Self) (#/arrayByAddingObject: (elements Self) item))))

;__________________________________
; Popup Button                     |
;__________________________________/

(defclass native-popup-button (ns:ns-pop-up-button)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod make-native-object ((Self popup-button-control))
  (let ((Native-Control (make-instance 'native-popup-button :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame:pullsDown: Native-Control Frame #$NO)
    Native-Control)))


(defmethod VALUE ((self popup-button-control))
  (#/title (#/selectedItem (native-view self))))


(defmethod GET-SELECTED-ACTION ((Self popup-button-control))
  (elt (actions self)  (#/indexOfSelectedItem (native-view self))))


(defmethod SET-SELECTED-ITEM-WITH-TITLE ((Self popup-button-control) text)
  (#/selectItemWithTitle: (native-view self) (native-string text)))

(defmethod ADD-ITEM ((Self popup-button-control) Text Action)
  (if (equal (#/indexOfItemWithTitle: (native-view Self) (native-string Text)) -1)
    (progn
      (#/addItemWithTitle: (native-view Self) (native-string Text))
      (setf (actions Self) (append (actions Self) (list Action))))
    (warn "Cannot add item with the same title (~S)" Text)))


;__________________________________
; Choice Button                   |
;__________________________________/

(defclass native-choice-button (ns:ns-pop-up-button)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod make-native-object ((Self choice-button-control))
  (let ((Native-Control (make-instance 'native-choice-button :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame:pullsDown: Native-Control Frame #$NO)
    Native-Control)))


(defmethod VALUE ((self choice-button-control))
  (ccl::lisp-string-from-nsstring(#/title (#/selectedItem (native-view self)))))


(defmethod GET-SELECTED-ACTION ((Self choice-button-control))
  (elt (actions self)  (#/indexOfSelectedItem (native-view self))))



(defmethod ADD-MENU-ITEM ((Self choice-button-control) text action Image-pathname)
  (#/addItemWithTitle: (native-view Self) (native-string text))
  (unless (equal image-pathname nil)
    (let ((image (#/alloc ns:ns-image)))
          (#/initWithContentsOfFile: image (native-string (native-path "lui:resources;" image-pathname)))
          (#/setImage: (#/itemWithTitle: (native-view Self) (native-string text)) Image)))
  (setf (actions Self) (append (actions Self) (list Action))))

;__________________________________
; Seperator                        |
;__________________________________/

(defclass native-seperator (ns:ns-box)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod make-native-object ((Self seperator-control))
  (let ((Native-Control (make-instance 'native-seperator :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame)
      (#/setBorderType: Native-Control #$NSLineBorder)
      (#/setBoxType: Native-Control #$NSBoxSeparator)
      Native-Control)))


(defmethod (setf text) :after (Text (Self seperator-control))
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
      #-cocotron (#/setTitle: Native-Control (native-string (text Self)))  ;; depreciated: use separate label
      ;; Make sure the slider's indicator/thumb is positioned properly based on
      ;; control's initial value
      (#/setFloatValue: Native-Control (slot-value Self 'value)))
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
; STATUS BAR                   |
;__________________________________/

(defclass native-status-bar (ns:ns-text-field)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod make-native-object ((Self status-bar-control))
  (let ((Native-Control (make-instance 'native-status-bar :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame)
      (#/setDrawsBackground: Native-Control nil)
      (#/setEditable: Native-Control #$NO)
      (#/setStringValue: Native-Control (native-string (text Self)))
      #| (ecase (align Self)
        (:left (#/alignLeft: Native-Control Native-Control))
        (:center (#/alignCenter: Native-Control Native-Control))
        (:right (#/alignRight: Native-Control Native-Control))
        (:justified (#/alignJustified: Native-Control Native-Control))) |# )  
    Native-Control))


(defmethod (setf text) :after (Text (Self status-bar-control))
  (#/setStringValue: (native-view Self) (native-string Text)))


(defmethod VALUE ((Self status-bar-control))
  (ccl::lisp-string-from-nsstring 
   (#/stringValue (native-view Self))))

(defmethod (setf VALUE)  (Text (Self status-bar-control))
  (#/setStringValue: (native-view Self) (native-string Text)))


(defmethod MAP-SUBVIEWS ((Self status-bar-control) Function &rest Args)
  (declare (ignore Function Args))
  ;; no Cocoa digging
  )


(defmethod SUBVIEWS ((Self status-bar-control))
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
        (let ((Image #-cocotron (#/initByReferencingFile: (#/alloc ns:ns-image) (native-string (source Self)))
                     #+cocotron (#/initWithContentsOfFile: (#/alloc ns:ns-image) (native-string (source Self)))))
          (unless #-cocotron (#/isValid Image)
                  #+cocotron (not (ccl:%null-ptr-p Image))
            (error "cannot create image from file ~S" (source Self)))
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
; Color Well                       |
;__________________________________/


(defclass NATIVE-COLOR-WELL (ns:ns-color-well)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod MAKE-NATIVE-OBJECT ((Self color-well-control))
  (let ((Native-Control (make-instance 'native-color-well :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (multiple-value-bind (Red Blue Green)
                           (parse-rgb-from-hex self (color self))
        ;(setf (color self) (concatenate (write-to-string Red) (write-to-string 
        (let ((nscolor (#/colorWithDeviceRed:green:blue:alpha: ns:ns-color (/ red 255.0) (/ blue 255.0) (/ green 255.0)  1.0 )))                
          (#/initWithFrame: Native-Control Frame)
          (#/setColor: Native-Control nscolor)))
      ;; setup alpha control but keep in mind color panel is shared -> cannot mix alpha / no alpha
      (#/setShowsAlpha: (#/sharedColorPanel ns:ns-color-panel) (if (show-alpha Self) #$YES #$NO))
      Native-Control)))


(defmethod PARSE-RGB-FROM-HEX ((Self color-well-control) string)
  (values
   (read-from-string (concatenate 'string "#x" (subseq string 0 2)))   ;Red
   (read-from-string (concatenate 'string "#x" (subseq string 2 4)))   ;Blue
   (read-from-string (concatenate 'string "#x" (subseq string 4 6))))) ;Green


(defmethod GET-RED ((self color-well-control))
  (rlet ((r #>CGFloat)
         (g #>CGFloat)
         (b #>CGFloat)
         (a #>CGFloat))
    (#/getRed:green:blue:alpha: (#/color (Native-View self)) r g b a)
    (truncate (* (pref r #>CGFloat) 255))))


(defmethod GET-GREEN ((self color-well-control))
  (rlet ((r #>CGFloat)
         (g #>CGFloat)
         (b #>CGFloat)
         (a #>CGFloat))
    (#/getRed:green:blue:alpha: (#/color (Native-View self)) r g b a)
    (truncate (* (pref g #>CGFloat) 255))))


(defmethod GET-BLUE ((self color-well-control))
  (rlet ((r #>CGFloat)
         (g #>CGFloat)
         (b #>CGFloat)
         (a #>CGFloat))
    (#/getRed:green:blue:alpha: (#/color (Native-View self)) r g b a)
    (truncate (* (pref b #>CGFloat) 255))))


(defmethod GET-ALPHA ((self color-well-control))
  (rlet ((r #>CGFloat)
         (g #>CGFloat)
         (b #>CGFloat)
         (a #>CGFloat))
    (#/getRed:green:blue:alpha: (#/color (Native-View self)) r g b a)
    (truncate (* (pref a #>CGFloat) 255))))


(defmethod SET-COLOR ((Self color-well-control) &key (Red 0.0) (Green 0.0) (Blue 0.0) (Alpha 1.0))
  ;; keep a native color instead of creating a new one for each display
  (#/setColor: (native-view Self) (#/retain (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color Red Green Blue Alpha))))

;__________________________________
; Web Browser                      |
;__________________________________/

#-cocotron
(eval-when (:compile-toplevel :load-toplevel :execute)
  (objc:load-framework "WebKit" :webkit))


#-cocotron
(progn
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

(export '(load-url))

(defmethod LOAD-URL ((Self web-browser-control) url)
  (let* ((webframe (#/mainFrame (Native-View Self)))
         (request (#/requestWithURL:
                   ns:ns-url-request
                   (ccl::with-autorelease-pool
                       (#/retain (#/URLWithString: ns:ns-url (ccl::%make-nsstring (string url ))))))))
    ;; Failing to wait until the main thread has
    ;; initiated the request seems to cause
    ;; view-locking errors.  Maybe that's just
    ;; an artifact of some other problem.
    (#/performSelectorOnMainThread:withObject:waitUntilDone:
     webframe (ccl::@selector #/loadRequest:) request t)
    ))
  
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
)

;__________________________________
; Show PopUp                       |
;__________________________________/
;(export '(show-string-popup))


(defun show-string-popup (window list)
    (let ((Pop-up (make-instance 'popup-button-control )))
      (dolist (String list)
        (add-item Pop-Up String nil))
      (add-subviews window Pop-up)
      (#/setTransparent: (native-view Pop-Up) #$YES)
      (#/performClick:  (native-view Pop-up) +null-ptr+)
      (#/removeFromSuperview (native-view Pop-up))
      (ccl::lisp-string-from-nsstring  (#/titleOfSelectedItem (native-view Pop-Up)))
      ))
 


