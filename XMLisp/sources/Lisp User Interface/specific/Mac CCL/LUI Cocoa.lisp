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
;; Missing Header Hacks           *
;;*********************************

;; remove these Gesture events once the OS X 10.6 headers have been integrated into CCL

(defconstant NSEventTypeGesture 29)
(defconstant NSEventTypeMagnify 30)
(defconstant NSEventTypeSwipe 31)
(defconstant NSEventTypeRotate 18)
(defconstant NSEventTypeBeginGesture 19)
(defconstant NSEventTypeEndGesture 20)


;;*********************************
;; Native -> LUI name converters  *
;;*********************************

(defmethod NATIVE-TO-LUI-EVENT-TYPE (NS-Type)
  (case NS-Type
    ;; mouse 
    (#.#$NSLeftMouseDown :left-mouse-down)
    (#.#$NSLeftMouseUp :left-mouse-up)
    (#.#$NSRightMouseDown :right-mouse-down)
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
    ;; scroll wheel
    (#.#$NSScrollWheel :scroll-wheel)
    ;; Gesture 
    #-cocotron (#.nseventtypebegingesture :begin-gesture)
    #-cocotron (#.nseventtypemagnify :magnify-gesture)
    #-cocotron (#.nseventtypeswipe :swipe-gesture)
    #-cocotron (#.nseventtyperotate :rotate-gesture)
    #-cocotron (#.nseventtypeendgesture :end-gesture)
    ;; trouble!!
    (t  :undefined-event)))

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


;**********************************
;* EVENT                          *
;**********************************

(defmethod COMMAND-KEY-P ()
  (let ((current-event (#/currentEvent (#/sharedApplication ns:ns-application))))
    (unless (%null-ptr-p current-event)
      (not (zerop (logand (#/modifierFlags current-event) #$NSCommandKeyMask))))))


(defmethod ALT-KEY-P ()
  (let ((current-event (#/currentEvent (#/sharedApplication ns:ns-application))))
    (unless (%null-ptr-p current-event)
      (not (zerop (logand (#/modifierFlags current-event) #$NSAlternateKeyMask))))))


(defmethod SHIFT-KEY-P ()
  (let ((current-event (#/currentEvent (#/sharedApplication ns:ns-application))))
    (unless (%null-ptr-p current-event)
      (not (zerop (logand (#/modifierFlags current-event) #$NSShiftKeyMask))))))
    

(defmethod CONTROL-KEY-P ()
  (let ((current-event (#/currentEvent (#/sharedApplication ns:ns-application))))
    (unless (%null-ptr-p current-event)
      (not (zerop (logand (#/modifierFlags current-event) #$NSControlKeyMask))))))


(defmethod DOUBLE-CLICK-P ()
  (when *Current-Event* 
    (= (#/clickCount (native-event *Current-Event*)) 2)))


(defmethod GET-MODIFIER-FLAGS ((Self event))
  (#/modifierFlags (native-event self)))


(defmethod GET-CHARACTERS ((Self event))
  (#/characters (native-event self)))


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
  ;; the subview will now be retained by the view so we can release the subview.
  ;; This will be a problem if we call add-subview on an object we don't own.  I.E. if the subview was not created using alloc, new..., copy..., mutableCopy... 
  ;; then this release will likely cause a crash
  (#/release (native-view Subview))
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
  (setf (part-of New-Subview) View)
  ;; set size again to give views such as opengl views a chance to reestablish themselves
  (set-size New-Subview (width Old-Subview) (height Old-Subview)))
  


(defmethod MAP-SUBVIEWS ((Self subview-manager-interface) Function &rest Args)
  (ccl::with-autorelease-pool
    (let ((Subviews (#/subviews (native-view Self))))
      (dotimes (i (#/count Subviews))
        (apply Function (lui-view (#/objectAtIndex: Subviews i)) Args)))))


(defmethod SUBVIEWS ((Self subview-manager-interface))
  (ccl::with-autorelease-pool
    (when (native-view Self)              ;Allows tracing MAKE-NATIVE-OBJECT
      (let* ((Subviews (#/subviews (native-view Self)))
             (Count (#/count Subviews))
             (Subview-List nil))
        (dotimes (i Count Subview-List)
          ;; If the subview does not have a slot matching lui-view it will trigger an error, so ignore that subview and don't push it.  
          (when (slot-exists-p (#/objectAtIndex: Subviews (- Count 1 i)) 'lui-view)
            (push (lui-view (#/objectAtIndex: Subviews (- Count 1 i))) Subview-List)))))))


(defmethod SUPERVIEW ((Self subview-manager-interface))
  (let ((Superview (#/superview (native-view Self))))
    (and (not (%null-ptr-p Superview))
         (slot-exists-p Superview 'lui-view)
         (lui-view Superview))))

;**********************************
;* Control                        *
;**********************************

(defmethod DISABLE ((Self control))
  (#/setEnabled: (native-view self) #$NO))

(defmethod ENABLE ((Self control))
  (#/setEnabled: (native-view self) #$YES))

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
  (ccl::with-autorelease-pool
    (ns:with-ns-point (Point X Y)
      (#/setFrameOrigin: (native-view Self) Point))))


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
  (subviews-swapped (window View) Old-Subview New-Subview))


(defmethod WINDOW ((Self view))
  (unless (native-view Self)
    (return-from window nil))
  (ccl::with-autorelease-pool
      (let ((ns-window (#/window (native-view Self))))
        (if (%null-ptr-p ns-window)
          (return-from WINDOW nil)
          (lui-window ns-window)))))


(defvar *View-Full-Screen-Restore-Sizes* (make-hash-table))


(defmethod ENTER-FULL-SCREEN-MODE ((Self view))
  (ccl::with-autorelease-pool
    (setf (full-screen-p self) t)
    (setf (gethash Self *view-Full-Screen-Restore-Sizes*) (frame self))
    ;; switching can be messy: prevent screen updates
    (#_NSDisableScreenUpdates)
    (hide (window self))
    (switch-to-full-screen-mode (window self))
    (hide-all-other-views (window self) self)
    (#_NSEnableScreenUpdates)
    ;(set-position self 0 0)
    ;(set-size self (width (window self)) (height (window self)))
    (show (window self))
    (display (window self))))


(defmethod EXIT-FULL-SCREEN-MODE ((Self view))
  (ccl::with-autorelease-pool
    (setf (full-screen-p self) nil)
    (switch-to-window-mode (window self))
    (let ((Frame (gethash Self *view-Full-Screen-Restore-Sizes*)))
      (set-frame-with-frame self frame))
    (show-all-views (window self))
    (display self)
    (display (window self))))


(defmethod RECURSIVELY-MAXIMIZE-SUPERVIEWS ((self view))
  (set-position self 0 0)
  (set-size self (width (window self)) (height (window self)))
  (when (and (superview self) (not (equal (type-of (native-view (superview self))) 'native-window-view)))
    (type-of (native-view (superview self)))
    (recursively-maximize-superviews (superview self))))


;;RENAME ALL THESE METHODS BELOW!!!
;; Move to window code, its conveniant to have this here for now but it belong with window methods
(defmethod HIDE-ALL-OTHER-VIEWS ((self window) view-not-to-hide)
  "Will hide all of the views in the window except for view and its superviews and subviews"
  (dolist (subview (subviews self))
    (hide-views-recursively subview :view-not-to-hide view-not-to-hide)))


(defmethod SHOW-ALL-VIEWS ((self window))
  "Show all hidden views"
  (dolist (subview (subviews self))
    (show-views-recursively subview)))


(defmethod HIDE-VIEWS-RECURSIVELY  ((Self view) &key (view-not-to-hide nil))
  (dolist (subview (subviews self))
    (hide-views-recursively subview :view-not-to-hide view-not-to-hide))
  (let ((subview-list (find-subview self view-not-to-hide)))
    (when (and view-not-to-hide subview-list)
      (dolist (subview (reverse subview-list))
        (set-position subview 0 0)
        (set-size subview  (width (window self)) (height (window self))))
      (return-from hide-views-recursively t)))
  (hide self))


(defmethod SHOW-VIEWS-RECURSIVELY  ((Self view))
  (dolist (subview (subviews self))
    (show-views-recursively subview))
  (show self))


(defmethod FIND-SUBVIEW ((self view) view-to-find &key (subview-list nil)) 
  "Returns true if view-to-find is anywhere in the subview hiearchy of view"
  (when (Equal view-to-find self)
    (setf subview-list (append subview-list (list self)))
    (return-from find-subview subview-list))
  (dolist (subview (subviews self))
    (let ((return-val  (find-subview subview view-to-find)))
      (when return-val
        (unless (equal subview view-to-find)
          (setf return-val (append return-val (list subview))))
        (return-from find-subview return-val)))))


(defmethod SHOW ((self view))
  (setf (hidden-p self) nil)
  (#/setHidden: (native-view self) #$NO))


(defmethod HIDE ((self view))
  (setf (hidden-p self) t)
  (#/setHidden: (native-view self) #$YES))


(defmethod DISPLAY ((Self view))
  ;; will not work without flushing window
  (#/setNeedsDisplay: (native-view Self) #$YES))


(defmethod ADD-TRACKING-RECT ((Self view))
  (#/addTrackingRect:owner:userData:assumeInside: 
   (native-view self)
   (#/frame (native-view self))
   (native-view self)
   +null-ptr+
   #$NO))

(defmethod REMOVE-TRACKING-RECT ((Self view) Tracking-Rect)
  (#/removeTrackingRect: (native-view self) Tracking-Rect))
  

(defclass TOOLTIP-DELEGATE (ns:ns-object)
  ((lui-view :accessor lui-view :initform nil :initarg :lui-view))
  (:metaclass ns:+ns-object
	      :documentation "delegate object receiving window events"))


(objc:defmethod (#/description :id) ((self tooltip-delegate) )
  (native-string (get-tooltip-of-most-specific-view-at-screen-position (lui-view Self) (ns:ns-point-x (#/mouseLocation ns:ns-event )) (ns:ns-point-y (#/mouseLocation ns:ns-event )))))


(defmethod ENABLE-TOOLTIPS ((Self view))
  (when  (get-tooltip self (x self) (y self))
    (#/removeAllToolTips (native-view self))
    (#/addToolTipRect:owner:userData: (native-view self) (#/frame (native-view self)) (make-instance 'tooltip-delegate :lui-view self) lui::+null-ptr+)))


(defmethod CONVERT-FROM-WINDOW-COORDINATES-TO-VIEW-COORDINATES ((Self view) x y)
  (ns:with-ns-point (point x y)
    (let ((convertedPoint (#/convertPoint:fromView: (native-view self) point nil)))
      (values
       (ns:ns-point-x convertedPoint)
       (ns:ns-point-y convertedPoint)))))


(defmethod GRAB-VIEW-LOCK ((Self view))
  (unless (lock Self)
    (setf (lock Self) (ccl::make-lock "view")))
  (ccl::grab-lock (lock Self)))


(defmethod RELEASE-VIEW-LOCK ((Self view))
  (unless (lock Self) (error "cannot release non existing lock"))
  (ccl::release-lock (lock Self)))

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


(defmethod MAKE-NATIVE-OBJECT ((Self scroll-view))
  (let ((Native-Control (make-instance 'native-scroll-view :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame)
      (#/setHasHorizontalScroller: Native-Control (has-horizontal-scroller Self))
      (#/setHasVerticalScroller: Native-Control (has-vertical-scroller Self))
      #-cocotron (#/setAutohidesScrollers: Native-Control #$YES)
      (#/setBorderType: Native-Control #$NSNoBorder)  ;;; #$NSLineBorder)
      (#/setDrawsBackground: Native-Control #$NO)
    Native-Control)))


(defmethod SET-COLOR ((Self scroll-view) &key (Red 0.0) (Green 0.0) (Blue 0.0) (Alpha 1.0))
  ;; keep a native color instead of creating a new one for each display
  (when (native-color Self) (#/release (native-color Self)))
  (setf (native-color Self) (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color Red Green Blue Alpha))
  (#/retain (native-color Self))
  (#/setDrawsBackground: (native-view self) #$YES)
  (#/setBackgroundColor: (native-view self) (native-color self)))


(defmethod ADD-SUBVIEWS ((view scroll-view)  &rest Subviews)
  (call-next-method)
  ;; it only really makes sense to have one subview
  (#/setDocumentView: (native-view View) (native-view (first Subviews)))
  ;(warn "You are adding multiple views to a scroll-view. Only the first one will be visible.")
  )


(defmethod GET-SCROLLABLE-CONTENTS ((self scroll-view))
  (unless (%null-ptr-p (#/documentView (native-view self)))  
    (lui-view (#/documentView (native-view self)))))


(defmethod GET-TOOLTIP ((Self scroll-view) x y)
  (let ((subview (get-scrollable-contents self)))
    (if (and subview (get-tooltip subview x y))
      (get-tooltip subview x y)
      (call-next-method))))


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
;* SCROLL-VIEW-ADJUSTING-CONTENTS *
;**********************************

(defmethod SET-SIZE ((Self scroll-view-adjusting-contents) W H)
  (declare (ignore W H))
  (call-next-method)
  ;; set width of subviews to be the width of the scroll-view minus the vertical scroller width, if present
  (map-subviews Self #'(lambda (View) (set-size View 
                                                (ns::ns-size-width (#/contentSize (native-view Self)))
                                                (height View)))))



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
    (#/fillRect: ns:ns-bezier-path Frame)))


;**********************************
;* COLOR-PALETTE-VIEW             *
;**********************************

(defmethod DRAW ((Self color-palette-view))
  (if (draw-transparency-triangles self)
    (let ((triangle (#/bezierPath ns:ns-bezier-path))(triangle2 (#/bezierPath ns:ns-bezier-path)))
      (ns:with-ns-point (point 0 0)
        (ns:with-ns-point (point1 (width Self) 0 )
          (ns:with-ns-point (point2 0 (height Self))
            (ns:with-ns-point (point3 (width Self) (height Self))
              (when (native-color Self)
                (#/set (opaque-native-color Self)))
              (#/moveToPoint: triangle point)
              (#/lineToPoint: triangle point1)
              (#/lineToPoint: triangle point2)
              (#/lineToPoint: triangle point)
              (#/fill triangle)
              (#/moveToPoint: triangle2 point1)
              (#/lineToPoint: triangle2 point2)
              (#/lineToPoint: triangle2 point3)
              (#/lineToPoint: triangle2 point1)
              (when (native-color Self)
                (#/set (native-color Self)))
              (#/fill triangle2))))))
    (call-next-method))
  (#/set (#/colorWithDeviceRed:green:blue:alpha: ns:ns-color 0.0 0.0 0.0 1.0))
  (#/strokeRect: ns:ns-bezier-path (#/frame (native-view self))))


(defmethod SET-COLOR ((Self color-palette-view) &key (Red 0.0) (Green 0.0) (Blue 0.0) (Alpha 1.0))
  (if (>=  alpha 1.0 )
    (setf (draw-transparency-triangles self) nil)
    (setf (draw-transparency-triangles self) t))
  (when (native-color Self) (#/release (native-color Self)))
  (setf (native-color Self) (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color Red Green Blue Alpha))
  (#/retain (native-color Self))
  (setf (opaque-native-color Self) (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color Red Green Blue 1.0))
  (#/retain (opaque-native-color Self)))

;;************************************
;; WINDOW                            *
;;************************************

(defmethod WINDOW-CLOSE ((Self Window))
  (when (window-should-close self)
    (window-will-close self nil)
    (#/close (native-window Self))
    (return-from window-close t))
  nil)


(defmethod END-TEXT-EDITTING ((Self Window))
  ;; If any text fields in this window are in the process of editting, end editting and resign first responder
  (#/endEditingFor: (native-window self) nil ))

  
(defmethod CLOSE-WITH-NO-QUESTIONS-ASKED ((Self Window))
  (window-will-close self nil)
  (#/close (native-window Self)))


(defmethod SET-COLOR ((Self Window) &key (Red 1.0) (Green 1.0) (Blue 1.0) (Alpha 1.0))
  (#/setBackgroundColor:
   (native-window Self)
   (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color Red Green Blue Alpha)))

;__________________________________
; NATIVE-WINDOW                     |
;__________________________________/

(defclass NATIVE-WINDOW (ns:ns-window)
  ((lui-window :accessor lui-window :initarg :lui-window)
   (delegate :accessor delegate :initform nil :initarg :delegate :documentation "event delegate")   
   #+cocotron (show-main-menu-on-windows :accessor show-main-menu-on-windows :initform nil :initarg :show-main-menu-on-windows :documentation "Should this window show a main menu on Windows?"))
  (:metaclass ns:+ns-object
	      :documentation "Native window"))


(objc:defmethod (#/zoom: :void) ((self native-window) sender)
  (call-next-method sender)
  (let ((Content-View (#/contentView (native-window (lui-window Self)))))
    (setf (width (lui-window Self)) (truncate (pref (#/frame Content-View) <NSR>ect.size.width)))
    (setf (height (lui-window Self)) (truncate (pref (#/frame Content-View) <NSR>ect.size.height)))
    (let ((Window (lui-window Self)))
      (setf (x Window) (truncate (pref (#/frame (native-window (lui-window Self))) <NSR>ect.origin.x)))
      (setf (y Window) 
            (- (screen-height (lui-window Self)) 
               (height (lui-window Self))
               (truncate (pref (#/frame (native-window (lui-window Self))) <NSR>ect.origin.y)))))    (screen-height nil)
    (size-changed-event-handler (lui-window Self) (width (lui-window Self)) (height (lui-window Self)))))


(objc:defmethod (#/close :void) ((self native-window))
  (if (is-modal-p (lui-window self))
    (cancel-modal (lui-window self))
    (call-next-method)))
  

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


(objc:defmethod (#/keyDown: :void) ((self native-window) event)
  (call-next-method event)
  (view-event-handler (lui-window Self)
                      (make-instance 'key-event 
                        :key-code (#/keyCode Event)
                        :event-type (native-to-lui-event-type (#/type event))
                        :native-event Event)))


(objc:defmethod (#/becomeMainWindow :void) ((self native-window))
  (call-next-method)
  (has-become-main-window (lui-window Self)))


#-cocotron
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


#+cocotron
(objc:defmethod (#/hasMainMenu :<BOOL>) ((self native-window))
  #$YES)



;;RESPONDER CHAIN HACK
(objc:defmethod (#/noResponderFor: :void)
                ((self native-window) (eventSelector :<SEL>))
  ;(declare (ignore eventSelector))
  ;; For now, if we get to the bottom of the responder chain and no one has taken responsibility for the event just do nothing
  ;;Do nothing
  )


;__________________________________
; Window-delegate                   |
;__________________________________/

(defclass WINDOW-DELEGATE (ns:ns-object)
  ((lui-window :accessor lui-window :initarg :lui-window))
  (:metaclass ns:+ns-object
	      :documentation "delegate object receiving window events"))


(objc:defmethod (#/windowDidResignKey: :void) ((self window-delegate) Notification)
  (declare (ignore Notification))
  (window-lost-focus (lui-window Self)))


(objc:defmethod (#/windowDidResize: :void) ((self window-delegate) Notification)
  (declare (ignore Notification))
  ;; only the size of the content view
  (let ((Content-View (#/contentView (native-window (lui-window Self)))))
    (setf (width (lui-window Self)) (truncate (pref (#/frame Content-View) <NSR>ect.size.width)))
    (setf (height (lui-window Self)) (truncate (pref (#/frame Content-View) <NSR>ect.size.height)))
    (let ((Window (lui-window Self)))
      (setf (x Window) (truncate (pref (#/frame (native-window (lui-window Self))) <NSR>ect.origin.x)))
      (setf (y Window) 
            (- (screen-height (lui-window Self)) 
               (height (lui-window Self))
               (truncate (pref (#/frame (native-window (lui-window Self))) <NSR>ect.origin.y)))))    
    ;; On window we are ging to save this resize for the end
    #-cocotron
    (size-changed-event-handler (lui-window Self) (width (lui-window Self)) (height (lui-window Self)))))


(objc:defmethod (#/windowShouldClose: :<BOOL>) ((self window-delegate) Sender)
  (declare (ignore sender))
  ;; Hack:  For now call the window-will-close code inside a positive response from window-should-close, this is needed for now because calling subviews (which is caused when we call window-will-close) 
  ;; Sometimes causes problems if the window is already in the process of being closed.  
  (window-close (lui-window self))
  #$NO)
  

(objc:defmethod (#/windowDidMove: :void) ((self window-delegate) Notification)
  (declare (ignore Notification))  
  (let ((Window (lui-window Self)))
    (setf (x Window) (truncate (pref (#/frame (native-window (lui-window Self))) <NSR>ect.origin.x)))
    (setf (y Window) 
          (- (screen-height (lui-window Self)) 
             (height (lui-window Self))
             (truncate (pref (#/frame (native-window (lui-window Self))) <NSR>ect.origin.y))))))


;__________________________________
; MENULESS-NATIVE-WINDOW           |
;__________________________________/


(defclass MENULESS-NATIVE-WINDOW (native-window)
  ()
  (:metaclass ns:+ns-object
	      :documentation "Native window with no main menu"))


#+cocotron
(objc:defmethod (#/hasMainMenu :<BOOL>) ((self menuless-native-window))
  #$NO)

;__________________________________
; window methods                   |
;__________________________________/

(defmethod MAKE-NATIVE-OBJECT ((Self window))
  (declare (ftype function window-controller))
  (in-main-thread ()
    (ccl::with-autorelease-pool
      (let ((Window (make-instance #-cocotron 'native-window #+cocotron (if (show-main-menu-on-windows self) 'native-window 'menuless-native-window )
                        :lui-window Self
                        #+cocotron :show-main-menu-on-windows #+cocotron (show-main-menu-on-windows self)
                        :with-content-rect (ns:make-ns-rect 0 0 (width Self) (height Self))
                        :style-mask (if (borderless Self)
                                      0
                                      (logior (if (title Self) #$NSTitledWindowMask 0)
                                              (if (closeable Self) #$NSClosableWindowMask 0)
                                              (if (resizable Self) #$NSResizableWindowMask 0)
                                              (if (minimizable Self) #$NSMiniaturizableWindowMask 0)))
                        :backing #$NSBackingStoreBuffered
                        :defer t)))
        (#/disableCursorRects Window) ;; HACK: http://www.mail-archive.com/cocoa-dev@lists.apple.com/msg27510.html
        (setf (native-window Self) Window)  ;; need to have this reference for the delegate to be in place
        (setf (native-view Self) (make-instance 'native-window-view :lui-window Self))
        ;; setup delegate
        (setf (delegate Window) (make-instance 'window-delegate :lui-window Self))
        (#/setDelegate: Window (delegate Window))
        ;; content view
        (#/setContentView: Window (#/autorelease (native-view Self)))
        (#/setTitle: Window (native-string (title Self)))
        (when (use-custom-window-controller self)
          (#/setWindowController: Window (window-controller)))
        (ns:with-ns-size (minSize (min-width self) (min-height self))
          (#/setMinSize: Window minSize))
        (ns:with-ns-size (Position (x Self) (- (screen-height Self)  (y Self)))
          (#/setFrameTopLeftPoint: (native-window Self) Position))
        (when (floating-p self)
          ;; #$kCGFloatingWindowLevelKey undefined on Cocotron so for now just use magic number 5 :(  
          (#/setLevel: Window #-cocotron #|#$kCGFloatingWindowLevelKey|#7 #+cocotron 5))
        ;; set background color
        (when (and (background-color Self) (= (length (background-color Self)) 8)) ;; we hope it's a string of hex numbers!
          (multiple-value-bind (r g b a)
                               (convert-hex-color-string-to-rgba-list (background-color Self))
            (set-color Self :red r :green g :blue b :alpha a)))
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
  (ns:with-ns-size (Position x (+ (title-bar-height self) (- (screen-height Self) y)))    
    (#/setFrameTopLeftPoint: (native-window Self) Position)))


(defmethod SHOW ((Self window))
  (in-main-thread ()
    ;; (let ((y (truncate (- (pref (#/frame (#/mainScreen ns:ns-screen)) <NSR>ect.size.height) (y Self) (height Self)))))
    ;;   (ns:with-ns-rect (Frame (x Self) y (width Self) (height Self))
    ;;   (#/setFrame:display: (native-window Self) Frame t)))
    (#/orderFront: (native-window Self) nil)
    (#/makeKeyWindow (native-window self))))


(defmethod CENTER ((Self window))
  (#/center (lui::native-window Self)))


(defmethod HIDE ((Self window))
  (in-main-thread ()
    (#/orderOut: (native-window Self) nil)))


(defmethod SCREEN-WIDTH ((Self window))
  (truncate (pref (#/frame (or (#/screen (native-window Self))
                               (#/mainScreen ns:ns-screen))) 
                  <NSR>ect.size.width)))


(defmethod SCREEN-HEIGHT ((Self window))
  (truncate (pref (#/frame (or (and (not (%null-ptr-p (#/screen (native-window Self)))) (#/screen (native-window Self)))
                               (#/mainScreen ns:ns-screen))) 
                  <NSR>ect.size.height)))


(defmethod VISIBLE-P ((Self window))
  (when (#/isVisible (native-window Self))
    t))


(defmethod SCREEN-HEIGHT ((Self null))
  (truncate (pref (#/frame (#/mainScreen ns:ns-screen))
                  <NSR>ect.size.height)))


(defmethod TITLE-BAR-HEIGHT ((Self window))
  (- (truncate (pref (#/frame (native-window self)) <NSR>ect.size.height)) (truncate (pref (#/frame (#/contentView (native-window self))) <NSR>ect.size.height))))


(defmethod (setf TITLE) :after (Title (self window))
  (when (visible-p self)
    (#/setTitle: (native-window Self) (native-string Title))))


(defmethod SET-DOCUMENT-EDITTED ((self window) &key (mark-as-editted t))
  (if mark-as-editted
    (when (not (#/isDocumentEdited (native-window self)))
      (#/setDocumentEdited: (native-window self) #$YES))
    (when (#/isDocumentEdited (native-window self))
      (#/setDocumentEdited: (native-window self) #$NO))))


(defvar *Run-Modal-Return-Value* nil "shared valued used to return the run modal values")

;; Modal windows

(defmethod SHOW-AND-RUN-MODAL ((Self window))
  (declare (special *Run-Modal-Return-Value*))
  (setq *Run-Modal-Return-Value* nil)
  (when (#/isVisible (native-window Self))
    (error "cannot run modal a window that is already visible"))
  (in-main-thread () (#/makeKeyAndOrderFront: (native-window self) (native-window self)))
  (before-going-modal self)
  (setf (is-modal-p self) t)
  (let ((Code (in-main-thread () 
                              (#/runModalForWindow: (#/sharedApplication ns:ns-application)
                                      (native-window Self)))))
    (declare (ignore Code))
    ;; ignore Code for now
    (in-main-thread () (#/close (native-window Self)))
    (case *Run-Modal-Return-Value*
      (:cancel  (throw :cancel nil))
      (t *Run-Modal-Return-Value*))))


(defmethod STOP-MODAL ((Self window) Return-Value)
  (setq *Run-Modal-Return-Value* Return-Value)  
  (#/stopModal (#/sharedApplication ns:ns-application)))


(defmethod STOP-MODAL :after ((Self window) Return-Value)
  (declare (ignore return-value))
  (setf (is-modal-p self) nil))


(defmethod STOP-MODAL ((Self t) Return-Value)
  ;; special stop-modal to stop without a window
  (setq *Run-Modal-Return-Value* Return-Value)
  (#/stopModal (#/sharedApplication ns:ns-application)))


(defmethod PAUSE-MODAL ((Self window))
  (#/stopModal (#/sharedApplication ns:ns-application)))


(defmethod RESUME-MODAL ((Self window))
  (#/stopModal (#/sharedApplication ns:ns-application)))


(defmethod CANCEL-MODAL ((Self window))
  (setq *Run-Modal-Return-Value* :cancel)
  (#/stopModal (#/sharedApplication ns:ns-application)))


(defmethod CANCEL-MODAL :after ((Self window) )
  (setf (is-modal-p self) nil))


(defmethod BEFORE-GOING-MODAL ((Self window))
  "Do any setup that needs to occur before the window becomes modal but after it is on the screen"
  ;; By default, do nothing
  )


(defmethod SET-SIZE-AND-POSITION-ENSURING-WINDOW-WILL-FIT-ON-SCREEN ((Self window) x y width height)
  "Will try and set the window to the given size but will make sure the window does not appear outside of the screen"
  (set-size self width height)
  (set-position self x y))


;; screen mode
(defvar *Window-Full-Screen-Restore-Sizes* (make-hash-table))


(defmethod SWITCH-TO-FULL-SCREEN-MODE ((Self window))
  (setf (gethash Self *Window-Full-Screen-Restore-Sizes*) (#/frame (native-window Self)))
  #-cocotron (#_SetSystemUIMode #$kUIModeAllSuppressed #$kUIOptionAutoShowMenuBar)
  #+cocotron 
  (set-size-and-position-ensuring-window-will-fit-on-screen
   self
   0
   (title-bar-height self)
   (pref (#/frame (#/screen (native-window Self))) <NSR>ect.size.width)
   (+ (pref (#/frame (#/screen (native-window Self))) <NSR>ect.size.height)))
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
      (#/setFrame:display:animate: (native-window Self) Frame #$YES #$NO)
      #+cocotron
      (size-changed-event-handler self (width self) (height self)))))


(defmethod MAKE-KEY-WINDOW ((Self window))
  (#/makeKeyWindow (native-window self)))


(defmethod BRING-TO-FRONT ((Self Window))
  (#/makeKeyWindow (native-window self))
  (#/orderFront: (native-window self) nil))


(defmethod GET-X-Y-OFFSET-FOR-WINDOW-ORIGIN ((Self Window))
  (let ((frame (#/frame (#/contentView (native-window self)))))
    (values
     (NS:NS-RECT-X frame)
     (NS:NS-RECT-Y frame))))

;__________________________________
; Window query functions            |
;__________________________________/


(defun ORDERED-WINDOW-INDEX (Window)
  #-cocotron
  (#/orderedIndex Window)
  #+cocotron
  (ccl::with-ns-exceptions-as-errors 
      (objc:objc-message-send window "orderedIndex" :int)))


(defun ORDERED-TEST ()
  (let ((window-array  (#/orderedWindows (#/sharedApplication ns::ns-application)))) 
    (dotimes (i (#/count window-array))
      (let ((array-window (#/objectAtIndex: window-array i)))    
        (#/title array-window)))))


(defun FIND-WINDOW-AT-SCREEN-POSITION (screen-x screen-y &key Type) "
  Return a LUI window at screen position x, y.
  If there is no window return nil
  If there are multiple windows return the topmost one"
  (ccl::with-autorelease-pool
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
                  (push (lui-window Window) Lui-Windows))))))))))


;__________________________________
; NATIVE-WINDOW-VIEW                |
;__________________________________/


(defclass native-window-view (ns:ns-view)
  ((lui-window :accessor lui-window :initarg :lui-window)
   (lui-view :accessor lui-view :initarg :lui-view))
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


(objc:defmethod (#/rightMouseDown: :void) ((self native-window-view) event)
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



(objc:defmethod (#/viewDidEndLiveResize :void) ((self native-window-view))
  (window-did-finish-resize (lui-window self))
  ;;On cocotron do the resize at the end
  #+cocotron
  (size-changed-event-handler (lui-window Self) (width (lui-window Self)) (height (lui-window Self)))
  (map-subviews (lui-window Self) #'(lambda (View) (view-did-end-resize View ))))


(defun GET-MOUSE-EVENT-DELTA-X-SAVELY (Event)
  ;; Apple has still (OS X 10.6.5) not provided public interfaces for these accessors: http://lists.apple.com/archives/cocoa-dev/2007/Feb/msg00050.html
  (catch :mouse-info-access-error
    (handler-bind
        ((condition #'(lambda (Condition)
                        (declare (ignore Condition))
                        ;; could be an NS-ERROR caused by the mouse not supporting the creation of an event with a deviceDeltaX
                        (throw :mouse-info-access-error (* (ccl::with-ns-exceptions-as-errors 
                                                               (objc:objc-message-send Event "deltaX" #>CGFloat)) 10)))))
      (ccl::with-ns-exceptions-as-errors 
          (objc:objc-message-send Event "deviceDeltaX" #>CGFloat)))))


(defun GET-MOUSE-EVENT-DELTA-Y-SAVELY (Event)
  ;; Apple has still (OS X 10.6.5) not provided public interfaces for these accessors: http://lists.apple.com/archives/cocoa-dev/2007/Feb/msg00050.html
  (catch :mouse-info-access-error
    (handler-bind
        ((condition #'(lambda (Condition)
                        (declare (ignore Condition))
                        ;; could be an NS-ERROR caused by the mouse not supporting the creation of an event with a deviceDeltaY
                        (throw :mouse-info-access-error (* (ccl::with-ns-exceptions-as-errors 
                                                               (objc:objc-message-send Event "deltaY" #>CGFloat)) 10)))))
      (ccl::with-ns-exceptions-as-errors 
          (objc:objc-message-send Event "deviceDeltaY" #>CGFloat)))))


(objc:defmethod (#/scrollWheel: :void) ((self native-window-view) Event)
  (let ((mouse-loc (#/locationInWindow event)))
    ;;(format t "~%dragged to ~A, ~A," (pref mouse-loc :<NSP>oint.x) (- (height (lui-window Self)) (pref mouse-loc :<NSP>oint.y)))
    (view-event-handler 
     (lui-window Self) 
     (make-instance 'mouse-event
       :x (truncate (pref mouse-loc :<NSP>oint.x))
       :y (truncate (- (height (lui-window Self)) (pref mouse-loc :<NSP>oint.y)))
       :dx (get-mouse-event-delta-x-savely Event)
       :dy (get-mouse-event-delta-y-savely Event)
       :event-type (native-to-lui-event-type (#/type event))
       :native-event Event))))


;; Gesture Events (OS X 10.6 and later)

(objc:defmethod (#/beginGestureWithEvent: :void) ((Self native-window-view) Event)
  ;; should be save at this event method should not even get called in non OS x and pre 10.6 environments
  (when (mac-os-x-10.6-and-later)  
    (let ((mouse-loc (#/locationInWindow event)))
      (view-event-handler 
       (lui-window Self)
       (make-instance 'gesture-event 
         :event-type (native-to-lui-event-type (#/type event))
         :x (truncate (pref mouse-loc :<NSP>oint.x))
         :y (truncate (- (height (lui-window Self)) (pref mouse-loc :<NSP>oint.y)))
         :native-event Event)))))


(objc:defmethod (#/magnifyWithEvent: :void) ((self native-window-view) Event)
  ;; should be save at this event method should not even get called in non OS x and pre 10.6 environments
  (when (mac-os-x-10.6-and-later) 
    (let ((mouse-loc (#/locationInWindow event)))
      (view-event-handler 
       (lui-window Self)
       (make-instance 'gesture-event
         :magnification (ccl::with-ns-exceptions-as-errors  
                            (objc:objc-message-send Event "magnification" #>CGFloat))
         :event-type (native-to-lui-event-type (#/type event))
         :x (truncate (pref mouse-loc :<NSP>oint.x))
         :y (truncate (- (height (lui-window Self)) (pref mouse-loc :<NSP>oint.y)))
         :native-event Event)))))


(objc:defmethod (#/rotateWithEvent: :void) ((self native-window-view) Event)
  ;; should be save at this event method should not even get called in non OS x and pre 10.6 environments
  (when (mac-os-x-10.6-and-later) 
    (let ((mouse-loc (#/locationInWindow event)))
      (view-event-handler 
       (lui-window Self)
       (make-instance 'gesture-event
         :rotation (ccl::with-ns-exceptions-as-errors 
                       (objc:objc-message-send Event "rotation" #>float))
         :event-type (native-to-lui-event-type (#/type event))
         :x (truncate (pref mouse-loc :<NSP>oint.x))
         :y (truncate (- (height (lui-window Self)) (pref mouse-loc :<NSP>oint.y)))
         :native-event Event)))))



;****************************************************
; Orientation of coordinates                        *
;****************************************************

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
  (catch-errors-nicely ("executing control action")
   (invoke-action (lui-control Self))))


(defmethod INITIALIZE-EVENT-HANDLING ((Self control))
  (#/setTarget: (native-view Self)
                (make-instance 'native-target 
                  :native-control (native-view Self)
                  :lui-control Self))
  (#/setAction: (native-view Self) (objc::@selector #/activateAction)))


(objc:defmethod (#/isFlipped :<BOOL>) ((self ns:ns-control))
  ;; ALL controls: Flip to coordinate system to 0, 0 = upper left corner
  #$YES)


(defmethod IS-ENABLED ((Self control))
  (when (#/isEnabled (native-view self))
    t))

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


(defmethod TURN-ON ((self button-control))
  (#/setState: (Native-View self) #$NSOnState))


(defmethod TURN-OFF ((self button-control))
  (#/setState: (Native-View self) #$NSOffState))
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
      (when (color self)
        (#/setBackgroundColor: (#/cell native-control) (#/redColor ns:ns-color))
        (#/setBordered: native-control #$NO))
      (cond
       ((equal (bezel-style self) "square")
        (#/setBezelStyle: Native-Control #$NSThickerSquareBezelStyle)
        ;(#/setBezelStyle: Native-Control #$NSShadowlessSquareBezelStyle)
        (#/setBezelStyle: native-control #$NSSmallSquareBezelStyle))
        (t
         ;(#/setBezelStyle: Native-Control #$NSShadowlessSquareBezelStyle)
         (#/setBezelStyle: Native-Control #$NSThickerSquareBezelStyle)))
      (#/setTitle: Native-Control (native-string (text Self))))
    Native-Control))


(defmethod (setf text) :after (Text (Self bevel-button-control))
  (#/setTitle: (native-view Self) (native-string Text)))


(defmethod TURN-ON ((self bevel-button-control))
  (#/setState: (Native-View self) #$NSOnState))


(defmethod TURN-OFF ((self bevel-button-control))
  (#/setState: (Native-View self) #$NSOffState))

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


(defmethod ENABLE ((self checkbox-control))
  (#/setEnabled: (native-view self) #$YES))


(defmethod TURN-ON ((self checkbox-control))
  (#/setState: (Native-View self) #$NSOnState))


(defmethod TURN-OFF ((self checkbox-control))
  (#/setState: (Native-View self) #$NSOffState))
;__________________________________
; STRING-LIST-TEXT-VIEW            |
;__________________________________/

(defclass NATIVE-STRING-LIST-TEXT-VIEW (ns:ns-text-view)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defclass STRING-LIST-TEXT-VIEW (control)
  ((is-selected :accessor is-selected :initform nil)
   (container :accessor container :initform nil)
   ;(lui-view :accessor lui-view :initform nil)
   ;(text :accessor text :initform nil)
   )
  (:documentation "A text field that detects mouse events.  "))

(defmethod MAKE-NATIVE-OBJECT ((Self string-list-text-view))
  (let ((Native-Control (make-instance 'native-string-list-text-view :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
     
      (#/initWithFrame: Native-Control Frame)
      ;(#/setBackgroundColor: self (#/whiteColor ns:ns-color))
      Native-Control)))

(objc:defmethod (#/drawRect: :void) ((self native-string-list-text-view) (rect :<NSR>ect))
  (ns:with-ns-rect (Frame (NS:NS-RECT-X rect) (NS:NS-RECT-Y rect) (- (NS:NS-RECT-WIDTH rect) 1)(NS:NS-RECT-HEIGHT rect))
  (call-next-method Frame) 
  (if (is-selected (lui-view self))
    (progn
      ;; Draw the selected item with a blue selection background.  
      (#/set (#/colorWithDeviceRed:green:blue:alpha: ns:ns-color 0.0 0.2 1.0 .6))
      
        (#/fillRect: ns:ns-bezier-path Frame)))))

(defmethod MAP-SUBVIEWS ((Self string-list-text-view) Function &rest Args)
  (declare (ignore Function Args))
  ;; no Cocoa digging
  )

(defmethod initialize-event-handling ((Self string-list-text-view))
  ;; no event handling for rows
  )

(objc:defmethod (#/mouseDown: :void) ((self native-string-list-text-view) Event)
  (declare (ignore Event))
  (if (list-items (container (lui-view self)))
    (dolist (item (list-items (container (lui-view self))))
      (setf (is-selected item) nil)
      (#/setNeedsDisplay: (native-view item) #$YES)))
  (setf (is-selected (lui-view self)) t)
  (setf (selected-string (container (lui-view self))) (text (lui-view self)))
  (#/setNeedsDisplay: self #$YES)
  (funcall (action (container (lui-view Self))) (window (container (lui-view Self))) (target (container (lui-view Self)))))


;__________________________________
; STRING-LIST-VIEW-CONTROL         |
;__________________________________/

(defclass NATIVE-STRING-LIST-VIEW (ns:ns-view)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(objc:defmethod (#/drawRect: :void) ((self native-string-list-view) (rect :<NSR>ect))
  (call-next-method rect)
  (layout (lui-view self))
  (set-size (lui-view self) (NS:NS-RECT-WIDTH rect) (NS:NS-RECT-HEIGHT rect))
  (#/set (#/colorWithDeviceRed:green:blue:alpha: ns:ns-color 1.0 1.0 1.0 1.0))
  (#/fillRect: ns:ns-bezier-path rect)
  (#/set (#/colorWithDeviceRed:green:blue:alpha: ns:ns-color .5 .5 .5 1.0))
  (#/strokeRect: ns:ns-bezier-path rect))


(objc:defmethod (#/isFlipped :<BOOL>) ((self native-string-list-view))
  ;; Flip to coordinate system to 0, 0 = upper left corner
  #$YES)


(defmethod MAP-SUBVIEWS ((Self string-list-view-control) Function &rest Args)
  (declare (ignore Function Args))
  ;; no Cocoa digging
  )


(defmethod MAKE-NATIVE-OBJECT ((Self string-list-view-control))
  (let ((Native-Control (make-instance 'native-string-list-view :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame)
      ;(#/setBackgroundColor: self (#/whiteColor ns:ns-color))
      Native-Control)))


(defmethod ADD-STRING-LIST-ITEM ((Self string-list-view-control) string)
  "Adds an item to the string-list that will display the text contained in the variable string"
  (let ((text (make-instance 'string-list-text-view))) 
    (ns:with-ns-rect (Frame2 1 (+ 1 (* (item-height self) (length (list-items self)))) (width self)  20 )
      (setf (container text) self)
      (setf (text text) string)
      (#/initWithFrame: (native-view text) Frame2)
      (#/insertText: (native-view text) (native-string string))
      (#/setBackgroundColor: (native-view text) (#/whiteColor ns:ns-color))
      (#/setDrawsBackground:  (native-view text) #$YES)
      (#/setEditable: (native-view text) #$NO)
      (#/setSelectable: (native-view text) #$NO)
      (#/addSubview:  (Native-view self) (native-view text)))
    (case (list-items self)
      (nil 
       (setf (list-items self) (list text))
       (setf (is-selected (first (list-items self))) t)
       (display self))
      (t (setf (list-items self) (append (list-items self) (list text)))))))


(defmethod SET-LIST ((Self string-list-view-control) list) 
  "Used to set the string-list to a given list instead setting the list string by string.  Also will select the first item in the list.  "

  (dolist (subview (gui::list-from-ns-array (#/subviews (native-view self))))
    (#/removeFromSuperview subview))
  (setf (list-items self) nil)
  (dolist (item list)
    (add-string-list-item self item))
  (setf (is-selected (first (list-items self))) t)
  (setf (selected-string self) (text (first (list-items self))))
  ;(#/setNeedsDisplay: (native-view self) #$YES)
  (display self))


(defmethod SELECT-ITEM ((Self string-list-view-control) item-name) 
  (dolist (item (list-items self))
    (if (equal (string-capitalize (text item)) (string-capitalize item-name))
      (progn
        (setf (is-selected item) t)
        (setf (selected-string self) (text item))
        (return-from select-item t))
      (setf (is-selected item) nil)))
  nil)


(defmethod initialize-event-handling ((Self string-list-view-control))
  ;; no event handling for rows
  )

(defclass ATTRIBUTE-TEXT-VIEW-DELEGATE (ns:ns-object)
  ((lui-view :accessor lui-view :initform nil :initarg :lui-view))
  (:metaclass ns:+ns-object
	      :documentation " delegate"))


(defclass NATIVE-attribute-editor-view (ns:ns-text-field)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod MAKE-NATIVE-OBJECT ((self attribute-editor-view))
  (let ((Native-Control (make-instance 'native-attribute-editor-view :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame)
      (#/setStringValue: Native-Control (native-string (Text self)))
      ;(#/setBackgroundColor: self (#/whiteColor ns:ns-color))
      Native-Control)))

(defmethod (setf text) :after (Text (self attribute-editor-view))
  (#/setStringValue: (native-view Self) (native-string Text)))

(defmethod VALUE ((self attribute-editor-view))
  (ccl::lisp-string-from-nsstring 
   (#/stringValue (native-view Self))))

(defmethod (setf VALUE)  (Text (self attribute-editor-view))
  (#/setStringValue: (native-view Self) (native-string Text)))

(objc:defmethod (#/textDidChange: :void) ((self native-attribute-editor-view) Notification)
  (call-next-method Notification)
  )


(objc:defmethod (#/textDidBeginEditing: :void) ((self native-attribute-editor-view) Notification)
  (setf (value-save (lui-view self)) (#/stringValue self))
  (call-next-method Notification))


(objc:defmethod (#/textDidEndEditing: :void) ((self native-attribute-editor-view) Notification)
  (call-next-method Notification)
  (text-did-end-editing (lui-view self)))



(defmethod TEXT-DID-END-EDITING ((Self attribute-value-list-text-view))
  (when (read-from-string (ccl::lisp-string-from-nsstring (#/stringValue (native-view self))) nil nil)
    (unless (numberp  (read-from-string (ccl::lisp-string-from-nsstring (#/stringValue (native-view self))) nil nil))
      (if (value-save self)
        (#/setStringValue: (native-view self) (value-save self))))
    (unless (attribute-owner self)
      (setf (attribute-owner self) (part-of  self)))
    (if (attribute-owner self)
      (funcall (attribute-changed-action self) (attribute-owner self)  (window self)  (attribute-symbol (container self)) (read-from-string (ccl::lisp-string-from-nsstring (#/stringValue (native-view self))) nil nil)) 
      )))

;__________________________________
; ATTRIBUTE-VALUE-LIST-ITEM-VIEW   |
;__________________________________/


(defclass ATTRIBUTE-VALUE-LIST-ITEM-VIEW (ns:ns-view)
  ((is-selected :accessor is-selected :initform nil)
   (container :accessor container :initform nil :initarg :container)
   (lui-view :accessor lui-view :initform nil)
   (text :accessor text :initform nil :initarg :text )
   (height :accessor height :initform nil :initarg :height )
   (width :accessor width :initform nil :initarg :width )
   (attribute-symbol :accessor attribute-symbol :initarg :attribute-symbol :initform nil :documentation "The text field will store the name of attribute as a native-string so for conveniance we store the attribute as a symbol here")
   (attribute-value :accessor attribute-value :initform 0 :initarg :attribute-value)
   (value-text-field :accessor value-text-field :initform nil :documentation "Editable NSTextField containg the value of the attribute")
   (name-text-field :accessor name-text-field :initform nil :documentation "NSTextView containg the name of the attribute" )
   (attribute-owner :accessor attribute-owner :initform nil :initarg :attribute-owner :documentation "An owner can be associated with this object and if so, it will be notifed when this objects value-text-field is editted.  In order for this to work, you will need to an attribute-changed-action.")
   (attribute-changed-action :accessor attribute-changed-action :initform nil :initarg :attribute-changed-action :documentation "The action that should be called when the attribute's value has been changed" )
   (timer-triggers :accessor timer-triggers :initform nil :documentation "when to start TIME triggers")
   )
  (:metaclass ns:+ns-object
              :documentation "An item of an attribute-value-list-view-control, this item is made up of a text view displaying the name of the attribute, and an editable field displaying the value.  "))


(defmethod INITIALIZE-INSTANCE :after ((Self attribute-value-list-item-view) &rest Args)
  (declare (ignore Args))
  (let ((text (#/alloc ns:ns-text-view))) 
    (ns:with-ns-rect (Frame2 1 (+ 1 0) (* .5 (width self))  20 )
      (#/initWithFrame: text Frame2)
      (#/insertText: text (native-string (text self)))
      (#/setBackgroundColor: text (#/whiteColor ns:ns-color))
      (#/setDrawsBackground:  text #$YES)
      (#/setEditable: text #$NO)
      (#/setSelectable: text #$NO)
      (setf (name-text-field self) text)
      (#/addSubview:  self text))
    (let ((value-text (make-instance 'attribute-value-list-text-view :container self :attribute-owner (attribute-owner self) :attribute-changed-action (attribute-changed-action self)))) 
      (ns:with-ns-rect (Frame2 (* .5 (width self)) 1 (* .5 (width self))  20 )
        (#/initWithFrame: (native-view value-text) Frame2)
        (#/setStringValue:  (native-view value-text) (native-string (write-to-string (attribute-value self))))
        (#/setBackgroundColor: (native-view value-text) (#/whiteColor ns:ns-color))
        (#/setDrawsBackground:  (native-view value-text) #$YES)
        (#/setSelectable: (native-view value-text) #$YES)
        (#/setEditable: (native-view value-text) #$YES)
        (setf (value-text-field self) (native-view value-text))
        (#/addSubview:  self (native-view value-text))))))


(defmethod TIMER-DUE-P ((Self attribute-value-list-item-view) Ticks) 
  (let ((Time (getf (timer-triggers Self) Ticks 0))
        (Now (get-internal-real-time)))
    (when (or (>= Now Time)                          ;; it's time
              (> Time (+ Now Ticks Ticks)))    ;; timer is out of synch WAY ahead
      (setf (getf (timer-triggers Self) Ticks) (+ Now Ticks))
      t)))


(objc:defmethod (#/drawRect: :void) ((Self attribute-value-list-item-view) (rect :<NSR>ect))
  (call-next-method rect)
  (when (timer-due-p self (truncate (* 1.0 internal-time-units-per-second)))
    (#/setTextColor: (value-text-field self) (#/blackColor ns:ns-color))))

;___________________________________
; ATTRIBUTE VALUE LIST VIEW CONTROL |
;__________________________________/

(defclass NATIVE-ATTRIBUTE-VALUE-LIST-VIEW-CONTROL (ns:ns-view)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(objc:defmethod (#/isFlipped :<BOOL>) ((self native-attribute-value-list-view-control))
  ;; Flip to coordinate system to 0, 0 = upper left corner
  #$YES)


(defmethod MAP-SUBVIEWS ((Self attribute-value-list-view-control) Function &rest Args)
  (declare (ignore Function Args))
  ;; no Cocoa digging
  )

(defmethod MAKE-NATIVE-OBJECT ((Self attribute-value-list-view-control))
  (let ((Native-Control (make-instance 'native-attribute-value-list-view-control :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame)
      Native-Control)))


(defmethod INITIALIZE-INSTANCE :after ((Self attribute-value-list-view-control) &rest Args)
  "We need to create a new thread that will set the color of attribute values back to black"
  (declare (ignore args))
  ;(call-next-method)
  (setf (color-update-process self)
    (process-run-function
     '(:name "Attribute Window Thread" )
     #'(lambda ()
         (loop
           (catch-errors-nicely ("animating attributes in attribute editor")
            (reset-color-of-items self)
            (sleep .01)))))))


(defmethod RESET-COLOR-OF-ITEMS ((Self attribute-value-list-view-control))
  (dolist (item (list-items self))
    (when (timer-due-p item (truncate (* 1.0 internal-time-units-per-second)))
      (#/setTextColor: (value-text-field item) (#/blackColor ns:ns-color)))))


(defmethod ADD-ATTRIBUTE-LIST-ITEM ((Self attribute-value-list-view-control) string value &key (action nil) (owner nil))
  "Adds an item to the attribute-list that will display the text contained in the variable string"
  (if (attribute-owner self)
    (setf owner (attribute-owner self)))
  (if (attribute-changed-action self)
    (setf action (attribute-changed-action self)))
  (let ((item (make-instance 'attribute-value-list-item-view :attribute-symbol string :container self :width (width self) :height (item-height self) :attribute-value value :text string :attribute-changed-action action :attribute-owner owner) ))
    (ns:with-ns-rect (Frame2 1 (+ 1 (* (item-height self) (length (list-items self)))) (Width self)  20 )
      (#/initWithFrame: item Frame2))
    (#/addSubview:  (Native-view self) item)
    (case (list-items self)
      (nil 
       (setf (list-items self) (list item))
       (setf (is-selected (first (list-items self))) t)
       (display self))
      (t 
       (setf (list-items self) (append (list-items self) (list item)))))))

  
(defmethod ADD-ITEM-BY-MAKING-NEW-LIST  ((Self attribute-value-list-view-control) string value &key (action nil) (owner nil)) 
  (declare (ignore action owner))
  ;(set-list self (append (list-items self) (list (make-instance 'attribute-value-list-item-view :attribute-symbol string :container self :width (width self) :height (item-height self) :attribute-value value :text string :attribute-changed-action action :attribute-owner owner))))
  (dolist (subview (gui::list-from-ns-array (#/subviews (native-view self))))
    (#/removeFromSuperview subview))
  (let ((temp-list (copy-list (list-items self))))
    (setf (list-items self) nil)
  (dolist (item temp-list)
    (add-attribute-list-item  self (string (attribute-symbol item)) (attribute-value item) ))
  (add-attribute-list-item self string value)))
                                         
(defmethod SET-LIST ((Self attribute-value-list-view-control) list ) 
  "Used to set the string-list to a given list instead setting the list string by string.  Also will select the first item in the list.  "
  (dolist (subview (gui::list-from-ns-array (#/subviews (native-view self))))
    (#/removeFromSuperview subview))
  (setf (list-items self) nil)
  (dolist (item list)
    (add-attribute-list-item self (first item) (second item) :action (attribute-changed-action self) :owner (attribute-owner self)))
  (display self))


(defmethod SET-VALUE-OF-ITEM-WITH-NAME ((Self attribute-value-list-view-control) name value)
  (dolist (item (list-items self))
    (when (equal (text item) name)
      (unless (equal (write-to-string value) (ccl::lisp-string-from-nsstring (#/stringValue (value-text-field item))))
        (setf (attribute-value item) value)
        (#/setStringValue: (value-text-field item) (native-string (write-to-string Value)))
        (#/setTextColor: (value-text-field item) (#/redColor ns:ns-color)))
      (return-from set-value-of-item-with-name t)))
  ;(add-attribute-list-item self name value)
  (lui::add-item-by-making-new-list self name value)
  (layout self)
  (display self))


(defmethod SELECT-ITEM ((Self attribute-value-list-view-control) item-name) 
  (dolist (item (list-items self))
    (if (equal (string-capitalize (text item)) (string-capitalize item-name))
      (progn
        (setf (is-selected item) t)
        (setf (selected-string self) (text item))
        (return-from select-item t))
      (setf (is-selected item) nil)))
  nil)


(defmethod initialize-event-handling ((Self attribute-value-list-view-control))
  ;; no event handling for rows
  )

;(truncate (* Time internal-time-units-per-second))
;__________________________________
; SCROLLER-CONTROL                 |
;__________________________________/

(defclass NATIVE-SCROLLER (ns:ns-scroller)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod MAKE-NATIVE-OBJECT ((Self scroller-control))
  (let ((Native-Control (make-instance 'native-scroller :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      #-cocotron 
      ;;cocotron does not support the NSSmallControlSize
      (when (small-scroller-size self)
          (setf (width self) (- (width self) 4))
          (#/setControlSize: Native-Control #$NSSmallControlSize))
      (#/sizeToFit Native-Control)
      (#/initWithFrame: Native-Control Frame)
      (#/setFloatValue:knobProportion: Native-Control 0.0 (knob-proportion self))
      (#/setArrowsPosition: Native-Control #$NSScrollerArrowsMinEnd ) 
      (#/setEnabled: Native-Control #$YES)
      Native-Control)))

;; Hack: NSscroller does not automatically deal with scrollWheel events
(objc:defmethod (#/scrollWheel: :void) ((self native-scroller) Event)
  (#/setDoubleValue: Self (- (#/doubleValue Self) (* 0.005d0 (#/deltaY Event))))
  (#/activateAction (#/target Self)))


(defmethod SET-SCROLLER-POSITION ((Self scroller-control) float)
  (#/setFloatValue:knobProportion: (native-view self) float .2))


(defmethod VALUE ((Self scroller-control))
  (#/floatValue (native-view self)))


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
          (#/setButtonType: Native-Control #$NSOnOffButton)   
          (#/setImagePosition: Native-Control #$NSImageOnly)
          (#/setImage: Native-Control NS-Image)
          (#/setBezelStyle: Native-Control #$NSShadowlessSquareBezelStyle)
          (#/setTitle: Native-Control (native-string (text Self)))))
      Native-Control)))

#|
(objc:defmethod (#/drawRect: :void) ((self native-button-image) (rect :<NSR>ect))
  (call-next-method rect)
  #+cocotron
  (if (selected-in-cluster (lui-view self))
    (progn
      (#/set (#/colorWithDeviceRed:green:blue:alpha: ns:ns-color .2 .2 .2 .62))
      (#/fillRect: ns:ns-bezier-path rect))))
|#

(defmethod (setf text) :after (Text (Self image-button-control))
  (#/setTitle: (native-view Self) (native-string Text)))


(defmethod SET-BUTTON-OFF ((Self image-button-control))
  (#/setState: (native-view self) #$NSOffState))


(defmethod SET-BUTTON-ON ((Self image-button-control))
  (#/setState: (native-view self) #$NSOnState))


(defmethod TURN-ON ((self image-button-control))
  (#/setState: (Native-View self) #$NSOnState))


(defmethod TURN-OFF ((self image-button-control))
  (#/setState: (Native-View self) #$NSOffState))


(defmethod IS-ON-P ((self image-button-control))
  (when (equal (#/state (native-view self)) #$NSOnState)
    t))
;__________________________________
; RADIO BUTTON                     |
;__________________________________/

(defclass native-radio-button (ns:ns-matrix)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod make-native-object ((Self radio-button-control))
  (let ((Native-Control (make-instance 'native-radio-button :lui-view Self))
        (prototype (#/init (#/alloc ns:ns-button-cell))))
    (unless (elements self)
      (setf (elements self) (#/init (#/alloc ns:ns-mutable-array))))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/setTitle: prototype (native-string "Option"))
      (#/setButtonType: prototype #$NSRadioButton)
      (#/initWithFrame:mode:prototype:numberOfRows:numberOfColumns: Native-Control Frame #$NSRadioModeMatrix prototype 3 1)
      (let ((cells (#/cells Native-Control))
            (cell (#/init (#/alloc ns:ns-button-cell))))
        (#/setTitle: cell (native-string "options3"))
        (#/setButtonType: cell #$NSRadioButton)       
        (#/setTitle: (#/objectAtIndex: cells '0) #@"Option1")
        (#/putCell:atRow:column: Native-Control cell '1 '0)))
    Native-Control))


(defmethod (setf text) :after (Text (Self radio-button-control))
  (#/setTitle: (native-view Self) (native-string Text)))


(defmethod GET-SELECTED-ACTION ((Self radio-button-control))
  (elt (actions self)  (#/indexOfObject: (elements Self) (#/selectedCell (native-view self)))))


(defmethod RADIO-ACTION ((window window) (self Radio-Button-Control))
  (let ((action (get-selected-action self)))
    (funcall action Window Self)))


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
; IMAGE BUTTON CLUSTER CONTROL     |
;__________________________________/

(defmethod INITIALIZE-INSTANCE :after ((Self radio-button-control) &rest Args)
  (declare (ignore Args))
  (setf (elements self) (#/init (#/alloc ns:ns-mutable-array)))
  (call-next-method))


;__________________________________
; Popup Button                     |
;__________________________________/

(defclass NATIVE-POPUP-BUTTON (ns:ns-pop-up-button)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod MAKE-NATIVE-OBJECT ((Self popup-button-control))
  (let ((Native-Control (make-instance 'native-popup-button :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame:pullsDown: Native-Control Frame #$NO)
      ;; mini controls
      (#/setControlSize: (#/cell native-control)  #$NSSmallControlSize)
      (#/setFont: native-control (#/systemFontOfSize: ns:ns-font (#/systemFontSizeForControlSize: ns:ns-font #$NSSmallControlSize)))
    Native-Control)))


(defmethod VALUE ((self popup-button-control))
  (if (%null-ptr-p (#/selectedItem (native-view self)))
    nil
    (ccl::lisp-string-from-nsstring (#/title (#/selectedItem (native-view self))))))


(defmethod GET-SELECTED-ACTION ((Self popup-button-control))
  ;; -1 implies a that nothing is selected on Windows
  (unless (equal -1 (#/indexOfSelectedItem (native-view self)))
    (elt (actions self)  (#/indexOfSelectedItem (native-view self)))))


(defmethod POPUP-ACTION ((window window) (self popup-Button-Control))
  (unless (eql (get-selected-action self) NIL)
    (let ((action (get-selected-action self)))
      (funcall action Window Self))))


(defmethod SET-SELECTED-ITEM-WITH-TITLE ((Self popup-button-control) text)
  (#/selectItemWithTitle: (native-view self) (native-string text)))


(defmethod ADD-ITEM ((Self popup-button-control) Text Action )
  (if (equal (#/indexOfItemWithTitle: (native-view Self) (native-string Text)) -1)
    (progn 
      (#/addItemWithTitle: (native-view Self) (native-string Text))
      (setf (actions Self) (append (actions Self) (list Action))))
    (warn "Cannot add item with the same title (~S)" Text)))


(defmethod REMOVE-ITEM ((Self popup-button-control) Title)
  (#/removeItemWithTitle: (native-view Self) (native-string Title)))


(defmethod ADD-NS-MENU-ITEM ((Self popup-button-control) Item)
  (if (equal (#/indexOfItemWithTitle: (native-view Self) (native-string (Text item))) -1)
    (progn   
      (#/addItem: (#/menu (native-view Self)) (native-view Item))
      (setf (actions Self) (append (actions Self) (list (Action item)))))
    (warn "Cannot add item with the same title (~S)" (Text item))))


;__________________________________
; Popup Image Button Item          |
;__________________________________/

(defclass native-popup-image-button-item (ns:ns-menu-item)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod INVOKE-ACTION ((Self popup-image-button-item-control))  
  (funcall (action Self) (window (popup-image-button self)) (target Self)))


(defmethod make-native-object ((Self popup-image-button-item-control))
  (let ((Native-Control (make-instance 'native-popup-image-button-item :lui-view Self)))
    (#/initWithTitle:action:keyEquivalent: Native-Control (native-string (text self)) (objc::@selector #/activateAction) (native-string ""))
    Native-Control))


;__________________________________
; Popup Image Button Submenu       |
;__________________________________/

(defclass native-popup-image-button-submenu (ns:ns-menu)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod initialize-event-handling ((Self popup-image-button-submenu-control))
  ;do nothing for now
  )


(defmethod make-native-object ((Self popup-image-button-submenu-control))
  (let ((Native-Control (make-instance 'native-popup-image-button-submenu :lui-view Self)))
    (#/initWithTitle: Native-Control (native-string (text self)) )
    (#/setAutoenablesItems: native-control #$NO)
    Native-Control))


;__________________________________
; Popup Image Button               |
;__________________________________/

(defclass native-popup-image-button (ns:ns-button)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod make-native-object ((Self popup-image-button-control))
  (let ((Native-Control (make-instance 'native-popup-image-button :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (let ((Path (native-path "lui:resources;buttons;" (image Self)))
            (NS-Image (#/alloc ns:ns-image))
            (disclosure-Image (#/alloc ns:ns-image)))
        (unless (probe-file Path) (error "no such image file for button ~A" (image Self)))
        (#/initWithContentsOfFile: NS-Image  (native-string (native-path "lui:resources;buttons;" (image Self))))
        (#/initWithContentsOfFile: disclosure-image  (native-string (native-path "lui:resources;buttons;" "disclosureDown4.png")))
        (#/setFlipped: disclosure-image #$YES)
        (#/setFlipped: ns-image #$YES)
        (setf (disclosure-image self) disclosure-image)
        (#/initWithFrame: Native-Control Frame)
        ;(#/setImageScaling: Native-Control 1)   
        (#/setButtonType: Native-Control #$NSOnOffButton)   ;;;NSMomentaryPushInButton)
        (#/setImagePosition: Native-Control #$NSImageOnly)
        (#/setImage: Native-Control NS-Image)
        (#/setBezelStyle: Native-Control #$NSShadowlessSquareBezelStyle)
        (#/setTitle: Native-Control (native-string (text Self))))
      (let ((Pop-up (make-instance 'popup-button-control :x 0 :y -20 )))  
        (#/setAutoenablesItems: (native-view pop-up) #$NO)
        (#/setTransparent: (native-view Pop-Up) #$YES)
        (setf (popup-button self) pop-up))
      Native-Control)))


(objc:defmethod (#/drawRect: :void) ((self native-popup-image-button) (rect :<NSR>ect))
  ;; if there is an error make this restartable from alternate console
  (if (draw-disclosure (lui-view self))
    (progn
      (ns:with-ns-rect (Frame (- (NS:NS-RECT-WIDTH rect) 10) 0 10 (NS:NS-RECT-HEIGHT rect))
        (#/drawInRect:fromRect:operation:fraction: (disclosure-image (lui-view self)) Frame #$NSZeroRect #$NSCompositeCopy 1.0))
      (ns:with-ns-rect (Frame 0 0 (- (NS:NS-RECT-WIDTH rect) 10)  (NS:NS-RECT-HEIGHT rect) )
        ;(call-next-method frame)
        (#/drawInRect:fromRect:operation:fraction: (#/image self) Frame #$NSZeroRect #$NSCompositeCopy 1.0)
        ))
    (#/drawInRect:fromRect:operation:fraction: (#/image self) rect #$NSZeroRect #$NSCompositeCopy 1.0)))


(defmethod POPUP-IMAGE-BUTTON-ACTION ((w window) (Button popup-image-button-control))
  (dotimes (i (length (items button)))
    (let ((item (#/itemAtIndex: (native-view (popup-button button))i)))
      (if (funcall (enable-predicate (elt (items button) i)) w (elt (items button) i))
        (#/setEnabled: item #$YES)
        (#/setEnabled: item #$NO))))
  (ns:with-ns-rect (Frame 0 0 10 10)
    (add-subviews button (popup-button button))
    (ns:with-ns-point (Point 0 0)    
      (let* ((event2 (#/alloc ns:ns-event))
             ;(item (#/selectedItem (native-view (popup-button button))))
             ; (action (#/action item))
             ; (target (#/action item))
             )        
        (#/trackMouse:inRect:ofView:untilMouseUp: (#/cell (native-view (popup-button button))) event2 (#/bounds (native-view (popup-button button))) (native-view (popup-button button)) #$NO)
        (#/setState: (#/cell (native-view (popup-button button))) #$NSOffState)
        #+cocotron
        (#/sendAction:to: (native-view (popup-button button)) (#/action (native-view (popup-button button))) (#/target (native-view (popup-button button))))
         (#/removeFromSuperview (native-view (popup-button button)))
        ))))


(defun SHOW-STRING-POPUP-FROM-ITEM-LIST (window item-list)
    (let ((Pop-up (make-instance 'popup-button-control )))
      (dolist (item item-list)
        (add-ns-menu-item Pop-Up item))
      (add-subviews window Pop-up)
      (#/setTransparent: (native-view Pop-Up) #$YES)
      (#/performClick:  (native-view Pop-up) +null-ptr+)
      (#/removeFromSuperview (native-view Pop-up))
      (ccl::lisp-string-from-nsstring  (#/titleOfSelectedItem (native-view Pop-Up)))
      ))


(defmethod ADD-POPUP-ITEM ((Self popup-image-button-control) Text Action predicate key-equivalent)
  (let ((item (make-instance 'popup-image-button-item-control  :text text :action action :popup-image-button self )))
    (if predicate
      (setf (enable-predicate item) predicate))
    (case (items self)
          (nil (setf (items self) (list item)))
          (t (setf (items self) (append (items self) (list item)))))
    ;(add-ns-menu-item (popup-button self) item)
    (add-item (popup-button self) text action)
    (if key-equivalent
      (#/setKeyEquivalent: (#/lastItem (native-view (popup-button self))) (native-string key-equivalent)))
    ;(#/addItem: (menu self) (native-view item) )
    ))


(defmethod ADD-POPUP-SUBMENU ((Self popup-image-button-control) Text action predicate )
  (declare (ignore  predicate))
  (let ((menu (make-instance 'popup-image-button-submenu-control)))
    (add-item (popup-button self) text action)
    ;(#/addItemWithTitle:action:keyEquivalent: (native-view menu) (native-string "SUB MENU ITEM 1") (objc::@selector #/activateAction) (native-string ""))
    (#/setSubmenu: (#/itemWithTitle: (native-view (popup-button self)) (native-string text)) (native-view menu))
    ))


(defmethod ADD-POPUP-SUBMENU2 ((Self popup-image-button-control) Menu text action predicate)
  (declare (ignore  predicate))
    (add-item (popup-button self) text action)
    ;(#/addItemWithTitle:action:keyEquivalent: (native-view menu) (native-string "SUB MENU ITEM 1") (objc::@selector #/activateAction) (native-string ""))
    (#/setSubmenu: (#/itemWithTitle: (native-view (popup-button self)) (native-string text)) (native-view menu))
    )

(defmethod ADD-SUBMENU-TO-SUBMENU ((Self popup-image-button-submenu-control) new-menu text action predicate)
  (declare (ignore  text action predicate))
  (#/addItemWithTitle:action:keyEquivalent: (native-view self) (native-string (text new-menu)) (objc::@selector #/activateAction)(native-string ""))
  ;(#/addItemWithTitle:action:keyEquivalent: (native-view menu) (native-string "SUB MENU ITEM 1") (objc::@selector #/activateAction) (native-string ""))
  (#/setSubmenu: (#/itemWithTitle: (native-view self) (native-string (text new-menu))) (native-view new-menu))
  )


(defmethod ADD-ITEM-TO-SUBMENU ((Self popup-image-button-submenu-control) Text action predicate)
  (declare (ignore action predicate))
  (#/addItemWithTitle:action:keyEquivalent: (native-view self) (native-string text) (objc::@selector #/activateAction) (native-string ""))
  ;   (#/setSubmenu: (#/itemWithTitle: (native-view (popup-button self)) (native-string text)) menu)
  )

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


(defmethod CHOICE-BUTTON-ACTION ((window window) (self choice-button-Control))
  (let ((action (get-selected-action self)))
    (funcall action Window Self)))


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

(defmethod (setf VALUE)  (Value (Self slider-control))
  (#/setFloatValue: (native-view Self) Value))

(defmethod VALUE ((Self slider-control))
  (#/floatValue (native-view Self)))

;__________________________________
; JOG SLIDER                        |
;__________________________________/

(defclass NATIVE-JOG-SLIDER (native-slider)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod MAKE-NATIVE-OBJECT ((Self jog-slider-control))
  ;; similar to native slider
  (let ((Native-Control (make-instance 'native-jog-slider :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame)
      (#/setMinValue: Native-Control (float (min-value Self) 0d0))
      (#/setMaxValue: Native-Control (float (max-value Self) 0d0))
      (#/setNumberOfTickMarks: Native-Control (truncate (tick-marks Self)))
      ;; Make sure the slider's indicator/thumb is positioned properly based on
      ;; control's initial value
      (#/setFloatValue: Native-Control (slot-value Self 'value)))
    Native-Control))


(objc:defmethod (#/mouseDown: :void) ((self native-jog-slider) event)
  ;; NSslider runs its own event loop on mouse down -> no mouseUp or mouseDragged events
  ;; http://www.cocoabuilder.com/archive/cocoa/157955-nsslider-mouseup.html
  ;; setup slide action to call lui action but also a thread that lives as long as the mouse is down
  (setf (is-jog-active (lui-view Self)) t)
  (process-run-function
     '(:name "Jog Dial Thread" )
     #'(lambda ()
         ;; start jog in separate thread to avoid delay of slider knob move
         (start-jog (lui-view Self))
         ;; as long as mouse is down keep running control action at interval frequency
         (loop
           (unless (is-jog-active (lui-view Self))  (return))
           (catch-errors-nicely ("user is moving jog dial")
            ;; better to activate the action in the main thread!!
                                (in-main-thread ()
              (#/activateAction (#/target Self));)
              (sleep (action-interval (lui-view Self))))))
         
         ))
  ;; this actually does the mouse tracking until mouse up
  (call-next-method Event)
 
  ;; mouse is up
  (stop-jog (lui-view Self)))


;__________________________________
; JOG BUTTON                       |
;__________________________________/

(defclass NATIVE-JOG-BUTTON (native-button-image)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod make-native-object ((Self jog-button-control))
  (let ((Native-Control (make-instance 'native-jog-button :lui-view Self)))
    (let ((NS-Image (#/alloc ns:ns-image)))
      (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
        (let ((Path (native-path "lui:resources;buttons;" (image Self))))
          (unless (probe-file Path) (error "no such image file for button ~A" (image Self)))
          (#/initWithContentsOfFile: NS-Image  (native-string (native-path "lui:resources;buttons;" (image Self))))
          (#/initWithFrame: Native-Control Frame)
          (#/setButtonType: Native-Control #$NSMomentaryLightButton)   
          (#/setButtonType: Native-Control #$NSMomentaryPushInButton)
          (#/setImagePosition: Native-Control #$NSImageOnly)
          (#/setImage: Native-Control NS-Image)
          ;(#/setBezelStyle: Native-Control #$NSRegularSquareBezelStyle)
          (#/setBezelStyle: Native-Control #$NSThickerSquareBezelStyle)
          (#/setTitle: Native-Control (native-string (text Self)))))
      Native-Control)))


(objc:defmethod (#/mouseDown: :void) ((self native-jog-button) event)
  (declare (ignore event))
  (setf (is-jog-active (lui-view Self)) t)
  (process-run-function
   '(:name "Jog Button Thread" )
   #'(lambda ()
       ;; start jog in separate thread to avoid delay of slider knob move
       (start-jog (lui-view Self))
       ;; as long as mouse is down keep running control action at interval frequency
       (loop
         (unless (is-jog-active (lui-view Self))  (return))
         (catch-errors-nicely ("user is pressing button")
           ;; better to activate the action in the main thread!!
                              
           (in-main-thread ()
             (#/activateAction (#/target Self))
                              
                              
           (sleep (action-interval (lui-view Self))))))))
  ;; this actually does the mouse tracking until mouse up
  ;(call-next-method Event)
  ;; mouse is up
  ;(stop-jog (lui-view Self))
  )


(objc:defmethod (#/mouseUp: :void) ((self native-jog-button) event)
  (declare (ignore event))
  (stop-jog (lui-view Self)))


;__________________________________
; LABEL                            |
;__________________________________/

(defclass NATIVE-LABEL (ns:ns-text-view)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod MAKE-NATIVE-OBJECT ((Self label-control))
  (let ((Native-Control (make-instance 'native-label :lui-view Self)))  ;; NSText is not actually a control, would NSTextField be better?
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame))
    (#/setDrawsBackground: Native-Control nil)
    (#/setString: Native-Control (native-string (text Self)))
    (ecase (align Self)
      (:left (#/alignLeft: Native-Control Native-Control))
      (:center (#/alignCenter: Native-Control Native-Control))
      (:right (#/alignRight: Native-Control Native-Control))
      (:justified (#/alignJustified: Native-Control Native-Control)))
    (#/setEditable: Native-Control #$NO)
    (#/setSelectable: Native-Control #$NO)
    (unless (zerop (size self))
      (#/setFont: Native-Control (if (bold Self)
                                   (#/boldSystemFontOfSize: ns:ns-font (size self))
                                   (#/systemFontOfSize: ns:ns-font (size self)))))
    Native-Control))


(defmethod (SETF TEXT) :after (Text (Self label-control))
  (#/setString: (native-view Self) (native-string Text)))


(defmethod (SETF WIDTH) :after (Width (Self label-control))
   (ns:with-ns-size (Size Width (height Self))
     (#/setFrameSize: (native-view Self) Size)))


;__________________________________
; Editable TEXT                    |
;__________________________________/

(defclass NATIVE-EDITABLE-TEXT (ns:ns-text-field)
  ((lui-view :accessor lui-view :initarg :lui-view)
   (text-before-edit :accessor text-before-edit :initform "" :documentation "sometimes it will be nescisary to remember the value of the text field before a text field is editted and restore this value if a bad value is entered"))
  (:metaclass ns:+ns-object))


(defclass NATIVE-SECURE-EDITABLE-TEXT (ns:ns-secure-text-field)
  ((lui-view :accessor lui-view :initarg :lui-view)
   (text-before-edit :accessor text-before-edit :initform "" :documentation "sometimes it will be nescisary to remember the value of the text field before a text field is editted and restore this value if a bad value is entered"))
  (:metaclass ns:+ns-object))


(defmethod MAKE-NATIVE-OBJECT ((Self editable-text-control))
  (let ((Native-Control (if (secure self)  (make-instance 'native-secure-editable-text :lui-view Self) (make-instance 'native-editable-text :lui-view Self))))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame)
      (#/setDrawsBackground: Native-Control nil)
      (#/setStringValue: Native-Control (native-string (text Self)))
      (#/setEditable: Native-Control #$YES)
      (unless (zerop (size Self))
        (#/setFont: Native-Control (#/systemFontOfSize: ns:ns-font (size self))))
      #| (ecase (align Self)
        (:left (#/alignLeft: Native-Control Native-Control))
        (:center (#/alignCenter: Native-Control Native-Control))
        (:right (#/alignRight: Native-Control Native-Control))
        (:justified (#/alignJustified: Native-Control Native-Control))) |# )
    Native-Control))


(objc:defmethod (#/becomeFirstResponder  :<BOOL>) ((self native-editable-text))
  (call-next-method))


(objc:defmethod (#/textDidChange: :void) ((self native-editable-text) Notification) 
  (let ((lui-view (lui-view self)))
    (if (validate-text-change lui-view  (ccl::lisp-string-from-nsstring  (#/stringValue self)))
      (setf (validation-text-storage lui-view) (value lui-view))
      (progn 
        (#_NSBeep)
        (#/setStringValue: self (native-string  (if (stringp (validation-text-storage lui-view)) (validation-text-storage lui-view) (write-to-string (validation-text-storage lui-view)))))
        (display lui-view))))
  (call-next-method Notification))
 

(objc:defmethod (#/textShouldBeginEditing: :<BOOL>) ((self native-editable-text) Notification)
  (setf (text-before-edit self) (ccl::lisp-string-from-nsstring (#/stringValue self)))
  (call-next-method Notification))


(objc:defmethod (#/textShouldEndEditing: :<BOOL>) ((self native-editable-text) Notification)
  (let ((lui-view (lui-view self)))
    (unless (validate-final-text-value lui-view (value lui-view)) 
      (setf (value lui-view) (text-before-edit self))))
  (call-next-method Notification))


(defmethod (setf text) :after (Text (Self editable-text-control))
  (#/setStringValue: (native-view Self) (native-string text)))


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
; Text View                        |
;__________________________________/

(defclass NATIVE-TEXT-VIEW (ns:ns-text-view)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod MAKE-NATIVE-OBJECT ((Self text-view-control))
  (let ((Native-Control (make-instance 'native-text-view :lui-view Self) ))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame)
      (#/setDrawsBackground: Native-Control nil)
      (#/setString: Native-Control (native-string (text Self)))
      (#/setEditable: Native-Control #$YES)
      (unless (zerop (size Self))
        (#/setFont: Native-Control (#/systemFontOfSize: ns:ns-font (size self))))
      #| (ecase (align Self)
        (:left (#/alignLeft: Native-Control Native-Control))
        (:center (#/alignCenter: Native-Control Native-Control))
        (:right (#/alignRight: Native-Control Native-Control))
        (:justified (#/alignJustified: Native-Control Native-Control))) |# )
    Native-Control))


(defmethod (setf text) :after (Text (Self text-view-control))
  (#/setStringValue: (native-view Self) (native-string text)))


(defmethod VALUE ((Self text-view-control))
  (ccl::lisp-string-from-nsstring 
   (#/string (native-view Self))))


(defmethod (setf VALUE)  (Text (Self text-view-control))
  (#/setString: (native-view Self) (native-string Text)))


(defmethod MAP-SUBVIEWS ((Self text-view-control) Function &rest Args)
  (declare (ignore Function Args))
  ;; no Cocoa digging
  )


(defmethod SUBVIEWS ((Self text-view-control))
  ;; no Cocoa digging
  )


(defmethod INITIALIZE-EVENT-HANDLING ((Self text-view-control))
  (declare (ignore self))
  ;; do nothing
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
      (#/setDrawsBackground: Native-Control #$NO)
      (#/setEditable: Native-Control #$NO)
      (#/setBezeled: Native-Control #$NO)
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
; Progress Indicator               |
;__________________________________/

(defclass NATIVE-PROGRESS-INDICATOR-CONTROL (ns:ns-progress-indicator)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod MAKE-NATIVE-OBJECT ((Self progress-indicator-control))
  (let ((Native-Control (make-instance 'native-progress-indicator-control :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame)
      (#/setIndeterminate: Native-Control #$YES))
  Native-Control))


(defmethod INITIALIZE-EVENT-HANDLING ((Self progress-indicator-control))
  (declare (ignore self)))


(defmethod START-ANIMATION ((self progress-indicator-control))
  (#/startAnimation: (native-view self) (native-view self)))


(defmethod STOP-ANIMATION ((Self progress-indicator-control) &key Stop-Function)
  (declare (ignore Stop-Function))
  (#/stopAnimation: (native-view Self) (native-view Self)))
    
;__________________________________
; Determinate Progress Indicator   |
;__________________________________/

(defclass NATIVE-DETERMINATE-PROGRESS-INDICATOR-CONTROL (ns:ns-progress-indicator)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod MAKE-NATIVE-OBJECT ((Self determinate-progress-indicator-control))
  (let ((Native-Control (make-instance 'native-determinate-progress-indicator-control :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame)
      (#/setIndeterminate: Native-Control #$NO)
      (#/setMinValue: Native-Control (min-value self))
      (#/setMaxValue: Native-Control (max-value self)))
  Native-Control))


(defmethod INITIALIZE-EVENT-HANDLING ((Self determinate-progress-indicator-control))
  (declare (ignore self)))


(defmethod INCREMENT-BY ((self determinate-progress-indicator-control) double)
  "This mthod will increment the determinate progress indicator by the given amount double"
  (#/incrementBy: (native-view self) double))

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
      (ccl::with-autorelease-pool
          (let ((Image #-cocotron (#/autorelease (#/initByReferencingFile: (#/alloc ns:ns-image) (native-string (source Self))))
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
              (#/initWithFrame: Native-Control Frame)
              (cond
               ((downsample self)
                (ns:with-ns-size (Size (width Self) (height Self))
                  (let ((resized-image  (#/initWithSize: (#/alloc ns:ns-image) size)))
                    (ns:with-ns-rect (orig-rect 0 0 (NS:NS-SIZE-WIDTH (#/size Image)) (NS:NS-SIZE-HEIGHT (#/size Image)))
                      (#/lockFocus resized-image)
                      (#/drawInRect:fromRect:operation:fraction: image Frame orig-rect #$NSCompositeCopy 1.0)
                      (#/unlockFocus resized-image)
                      (#/setImage: Native-Control resized-image)))))
               (t
                (#/setImage: Native-Control image))))
            (if (scale-proportionally self)
              (#/setImageScaling: Native-Control #$NSScaleProportionally)
              (#/setImageScaling: Native-Control #$NSScaleToFit)))))
     (t
      (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
        (#/initWithFrame: Native-Control Frame))))
    Native-Control))


(defmethod change-image ((self image-control) image-name)
  (setf (src self) image-name)
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
    (#/setImage: (Native-view self) Image)
    (#/setNeedsDisplay: (native-view self) #$YES)))


(defmethod SET-IMAGE-FROM-IMAGE ((self image-control) ns-image)
  "Setf the image of this image view to the ns-image provided ns-image MUST be an ns-image NOT an image-control"
  (#/setImage: (Native-view self) ns-Image))

;__________________________________
; IMAGE                            |
;__________________________________/

(defclass native-clickable-image (ns:ns-control)
  ((lui-view :accessor lui-view :initarg :lui-view)
   (native-image :accessor native-image :initform nil :documentation "this control's native-image")
   )
  (:metaclass ns:+ns-object))


(defmethod make-native-object ((Self clickable-image-control))
  (let ((Native-Control (make-instance 'native-clickable-image :lui-view Self))) 
    ;; no problem if there is no source, just keep an empty view
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame))
    (ccl::with-autorelease-pool
        (let ((Image #-cocotron (#/retain (#/initByReferencingFile: (#/alloc ns:ns-image) (native-string (source Self))))
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
            (#/initWithFrame: Native-Control Frame)
            (cond
             ((downsample self)
              (ns:with-ns-size (Size (width Self) (height Self))
                (let ((resized-image  (#/initWithSize: (#/alloc ns:ns-image) size)))
                  (ns:with-ns-rect (orig-rect 0 0 (NS:NS-SIZE-WIDTH (#/size Image)) (NS:NS-SIZE-HEIGHT (#/size Image)))
                    (#/lockFocus resized-image)
                    (#/drawInRect:fromRect:operation:fraction: image Frame orig-rect #$NSCompositeCopy 1.0)
                    (#/unlockFocus resized-image)
                    (setf (native-image Native-Control) resized-image)))))
             (t
              (setf (native-image Native-Control) image))))))
    Native-Control))


(objc:defmethod (#/isFlipped :<BOOL>) ((Self native-clickable-image))
  ;; Flip to coordinate system to 0, 0 = upper left corner
  #$NO)


(objc:defmethod (#/acceptsFirstResponder :<BOOL>) ((Self native-clickable-image))
  ;; Flip to coordinate system to 0, 0 = upper left corner
  #$YES)

(objc:defmethod (#/mouseDown: :void) ((self native-clickable-image) event)
  (let ((mouse-loc (#/locationInWindow event)));(#/locationInWindow event)))    
    (multiple-value-bind (test-x test-y)
                             (convert-from-window-coordinates-to-view-coordinates (lui-view self) (truncate (pref mouse-loc :<NSP>oint.x))(truncate (pref mouse-loc :<NSP>oint.y)))
      (view-left-mouse-down-event-handler (lui-view self)  (truncate test-x)(truncate (- (height (lui-view self)) test-y))))))

#|
(objc:defmethod (#/mouseDragged: :void) ((self native-clickable-image) event)
  (let ((mouse-loc (#/locationInWindow event)));(#/locationInWindow event)))    
    (multiple-value-bind (test-x test-y)
                             (convert-from-window-coordinates-to-view-coordinates (lui-view self) (truncate (pref mouse-loc :<NSP>oint.x))(truncate (pref mouse-loc :<NSP>oint.y)))
      (view-left-mouse-dragged-event-handler (lui-view self)  (truncate test-x)(truncate (- (height (lui-view self)) test-y)) (truncate (#/deltaX Event)) (truncate (#/deltaY Event))))))
|#

(objc:defmethod (#/drawRect: :void) ((self native-clickable-image) (rect :<NSR>ect))
  (ns:with-ns-rect (Frame (NS:NS-RECT-X rect) (NS:NS-RECT-Y rect) (- (NS:NS-RECT-WIDTH rect) 1)(NS:NS-RECT-HEIGHT rect))
    (#/drawInRect:fromRect:operation:fraction: (native-image self) frame #$NSZeroRect #$NSCompositeCopy 1.0)))

#|
(objc:defmethod (#/mouseDown: :void) ((self native-clickable-image) event)
  (let* ((mouse-loc (#/locationInWindow event))
         (x (- (ns:ns-point-x  (#/locationInWindow event)) (x (lui-view self))))
         ;(y (+ (- (height (lui-view self))(ns:ns-point-y  (#/locationInWindow event))   ) (y (lui-view self))))
         ;(y (- (ns:ns-point-y  (#/locationInWindow event)) (y (lui-view self))))
         (y (- (height (window (lui-view self))) (ns:ns-point-y  (#/locationInWindow event)) (y (lui-view self))))
         )
        
        
    
    (view-left-mouse-down-event-handler (lui-view self) x y)
    
    )

  )


(defmethod VIEW-LEFT-MOUSE-DOWN-EVENT-HANDLER ((self clickable-image-control) x y)
  (print "NOTHINg TO DO")
  )
|#
;__________________________________
; Color Well                       |
;__________________________________/

(defclass NATIVE-COLOR-WELL (ns:ns-color-well)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(objc:defmethod (#/mouseDown: :void) ((self native-color-well) event)
  (call-next-method event)
  (unless (#/isActive self)
    (#/activate: self #$YES)))

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
    (#/getRed:green:blue:alpha:  (#/color (Native-View self)) r g b a)
    (truncate (* (pref r #>CGFloat) 255))))


(defmethod GET-GREEN ((self color-well-control))
  (rlet ((r #>CGFloat)
         (g #>CGFloat)
         (b #>CGFloat)
         (a #>CGFloat))
    (#/getRed:green:blue:alpha:  (#/color (Native-View self)) r g b a)
    (truncate (* (pref g #>CGFloat) 255))))


(defmethod GET-BLUE ((self color-well-control))
  (rlet ((r #>CGFloat)
         (g #>CGFloat)
         (b #>CGFloat)
         (a #>CGFloat))
    (#/getRed:green:blue:alpha:  (#/color (Native-View self)) r g b a)
    (truncate (* (pref b #>CGFloat) 255))))


(defmethod GET-ALPHA ((self color-well-control))
  (rlet ((r #>CGFloat)
         (g #>CGFloat)
         (b #>CGFloat)
         (a #>CGFloat))
    (#/getRed:green:blue:alpha:  (#/color (Native-View self)) r g b a)
    (truncate (* (pref a #>CGFloat) 255))))


(defmethod SET-COLOR ((Self color-well-control) &key (Red 0.0) (Green 0.0) (Blue 0.0) (Alpha 1.0))
  ;; keep a native color instead of creating a new one for each display
  (#/setColor: (native-view Self) (#/retain (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color Red Green Blue Alpha))))

;__________________________________
; Color Well Button                |
;__________________________________/

(defclass NATIVE-COLOR-WELL-BUTTON (ns:ns-button)
  ((lui-view :accessor lui-view :initarg :lui-view)
   (native-color :accessor native-color :initform nil :initarg :native-color))
  (:metaclass ns:+ns-object))


(objc:defmethod (#/drawRect: :void) ((self native-color-well-button) (rect :<NSR>ect))
  (call-next-method rect)
  (ns:with-ns-rect (Frame (+ (NS:NS-RECT-X rect)  (* .25 (NS:NS-RECT-WIDTH rect))) (+ (NS:NS-RECT-Y rect)  (* .25 (NS:NS-RECT-HEIGHT rect))) (* .5 (NS:NS-RECT-WIDTH rect)) (* .5 (NS:NS-RECT-HEIGHT rect)))
    (#/set (native-color self))
    (#/fillRect: ns:ns-bezier-path frame)))


(defmethod MAKE-NATIVE-OBJECT ((Self color-well-button-control))
  (let ((Native-Control (make-instance 'native-color-well-button :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (multiple-value-bind (Red Blue Green)
                           (parse-rgb-from-hex self (color self))
        (let ((nscolor (#/colorWithDeviceRed:green:blue:alpha: ns:ns-color (/ red 255.0) (/ blue 255.0) (/ green 255.0)  1.0 ))
              (image (#/initWithSize: (#/alloc ns:ns-image) Frame))) 
          (setf (native-color Native-control) nscolor)
          (#/setBackgroundColor: image nscolor)
          (#/initWithFrame: Native-Control Frame)
          (#/setButtonType: Native-Control #$NSOnOffButton)
          (#/setImagePosition: Native-Control #$NSImageOnly)
          (#/setImage: Native-Control image)
          (#/setBezelStyle: Native-Control #$NSShadowlessSquareBezelStyle)))
      ;; setup alpha control but keep in mind color panel is shared -> cannot mix alpha / no alpha
      Native-Control)))


(defmethod PARSE-RGB-FROM-HEX ((Self color-well-button-control) string)
  (values
   (read-from-string (concatenate 'string "#x" (subseq string 0 2)))   ;Red
   (read-from-string (concatenate 'string "#x" (subseq string 2 4)))   ;Blue
   (read-from-string (concatenate 'string "#x" (subseq string 4 6))))) ;Green


(defmethod GET-RED ((self color-well-button-control))
  (rlet ((r #>CGFloat)
         (g #>CGFloat)
         (b #>CGFloat)
         (a #>CGFloat))
    (#/getRed:green:blue:alpha: (native-color (native-view self)) r g b a)
    (truncate (* (pref r #>CGFloat) 255))))


(defmethod GET-GREEN ((Self color-well-button-control))
  (rlet ((r #>CGFloat)
         (g #>CGFloat)
         (b #>CGFloat)
         (a #>CGFloat))
    (#/getRed:green:blue:alpha: (native-color (native-view self)) r g b a)
    (truncate (* (pref g #>CGFloat) 255))))


(defmethod GET-BLUE ((Self color-well-button-control))
  (rlet ((r #>CGFloat)
         (g #>CGFloat)
         (b #>CGFloat)
         (a #>CGFloat))
    (#/getRed:green:blue:alpha: (native-color (native-view self)) r g b a)
    (truncate (* (pref b #>CGFloat) 255))))


(defmethod GET-ALPHA ((Self color-well-button-control))
  (rlet ((r #>CGFloat)
         (g #>CGFloat)
         (b #>CGFloat)
         (a #>CGFloat))
    (#/getRed:green:blue:alpha: (native-color (native-view self)) r g b a)
    (truncate (* (pref a #>CGFloat) 255))))


(defmethod SET-COLOR ((Self color-well-button-control) &key (Red 0.0) (Green 0.0) (Blue 0.0) (Alpha 1.0))
  ;; keep a native color instead of creating a new one for each display
  (setf (native-color (native-view self)) (#/retain (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color Red Green Blue Alpha)))
  (#/setNeedsDisplay: (native-view self) #$YES)
  (#/setState: (native-view self) #$NSOffState))


;__________________________________
; Web Browser                      |
;__________________________________/

#| Takes a long time to load: disable for now

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

|#
;__________________________________
; SET-ICON-AT-PATH                 |
;__________________________________/
#|
(defun SET-ICON-OF-FILE-AT-PATH (path-to-image path-to-file)
  (#/setIcon:forFile:options: (#/sharedWorkspace ns:ns-workspace)  (#/initByReferencingFile: (#/alloc ns:ns-image) (native-string (namestring (truename path-to-image)))) (lui::native-string (namestring path-to-file)) 0  ))
|#


(defun ENCODE-URL-CHARS (Url) "
  in:  URL {string}.
  out: Compliant-URL {string}.
  Replace non URL compliant characters such as strings with URL equivalents."
  (with-output-to-string (Output)
    (with-input-from-string (Input URL)
      (loop
        (let ((Char (or (read-char Input nil nil))))
          (unless Char (return))
          (case Char
            (#\space (princ "%20" Output))
            (#\! (princ "%21" Output))
            (#\" (princ "%22" Output))
            (#\# (princ "%23" Output))
            (#\$ (princ "%24" Output))
            (#\% (princ "%25" Output))
            (#\& (princ "%26" Output))
            (#\' (princ "%27" Output))
            (#\( (princ "%28" Output))
            (#\) (princ "%29" Output))
            (#\* (princ "%2A" Output))
            (#\+ (princ "%2B" Output))
            (#\, (princ "%2C" Output))
            (#\- (princ "%2D" Output))
            (#\. (princ "%2E" Output))
            (#\/ (princ "%2F" Output))
            (#\: (princ "%3A" Output))
            (#\; (princ "%3B" Output))
            (#\< (princ "%3C" Output))
            (#\= (princ "%3D" Output))
            (#\> (princ "%3E" Output))
            (#\? (princ "%3F" Output))
            (#\@ (princ "%40" Output))
            (#\[ (princ "%5B" Output))
            (#\\ (princ "%5C" Output))
            (#\] (princ "%5D" Output))
            (#\^ (princ "%5E" Output))
            (#\_ (princ "%5F" Output))
            (#\` (princ "%60" Output))
            (#\{ (princ "%7B" Output))
            (#\| (princ "%7C" Output))
            (#\} (princ "%7D" Output))
            (#\~ (princ "%7E" Output))
            (#\newline (princ "%0A" Output))
            (#\return (princ "%0D" Output))
            (#\tab (princ "%09" Output))
            (t (when (alphanumericp Char)
                 (princ Char Output)))))))))


;__________________________________
; OPEN-URL                         |
;__________________________________/

(defun OPEN-URL (url)
  "IN: A URL in the form of a string
  OUT: the user's default should open to the specified URL"
  (#/openURL: (#/sharedWorkspace ns:ns-workspace) (#/URLWithString: ns:ns-url (native-string url))))


;___________________________________
; LINK-CONTROL                      |
;__________________________________/

(defclass NATIVE-LINK (ns:ns-text-view)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object))


(defmethod MAKE-NATIVE-OBJECT ((Self link-control))
  (let ((Native-Control (make-instance 'native-link :lui-view Self)))  ;; NSText is not actually a control, would NSTextField be better?
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame))
    (#/setDrawsBackground: Native-Control nil)
    (#/setString: Native-Control (native-string (text Self)))
    (#/setTextColor: Native-Control (#/colorWithDeviceRed:green:blue:alpha: ns:ns-color 0.0 0.0 255.0 1.0))
    (ecase (align Self)
      (:left (#/alignLeft: Native-Control Native-Control))
      (:center (#/alignCenter: Native-Control Native-Control))
      (:right (#/alignRight: Native-Control Native-Control))
      (:justified (#/alignJustified: Native-Control Native-Control)))
    (#/setEditable: Native-Control #$NO)
    (#/setSelectable: Native-Control #$NO)
    (unless (zerop (size self))
      (#/setFont: Native-Control (if (bold Self)
                                   (#/boldSystemFontOfSize: ns:ns-font (size self))
                                   (#/systemFontOfSize: ns:ns-font (size self)))))
    Native-Control))


(objc:defmethod (#/mouseDown: :void) ((self native-link) Event)
  (declare (ignore Event))
  (when (action (lui-view Self))
    (funcall (action (lui-view Self)) (window (lui-view Self)) (target (lui-view Self))))
  (open-url (url (lui-view Self))))


(defmethod initialize-event-handling ((Self link-control))
  (declare (ignore self))
  ;; do nothing
  )


(defmethod (setf text) :after (Text (Self link-control))
  (#/setString: (native-view Self) (native-string text)))



;__________________________________
; Show PopUp                       |
;__________________________________/

(defun SHOW-STRING-POPUP (window list &key selected-item container item-addition-action-string-list)
  (unless (or list item-addition-action-string-list) (return-from show-string-popup nil))
  (let ((longest-string "")
        (text-buffer 6)) ;;Need a small buffer to make sure the longest string in the list does not get clipped
    (dolist (string list)
      (when (> (length string) (length longest-string))
        (setf longest-string string)))
    (let ((Pop-up (make-instance 'popup-button-control  :container container :width (+ text-buffer (truncate (ns:ns-size-width (#/sizeWithAttributes: (lui::native-string longest-string) nil)))) :height 1 :x   (- (rational (NS:NS-POINT-X (#/mouseLocation ns:ns-event)))(x window))  :y   (-  (- (NS:NS-RECT-HEIGHT (#/frame (#/mainScreen ns:ns-screen)))(NS:NS-POINT-Y (#/mouseLocation ns:ns-event)))(y window))  )))
    (dolist (String list)
      (add-item Pop-Up String nil))
      (if item-addition-action-string-list
        (add-item Pop-Up (first item-addition-action-string-list) (second item-addition-action-string-list)))
    (add-subviews window Pop-up)
    (if selected-item
      (#/selectItemWithTitle: (native-view pop-up) (native-string selected-item))
      (#/selectItemWithTitle: (native-view pop-up) nil))
    ;(#/setTransparent: (native-view Pop-Up) #$YES)
    (#/performClick:  (native-view Pop-up) +null-ptr+)
      (let ((title (#/titleOfSelectedItem (native-view Pop-Up))))
        (#/removeFromSuperview (native-view Pop-up))
        (unless (%null-ptr-p title)
          (ccl::lisp-string-from-nsstring  title))))))


