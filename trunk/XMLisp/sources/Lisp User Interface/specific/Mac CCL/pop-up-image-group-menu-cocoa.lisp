(in-package :lui)

;--------------------------------
; GROUP MENU POPUP WINDOW       |
;________________________________


(defmethod GET-WINDOW-WIDTH ((Self pop-up-image-group-menu))
  (let ((longest-row 0))
    (dolist (group (image-names self))
      (let* ((group-name (first group)) 
             (size (#/sizeWithAttributes: (native-string group-name) (#/dictionary ns:ns-dictionary))))
        (if (< (label-width self) (+ 15 (ns::ns-size-width size)))
          (setf (label-width self) (+ 15 (ns::ns-size-width size))))
        (let ((group-length (label-width self)))
          (setf group-length (- (+ group-length (* (+ 1 (length (second group))) (image-width self))) (image-width self)))
          (if (> group-length longest-row)
            (setf longest-row group-length)))))
    (return-from GET-WINDOW-WIDTH longest-row)))


(defmethod INITIALIZE-INSTANCE :after  ((Self pop-up-image-group-menu) &rest Initargs)
  (declare (ignore initargs))
  (setf (window-height self) (+ (image-preview-height self)(shape-text-box-height self) (* (image-height self) (length (image-names self)))))
  (setf (width self) (get-window-width self)))


(defclass GROUP-MENU-POPUP-WINDOW (popup-window)
  ((groups :accessor groups :initform nil :documentation "a list of the groups contained in this window"))
    (:metaclass ns:+ns-object
	      :documentation "Native window"))

#|
(objc:defmethod (#/canBecomeKeyWindow :boolean) ((self group-menu-popup-window))
  #$YES)
|#

(objc:defmethod (#/mouseMoved: :void) ((self group-menu-popup-window) Event)
  (let ((mouse-x (NS:NS-POINT-X (#/locationInWindow event)))
        (mouse-y (NS:NS-POINT-Y (#/locationInWindow event))))
    (let ((subviews (gui::list-from-ns-array (#/subviews (#/contentView self)))))  
      (dolist (subview subviews)
        
        
        (when (equal (type-of subview) 'LUI::INDEXED-IMAGE-VIEW )
          
          (let ((x-subview (NS:NS-RECT-X (#/frame subview)))
                (y-subview (NS:NS-RECT-Y (#/frame subview)))
                (width-subview  (NS:NS-RECT-WIDTH (#/frame subview)))
                (height-subview  (NS:NS-RECT-HEIGHT (#/frame subview))))
            (if (and (<= x-subview mouse-x  (+ x-subview width-subview))
                     (<= y-subview mouse-y (+ y-subview height-subview)))
              (progn
                (#/setString: (shape-text-box (lui-window self)) (native-string (string-capitalize (name subview))))
                (setf (selected subview) t)
                (#/setImage: (image-preview-view (lui-window self)) (#/image subview)))
              (setf (selected subview) nil))))
        (when (equal (type-of subview) 'LUI::INDEXED-TEXT-VIEW  )
          (let ((x-subview (NS:NS-RECT-X (#/frame subview)))
                (y-subview (NS:NS-RECT-Y (#/frame subview)))
                (width-subview  (NS:NS-RECT-WIDTH (#/frame subview)))
                (height-subview  (NS:NS-RECT-HEIGHT (#/frame subview))))
            (if (and (<= x-subview mouse-x  (+ x-subview width-subview))
                     (<= y-subview mouse-y (+ y-subview height-subview)))
              (progn
                (let ((image-view (first (image-views subview))))
                  (#/setString: (shape-text-box (lui-window self)) (native-string (string-capitalize (name image-view))))
                  (setf (selected image-view) t)
                  (#/setImage: (image-preview-view (lui-window self)) (#/image image-view))
                  (call-next-method event)
                  (#/setNeedsDisplay: (#/contentView self) #$YES)
                  (return))))))))
    (call-next-method event)
    (#/setNeedsDisplay: (#/contentView self) #$YES)))


(defclass POPUP-GROUP-WINDOW-VIEW (popup-window-view)
  ((lui-winodw :accessor lui-window :initform nil :initarg :lui-window))
  (:metaclass ns:+ns-object
              :documentation "popup-group-window-view"))


(objc:defmethod (#/isFlipped :<BOOL>) ((self popup-group-window-view))
  ;; Flip to coordinate system to 0, 0 = upper left corner
  #$NO)


;--------------------------------
; INDEXED IMAGE VIEW             |
;________________________________


(defclass INDEXED-IMAGE-VIEW (ns:ns-image-view)
  ((index :accessor index :initform 200  :initarg :index :documentation "index of button")
   (lui-superview :accessor lui-superview :initform nil :initarg :lui-superview)
   (selected :accessor selected :initform nil)
   (name :accessor name :initform nil))
  (:metaclass ns:+ns-object
	      :documentation "Index Button"))


(objc:defmethod (#/drawRect: :void) ((self indexed-image-view) (rect :<NSR>ect)) 
  (call-next-method rect)  
  (if (selected self)
    (progn
      (#/set (#/blackColor ns:ns-color))
      (#/strokeRect: ns:ns-bezier-path rect)
      (#/set (#/whiteColor ns:ns-color))
      (ns:with-ns-rect (inside-rect (+ 1 (NS:NS-RECT-X rect)) (+ 1 (NS:NS-RECT-Y rect)) (-(NS:NS-RECT-WIDTH rect) 2) (-(NS:NS-RECT-HEIGHT rect) 2))
        (#/strokeRect: ns:ns-bezier-path inside-rect))))
  (setf (selected self) nil))


(objc:defmethod (#/mouseDown: :void) ((self indexed-image-view) Event)
  (call-next-method Event)
  (setf (index-of-selection (lui-superview Self)) (index self))
  (close-window (lui-superview self)))


;--------------------------------
; INDEXED LABEL                    |
;________________________________

(defclass INDEXED-TEXT-VIEW (ns:ns-text-field)
  ((index :accessor index :initform 200  :initarg :index :documentation "index of button")
   (lui-superview :accessor lui-superview :initform nil :initarg :lui-superview)
   (image-views :accessor image-views :initform nil :documentation "A list of indexed image-views ascoicated with this text-view's group"))
  (:metaclass ns:+ns-object
	      :documentation "Text view with and associated index"))

(objc:defmethod (#/mouseDown: :void) ((self indexed-text-view) Event)
  (call-next-method Event)
  (setf (index-of-selection (lui-superview Self)) (index self))
  (close-window (lui-superview self)))

;--------------------------------
; POP UP IMAGE GROUP MENU COCOA  |
;________________________________

(defmethod FIND-GROUP-BY-INDEX ((Self pop-up-image-group-menu) Index)
  (let ((i 0))
    (dolist (group (image-names self))
      (let ((group-name (first group)))
        (dolist (string (second group))
          (if (eql  i Index)
            (return-from FIND-GROUP-BY-INDEX (list group-name string) ))
          (incf i))))))      
        

(defmethod ENSURE-FITS-ON-SCREEN ((Self pop-up-image-group-menu))
  (let ((screen-height (ns:ns-rect-height (#/frame (#/mainScreen ns:ns-screen))))
        (toolbar-height #+cocotron 0 #-cocotron 20))
    (when (> (+ toolbar-height (y self) (window-height self)) screen-height)
      (setf (y self) (+  (y self) (- screen-height (+ toolbar-height (y self) (window-height self))))))))


(defmethod DISPLAY-POP-UP-MENU ((Self pop-up-image-group-menu) &key (x nil) (y nil))
  (if (and x y)
    (progn
      (setf (x self) x)
      (setf (y self) y))
    (progn
      (setf (x self) (NS:NS-POINT-X (#/mouseLocation ns:ns-event)))
      (setf (y self) (NS:NS-POINT-Y (#/mouseLocation ns:ns-event)))))
  (setf (window-height self) (+ (image-preview-height self)(shape-text-box-height self) (* (image-height self) (length (image-names self)))))
  (setf (width self) (get-window-width self))
  (ensure-fits-on-screen self)
  (with-simple-restart (cancel-pop "Stop trying to pop up ~s" Self)
    (in-main-thread ()
      (ccl::with-autorelease-pool
          (let ((Window (make-instance 'group-menu-popup-window
                          :lui-window Self
                          :with-content-rect (ns:make-ns-rect (x self) (y self)  (width Self) (window-height Self))
                          :style-mask 0
                          :backing #$NSBackingStoreBuffered
                          :defer t)))
            (#/setAcceptsMouseMovedEvents: Window #$YES)
            (setf (native-window Self) Window)  ;; need to have this reference for the delegate to be in place
            (setf (native-view Self) (make-instance 'popup-group-window-view :lui-window Self ))              
            ;; setup delegate
            (setf (delegate Window) (make-instance 'popup-delegate :lui-window Self))
            (#/setDelegate: Window (delegate Window))
            ;; content view
            (#/setContentView: Window (#/autorelease (native-view Self)))
            (#/setHasShadow: Window #$YES)         
            (let ((current-row 0) (current-col 0)(i 0))
              (dolist (group (image-names self))
                (let ((group-name (first group)) 
                      (list (elt group 1))             
                      (label (make-instance 'indexed-text-view)))
                  (ns:with-ns-rect (Frame (* (image-width self) current-col) (+ (image-preview-height self)(shape-text-box-height self)(* (image-height self) current-row)) (label-width self) #|(* (image-width self) 4)|# (image-height self))   
                    (#/initWithFrame: Label Frame)
                    (#/setTextColor: Label  (#/blackColor ns:ns-color))
                    (#/setStringValue: Label (native-string group-name))
                    (#/setEditable: Label #$NO)
                    (#/setSelectable: Label #$NO)
                    (#/setDrawsBackground: Label #$NO)
                    (#/setBezeled: Label #$NO)
                    (setf (lui-superview label) self)
                    (setf (index label) i)
                    (#/addSubview: (native-view self) Label))     
                  (dolist (Item list )                      
                    (let ((indexed-view (make-instance 'indexed-image-view))                       
                          (NS-Image (#/alloc ns:ns-image))
                          (image-pathname (image-pathname self))
                          (image-name nil)
                          (tooltip nil))
                      (ns:with-ns-rect (Frame (+ (label-width self) (* (image-width self) current-col)) (+ (image-preview-height self)(shape-text-box-height self)(* (image-height self) current-row)) (-(image-width self)(image-border-thickness self)) (-(image-height self)(image-border-thickness self)))                                      
                        (#/initWithFrame: indexed-view Frame)
                        (etypecase Item
                          (list
                           (setf image-name (second item)) ;(concatenate 'string (first Item) "." (image-file-extension self)))
                           ;(setf image-pathname (Second Item))
                           (setf tooltip (first item))
                           (unless (probe-file  image-name)
                             (warn "Cannot load file ~A does not exist" image-name))
                           (#/initWithContentsOfFile: NS-Image  (native-string  (namestring image-name))))
                          (string
                           (setf tooltip item)
                           (setf Item (concatenate 'string Item "." (image-file-extension self)))
                           (setf image-name Item)  
                           (unless (probe-file (native-path image-pathname image-name))
                             (warn "Cannot load file ~A does not exist" (native-path image-pathname image-name)))
                           (#/initWithContentsOfFile: NS-Image  (native-string (native-path image-pathname image-name)))))
                        (setf (name indexed-view) tooltip)
                        (setf (lui-superview indexed-view) self)                                 
                        (#/setImage: indexed-view NS-Image)   
                        (#/setImageScaling: indexed-view #$NSScaleToFit)
                        (setf (index indexed-view) i)
                        (if (image-views label)
                          (setf (image-views label) (append (image-views label) (list indexed-view)))
                          (setf (image-views label) (list indexed-view)))
                        (#/addSubview: (native-view self) indexed-view)))
                    (incf i)
                    (incf current-col))
                  (setf current-col 0)
                  (incf current-row))))
            (let ((shape-text-box (#/alloc ns:ns-text-view)))
              (ns:with-ns-rect (Frame 0 (image-preview-height self) (width self) (shape-text-box-height self))
                (#/initWithFrame: shape-text-box Frame)
                (#/setEditable: shape-text-box #$NO)
                (#/setSelectable: shape-text-box #$NO)
                (#/setBackgroundColor: shape-text-box (#/colorWithDeviceRed:green:blue:alpha: ns:ns-color .9 .9 .9 1.0));(#/lightGrayColor ns:ns-color))
                (#/setAlignment: shape-text-box #$NSCenterTextAlignment)
                (setf (shape-text-box self) shape-text-box)
                (#/addSubview: (native-view self) shape-text-box)))
            (let ((image-preview (#/alloc ns:ns-image-view)))
              (ns:with-ns-rect (Frame 0 0 (width self) (image-preview-height self))
                (#/initWithFrame: image-preview Frame)
                (#/addSubview: (native-view self) image-preview)
                
                (#/setImageScaling: image-preview 3)
                (setf (image-preview-view self) image-preview)
                ))
            (#/setAcceptsMouseMovedEvents: Window #$YES)
            (#/makeKeyAndOrderFront: Window Window)
            (#/runModalForWindow: (#/sharedApplication ns:ns-application) Window)
            (#/close (native-window self))
            (let ((selected-group (find-group-by-index self (index-of-selection self))))
              (etypecase (second selected-group)
                (string
                 (values (first selected-group) (second selected-group)))
                (list 
                 (values (first selected-group) (first (second selected-group)))))))))))


(objc:defmethod (#/buttonAction :void) ((self native-target))
  (setf (index-of-selection (lui-control Self)) (index (native-control self)))
  (close-window (lui-control self)))


(defmethod CLOSE-WINDOW ((Self pop-up-image-group-menu))
  (#/stopModal (#/sharedApplication ns:ns-application)))
    



