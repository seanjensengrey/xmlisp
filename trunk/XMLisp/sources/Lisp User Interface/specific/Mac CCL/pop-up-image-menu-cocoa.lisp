(in-package :lui)

;;_________________________________
;;Pop Up Image Menu Cocoa          |
;;_________________________________

(defmethod INITIALIZE-INSTANCE :after  ((Self pop-up-image-menu) &rest Initargs)
  (declare (ignore initargs))
  (unless (name-of-selection self)
    (setf (name-of-selection self) (first (image-names self))))
  (unless (find (name-of-selection self) (image-names self):test #'equal)
    (progn
      (warn "Cannot find the given name-of-selection in image-names, setting name-of-selection to first item in image-names") 
      (setf (name-of-selection self) (first (image-names self))))) 
  (unless (index-of-selection self)
    (setf (index-of-selection self) (position (name-of-selection self)(image-names Self):test #'equal)))
  (let ((Image (#/alloc ns:ns-image))) 
    (unless (probe-file (image-name-pathname self (concatenate 'string (first (image-names self)) ".png")))
      (error "Cannot load file ~A does not exist" (native-path (image-pathname self) String)))
    (#/initWithContentsOfFile: Image (native-string (image-name-pathname self (concatenate 'string (first (image-names self)) ".png"))) ); "lui:resources;buttons;"  "bevelButton-off-popup.png"))) ;(image-name-pathname self "bevelButton-off-popup")))
    (let ((width(+ 6 ( ns::ns-size-width (#/size Image)))) (height (+ 6 (ns::ns-size-height (#/size Image)))))
      (setf (image-height self) height)
      (setf (image-width self) width)
      (setf (width self) (* width (number-of-columns self)))
      (setf (height self) (* height (number-of-rows self)))
      
      (if (find  (name-of-selection self)(image-names self):test #'equal)      
        (multiple-value-bind (x-selection-index y-selection-index)
                             (get-selection-indices self)
          (setf (window-offset-x self) (* x-selection-index (image-width self)))
          (setf (window-offset-y self) (*  (- (- (number-of-rows self)  y-selection-index)1) (image-width self))))
        (warn "Could not find the name-of-selection in image-names")))))


(defmethod DISPLAY-POP-UP-MENU ((Self pop-up-image-menu) &key (x (x self)) (y (y self)))
  (setf (x self) x)
  (setf (y self) y)
  (multiple-value-bind (x-selection-index y-selection-index)
                             (get-selection-indices self)
          (setf (window-offset-x self) (* x-selection-index (image-width self)))
          (setf (window-offset-y self) (*  (- (- (number-of-rows self)  y-selection-index)1) (image-width self))))
  (setf (window-offset-x self) (+ (window-offset-x self) (/(image-width self)2)))
  (setf (window-offset-y self) (+ (window-offset-y self)(/(image-width self)2)))
  (format t "x-offset= ~S y-offset= ~S" (window-offset-x self)(window-offset-y self))
  (with-simple-restart (cancel-pop "Stop trying to pop up ~s" Self)
    (in-main-thread ()
      (ccl::with-autorelease-pool
          (let ((Window (make-instance 'popup-window
                          :lui-window Self
                          :with-content-rect (ns:make-ns-rect (- (x self) (window-offset-x self)) (- (screen-height nil) (+ (y self) (window-offset-y self))) (width Self) (height Self))
                          :style-mask 0
                          :backing #$NSBackingStoreBuffered
                          :defer t)))
            (setf (native-window Self) Window)  ;; need to have this reference for the delegate to be in place
            (setf (native-view Self) (make-instance 'popup-window-view :lui-window Self ))              
            ;; setup delegate
            (setf (delegate Window) (make-instance 'popup-delegate :lui-window Self))
            (#/setDelegate: Window (delegate Window))
            ;; content view
            (#/setContentView: Window (#/autorelease (native-view Self)))
            (#/setHasShadow: Window #$YES)
            (#/setAcceptsMouseMovedEvents: (native-window Self) #$YES)
            (let ((current-row 0) (current-col 0))
              (let ((i 0) )
                (dolist (String (image-names self))
                  (setf String (concatenate 'string String ".png"))
                  (let ((Button (make-instance 'indexed-button)))                       
                    (let ((NS-Image (#/alloc ns:ns-image)))
                      (ns:with-ns-rect (Frame (* (image-width self) current-col) (* (image-height self) current-row) (image-width self) (image-height self))                                      
                        (#/initWithFrame: Button Frame)
                        (unless (probe-file (native-path (image-pathname self) String))
                          (error "Cannot load file ~A does not exist" (native-path (image-pathname self) String)))
                        (#/initWithContentsOfFile: NS-Image  (native-string (native-path (image-pathname self) String)))
                        (#/setButtonType: Button #$NSOnOffButton) 
                        (#/setBezelStyle: Button #$NSShadowlessSquareBezelStyle)
                        (#/setShowsBorderOnlyWhileMouseInside: Button #$YES)
                        (#/setImagePosition: Button #$NSImageOnly)                                   
                        (#/setImage: Button NS-Image)     
                        (#/setTarget: Button 
                                      (make-instance 'native-target 
                                        :native-control Button ;(native-view Self)
                                        :lui-control Self))
                        (#/setAction: Button (objc::@selector #/buttonAction))
                        (setf (index Button) i)
                        (#/addSubview: (native-view self) Button))))
                  (incf i)
                  (incf current-col)
                  (if (eql current-col (number-of-columns Self) )
                    (progn
                      (setf current-col 0)
                      (incf current-row))))))
            (#/makeKeyAndOrderFront: Window Window)
            (#/runModalForWindow: (#/sharedApplication ns:ns-application) Window)
            (#/close (native-window self))
            (elt (image-names self) (index-of-selection self)))))))


(defmethod CLOSE-WINDOW ((Self pop-up-image-menu))
  (#/stopModal (#/sharedApplication ns:ns-application)))


(objc:defmethod (#/buttonAction :void) ((self native-target))
  (setf (index-of-selection (lui-control Self)) (index (native-control self)))
  (close-window (lui-control self)))


(defclass INDEXED-BUTTON (ns:ns-button)
  ((index :accessor index :initform 200  :initarg :index :documentation "index of button"))
  (:metaclass ns:+ns-object
	      :documentation "Index Button"))


;;_______________________________
;; Easy Dipslay Helper Functions|
;; for General Pop Up Menu      |
;;_______________________________

(defun DISPLAY-POP-UP (List)
  (if (< 1 (length List))
    (make-pop-up List)
    (warn "Cannot make a popup menu with an empty list")))


(defun MAKE-POP-UP (List)
  (let ((*Pop-Up-Menu* (make-instance 'pop-up-image-menu :images List :name-of-selection   (First List))))
    (display-Pop-Up-menu *Pop-Up-Menu* )))


;;_______________________________
;; Popup window                  |
;;_______________________________

(defclass POPUP-WINDOW (ns:ns-window)
  ((lui-window :accessor lui-window :initarg :lui-window)
   (delegate :accessor delegate :initform nil :initarg :delegate :documentation "event delegate"))
  (:metaclass ns:+ns-object
	      :documentation "Native window"))


(objc:defmethod (#/sendEvent: :void) ((Self popup-window) Event)
  ;(print (native-to-lui-event-type (#/type Event)))
  (call-next-method Event))


#|  Attemp to track off window mouse events
(objc:defmethod (#/resignKeyWindow :void) ((Self popup-window))
  ;(print "resigned!!!!"))

  (objc:defmethod (#/resignMainWindow :void) ((Self popup-window))
  ;(print "resigned!!!!"))                                        
    (objc:defmethod (#/mouseMoved: :void) ((self popup-window) Event))
|#
;;_______________________________
;; Popup window view             |
;;_______________________________

(defclass POPUP-WINDOW-VIEW (native-window-view)
  ()
  (:metaclass ns:+ns-object
              :documentation "popup-window-view"))
    

(objc:defmethod (#/mouseDown: :void) ((self popup-window-view) Event)
  (declare (ignore Event)))


(objc:defmethod (#/mouseUp: :void) ((self popup-window-view) Event)
  (declare (ignore Event))
  (#/stopModal (#/sharedApplication ns:ns-application)))


(objc:defmethod (#/mouseDragged: :void) ((self popup-window-view) Event)
  (declare  (ignore Event)))



(defclass POPUP-DELEGATE(window-delegate)
  ()
  (:metaclass ns:+ns-object
	      :documentation "window delegate"))

(objc:defmethod (#/windowDidMove: :void) ((self popup-delegate) Notifaction)
  (declare (ignore Notifaction)))


(objc:defmethod (#/windowDidResignMain: :void) ((self popup-delegate) Notifaction)
  (declare (ignore Notifaction)))


(objc:defmethod (#/windowDidBecomeKey: :void) ((self popup-delegate) Notifaction)
   (declare (ignore Notifaction)))

    