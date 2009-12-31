(in-package :lui)

;;_________________________________
;; General Pop Up Image Menu Cocoa |
;;_________________________________

(defmethod INITIALIZE-INSTANCE :after  ((Self general-pop-up-image-menu) &rest Initargs)
  (declare (ignore initargs))
  (if (eql nil (name-of-selection self))
    (setf (name-of-selection self) (first (image-names self))))
  (unless (find (name-of-selection self) (image-names self):test #'equal)
    (progn
      (warn "Cannot find the given name-of-selection in image-names, setting name-of-selection to first item in image-names") 
      (setf (name-of-selection self) (first (image-names self))))) 
  (if (eql nil (index-of-selection self))
    (setf (index-of-selection self) (position (name-of-selection self)(image-names Self):test #'equal)))
  (let ((Image (#/alloc ns:ns-image)))
    (#/initWithContentsOfFile: Image (native-string (image-name-pathname self (first (image-names self))))) ; "lui:resources;buttons;"  "bevelButton-off-popup.png"))) ;(image-name-pathname self "bevelButton-off-popup")))
    (let ((width(+ 6 ( ns::ns-size-width (#/size Image)))) (height (+ 6 (ns::ns-size-height (#/size Image)))))
      (setf (image-height self) height)
      (setf (image-width self) width)
      (setf (width self) (* width (number-of-columns self)))
      (setf (height self) (* height (number-of-rows self)))
      
      (if (find  (name-of-selection self)(image-names self):test #'equal)      
        (multiple-value-bind (x-selection-index y-selection-index)
                             (GET-SELECTION-INDICES self)
          (setf (window-offset-x self) (* x-selection-index (image-width self)))
          (setf (window-offset-y self) (*  (- (- (number-of-rows self)  y-selection-index)1) (image-width self))))
        (warn "Could not find the name-of-selection in image-names")))))

(defmethod DISPLAY-POP-UP-MENU ((Self general-pop-up-image-menu))
  (with-simple-restart (cancel-pop "Stop trying to pop up ~s" Self)
    (in-main-thread ()
                    (ccl::with-autorelease-pool
                        (let ((Window (make-instance 'popup-window
                                        :lui-window Self
                                        :with-content-rect (ns:make-ns-rect (- (x self) (window-offset-x self)) (- (y self) (window-offset-y self)) (width Self) (height Self))
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
                          
                          (#/setAcceptsMouseMovedEvents: (native-window Self) #$YES)
                          (let ((current-row 0) (current-col 0))
                            (let ((i 0) )
                              (dolist (String (image-names self))
                                (let ((Button (make-instance 'indexed-button)))                       
                                  (let ((NS-Image (#/alloc ns:ns-image)))
                                    (ns:with-ns-rect (Frame (* (image-width self) current-col) (* (image-height self) current-row) (image-width self) (image-height self))                                      
                                      (#/initWithFrame: Button Frame)
                                      (if (eql nil (probe-file (native-path (image-pathname self) String)))
                                        (warn "Cannot load file ~A does not exist" (native-path (image-pathname self) String)))
                                      
                                      (#/initWithContentsOfFile: NS-Image  (native-string (native-path (image-pathname self) String)))
                                      (#/setButtonType: Button #$NSOnOffButton) 
                                      (#/setBezelStyle: Button #$NSShadowlessSquareBezelStyle)
                                      (#/setShowsBorderOnlyWhileMouseInside: Button #$YES)
                                      (#/setImagePosition: Button #$NSImageOnly)                                   
                                      (#/setImage: Button NS-Image)     
                                      (#/setTarget: Button 
                                                    (make-instance 'native-target 
                                                      :native-control Button;(native-view Self)
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

(defmethod CLOSE-WINDOW ((Self general-pop-up-image-menu))
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
    (MAKE-POP-UP List)
    (warn "Cannot make a popup menu with an empty list")))


(defun MAKE-POP-UP (List)
  (let ((*Pop-Up-Menu* (make-instance 'general-pop-up-image-menu :images List :name-of-selection   (First List))))
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

#|
(objc:defmethod (#/resignKeyWindow :void) ((Self popup-window))
  ;(print "resigned!!!!"))

  (objc:defmethod (#/resignMainWindow :void) ((Self popup-window))
  ;(print "resigned!!!!"))                                        
    (objc:defmethod (#/mouseMoved: :void) ((self popup-window) Event)
      
      )
    |#
;;_______________________________
;; Popup window view             |
;;_______________________________
(defclass POPUP-WINDOW-VIEW (native-window-view)
  ()
  (:metaclass ns:+ns-object
              :documentation "popup-window-view"))
    
(objc:defmethod (#/mouseDown: :void) ((self popup-window-view) Event)
  )

(objc:defmethod (#/mouseUp: :void) ((self popup-window-view) Event)
  (#/stopModal (#/sharedApplication ns:ns-application)))


(objc:defmethod (#/mouseDragged: :void) ((self popup-window-view) Event)
  )
   ;(#/close (#/superview self)))

(defclass POPUP-DELEGATE(window-delegate)
  ()
  (:metaclass ns:+ns-object
	      :documentation "window delegate"))


(objc:defmethod (#/windowDidResignMain: :void) ((self popup-delegate) Notifaction)
  ;(print "resigned")
)


(objc:defmethod (#/windowDidBecomeKey: :void) ((self popup-delegate) Notifaction)
  ;;(print;(print "key")
)

  
;;_______________________________
;; Direction Pop Up Menu         |
;;_______________________________
;(export '(DIRECTIONAL-POP-UP-IMAGE-MENU DISPLAY-POP-UP))
(defclass DIRECTIONAL-POP-UP-IMAGE-MENU (general-pop-up-image-menu)
  ()
  (:documentation "An image pop up menu that is used as a directional picker"))

(defmethod image-names ((Self directional-pop-up-image-menu))
  '("direction_north_west.png" "direction_north.png" "direction_north_east.png" "direction_west.png" "direction_center.png" "direction_east.png" "direction_south_west.png" "direction_south.png" "direction_south_east.png"))

    