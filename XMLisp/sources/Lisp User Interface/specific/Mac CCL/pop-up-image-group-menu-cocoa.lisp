(in-package :lui)


(defmethod FIND-GROUP-BY-INDEX ((Self pop-up-image-group-menu) Index)
  (let ((i 0))
    (dolist (group (image-names self))
      (let ((group-name (first group)))
        (dolist (string (second group))
          (if (eql  i Index)
            (return-from FIND-GROUP-BY-INDEX (list group-name string) ))
          (incf i))))))


(defmethod DISPLAY-POP-UP-MENU ((Self pop-up-image-group-menu) &key (x 100) (y 100))
  (setf (x self) x)
  (setf (y self) y)
  (with-simple-restart (cancel-pop "Stop trying to pop up ~s" Self)
    (in-main-thread ()
      (ccl::with-autorelease-pool
          (let ((Window (make-instance 'popup-window
                          :lui-window Self
                          :with-content-rect (ns:make-ns-rect  (x self) (y self) (width Self) (window-height Self))
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
            (let ((current-row 0) (current-col 0)(i 0))
                (dolist (group (image-names self))
                  (let ((group-name (first group)) 
                        (list (elt group 1))             
                        (label (#/alloc ns:ns-text-field)))
                    (ns:with-ns-rect (Frame (* (image-width self) current-col) (* (image-height self) current-row) (label-width self) #|(* (image-width self) 4)|# (image-height self))   
                      (#/initWithFrame: Label Frame)
                      (#/setTextColor: Label  (#/blackColor ns:ns-color))
                      (#/setStringValue: Label (native-string group-name))
                      (#/setEditable: Label #$NO)
                      (#/setSelectable: Label #$NO)
                      (#/setDrawsBackground: Label #$NO)
                      (#/setBezeled: Label #$NO)
                      (#/addSubview: (native-view self) Label))     
                    (dolist (Item list )
                      
                      (let ((indexed-view (make-instance 'indexed-image-view))                       
                            (NS-Image (#/alloc ns:ns-image))
                            (image-pathname (image-pathname self))
                            (image-name nil))
                        (ns:with-ns-rect (Frame (+ (label-width self) (* (image-width self) current-col)) (* (image-height self) current-row) (image-width self) (image-height self))                                      
                          (#/initWithFrame: indexed-view Frame)
                          (etypecase Item
                            (list
                             (setf image-name (second item)) ;(concatenate 'string (first Item) "." (image-file-extension self)))
                             ;(setf image-pathname (Second Item))
                             (unless (probe-file  image-name)
                               (warn "Cannot load file ~A does not exist" image-name))
                             (#/initWithContentsOfFile: NS-Image  (native-string  (namestring image-name))))
                            (string
                             (setf Item (concatenate 'string Item "." (image-file-extension self)))
                             (setf image-name Item)
                             (unless (probe-file (native-path image-pathname image-name))
                               (warn "Cannot load file ~A does not exist" (native-path image-pathname image-name)))
                             (#/initWithContentsOfFile: NS-Image  (native-string (native-path image-pathname image-name)))))
                          
                          
                          (setf (lui-superview indexed-view) self)
                         ; (#/setButtonType: Button #$NSOnOffButton) 
                         ; (#/setBezelStyle: Button #$NSShadowlessSquareBezelStyle)
                         ; (#/setShowsBorderOnlyWhileMouseInside: Button #$YES)
                         ; (#/setImagePosition: Button #$NSImageOnly)                                   
                          (#/setImage: indexed-view NS-Image)     
                         ; (#/setImageScaling: Button 1)
                          (print "SCALING")
                          
                        ;  (print (#/imageScaling Button))
                          #|
                          (#/setTarget: Button 
                                        (make-instance 'native-target 
                                          :native-control Button ;(native-view Self)
                                          :lui-control Self))
                          (#/setAction: Button (objc::@selector #/buttonAction))
                          |#
                          (setf (index indexed-view) i)
                          (#/addSubview: (native-view self) indexed-view)))
                      (incf i)
                      (incf current-col))
                    (setf current-col 0)
                    (incf current-row))))
            (#/makeKeyAndOrderFront: Window Window)
            (#/runModalForWindow: (#/sharedApplication ns:ns-application) Window)
            (#/close (native-window self))
            ;(inspect (find-group-by-index self (index-of-selection self)))
            (let ((selected-group (find-group-by-index self (index-of-selection self))))
              (etypecase (second selected-group)
                (string
                 (values (first selected-group) (second selected-group)))
                (list 
                 (values (first selected-group) (first (second selected-group))))))
                          
                          
                )))))


(objc:defmethod (#/mouseDown: :void) ((self indexed-image-view) Event)
  (call-next-method Event)
  (setf (index-of-selection (lui-superview Self)) (index self))
  (close-window (lui-superview self)))


(objc:defmethod (#/buttonAction :void) ((self native-target))
  (setf (index-of-selection (lui-control Self)) (index (native-control self)))
  (close-window (lui-control self)))


(defmethod CLOSE-WINDOW ((Self pop-up-image-group-menu))
  (#/stopModal (#/sharedApplication ns:ns-application)))
    

(defmethod GET-WINDOW-WIDTH ((Self pop-up-image-group-menu))
  (let ((longest-row 0))
    (dolist (group (image-names self))
      (let* ((group-name (first group)) 
       (size (#/sizeWithAttributes: (native-string group-name) (#/dictionary ns:ns-dictionary))))
          (if (< (label-width self) (+ 15 (ns::ns-size-width size)))
            (setf (label-width self) (+ 15 (ns::ns-size-width size))))
        (let ((group-length (label-width self)))
          (setf group-length (+ group-length (* (+ 1 (length group)) (image-width self))))
          (if (> group-length longest-row)
            (setf longest-row group-length)))))
    (return-from GET-WINDOW-WIDTH longest-row)))

