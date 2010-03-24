(in-package :lui)


(defmethod DISPLAY-POP-UP-MENU ((Self pop-up-image-group-menu) &key (x 100) (y 100))
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
            (let ((current-row 0) (current-col 0))
              (let ((i 0) )
                (dolist (group (image-names self))
                  (let ((group-name (first group)) (list (elt group 1)))
                    (let ((size (#/sizeWithAttributes: (native-string group-name) (#/dictionary ns:ns-dictionary))))
                      (let ((label (#/alloc ns:ns-text-field)))
                        (ns:with-ns-rect (Frame (* (image-width self) current-col) (* (image-height self) current-row) (label-width self) #|(* (image-width self) 4)|# (image-height self))   
                          (#/initWithFrame: Label Frame)
                          (#/setTextColor: Label  (#/blackColor ns:ns-color))
                          (#/setStringValue: Label (native-string group-name))
                          (#/setEditable: Label #$NO)
                          (#/setSelectable: Label #$NO)
                          (#/setDrawsBackground: Label #$NO)
                          (#/setBezeled: Label #$NO)
                          (#/addSubview: (native-view self) Label)))     
                      (dolist (String list )
                        (setf String (concatenate 'string String ".png"))
                        (let ((Button (make-instance 'indexed-button)))                       
                          (let ((NS-Image (#/alloc ns:ns-image)))
                            (ns:with-ns-rect (Frame (+ (label-width self) (* (image-width self) current-col)) (* (image-height self) current-row) (image-width self) (image-height self))                                      
                              (#/initWithFrame: Button Frame)
                              (unless (probe-file (native-path (image-pathname self) String))
                                (warn "Cannot load file ~A does not exist" (native-path (image-pathname self) String)))
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
                        (incf current-col)))
                    (setf current-col 0)
                    (incf current-row)))))
            (#/makeKeyAndOrderFront: Window Window)
            (#/runModalForWindow: (#/sharedApplication ns:ns-application) Window)
            (#/close (native-window self))
            (find-group-by-index self (index-of-selection self)))))))


(objc:defmethod (#/buttonAction :void) ((self native-target))
  (setf (index-of-selection (lui-control Self)) (index (native-control self)))
  (close-window (lui-control self)))


(defmethod CLOSE-WINDOW ((Self pop-up-image-group-menu))
  (#/stopModal (#/sharedApplication ns:ns-application)))


(defmethod FIND-GROUP-BY-INDEX ((Self pop-up-image-group-menu) Index)
  (let ((i 0))
    (dolist (group (image-names self))
      (let ((group-name (first group)))
      (dolist (string (second group))
        (if (eql  i Index)
          (return-from FIND-GROUP-BY-INDEX (list group-name string) ))
        (incf i)))))
  (nil))
    

(defmethod GET-WINDOW-WIDTH ((Self pop-up-image-group-menu))
  (let ((i 1))
    
    (let ((longest-row 0))
      (dolist (group (image-names self))
        (let ((group-name (first group)) )
      
        (print group-name)
        (let ((size (#/sizeWithAttributes: (native-string group-name) (#/dictionary ns:ns-dictionary))))
          (if (< (label-width self) (+ 15 (ns::ns-size-width size)))
            (setf (label-width self) (+ 15 (ns::ns-size-width size)))))
        (let ((group-length (label-width self)))
          (dolist (string (second group))
            (setf group-length (+ group-length (* i (image-width self))))
            (print i)
            (incf i))
          (if (> group-length longest-row)
            (setf longest-row group-length)))
        (setf i 0)))
      (return-from GET-WINDOW-WIDTH longest-row))))

