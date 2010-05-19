(in-package :lui)


;;*********************************
;; HELPER FUNCTIONS               *
;;*********************************


(defun VALIDATE-AGENT-NAME (name)
  name)


(defun CREATE-LIST-OF-ITEMS-FROM-LIST-OF-STRINGS (list)
  (let ((list-of-items nil))
    (dolist (item list)
      (let ((list-item (make-instance 'list-group-item :item-name (first item) :image-name (second item))))
        (case list-of-items
          (nil (setf list-of-items (list list-item)))
          (t (setf list-of-items (append list-of-items (list list-item)))))))
    list-of-items))


(defmethod REMOVE-BACKGROUND-FROM-TEXT-FIELDS ((self ns:ns-view))
  (let ((subviews (gui::list-from-ns-array (#/subviews self))))  
    (dolist (subview subviews)
      (if (or
           (equal (type-of subview) 'NS:NS-TEXT-VIEW )
           (equal (type-of subview) 'LUI::MOUSE-DETECTION-TEXT-FIELD ))
        (progn
          (ns:with-ns-range (range 0 0)
            (#/setSelectedRange: subview range))
          (#/setDrawsBackground: subview #$NO))
        (remove-background-from-text-fields subview)))))


(defmethod remove-background-and-end-editting-for-all-text-fields((self ns:ns-view))
  (if (or
       (equal (type-of (#/firstResponder (#/window  self))) 'NS:NS-TEXT-VIEW )
       (equal (type-of (#/firstResponder (#/window  self))) 'LUI::MOUSE-DETECTION-TEXT-FIELD ))
      (progn
        (#/endEditingFor: (#/window  self) nil )))
  (remove-background-from-text-fields  self))


(defmethod GET-GROUP-WITH-NAME ((self badged-image-group-list-manager-view) group-name)
  (dolist (group (groups self))
    (if (equal (group-name group) group-name)
      (progn 
        (return-from get-group-with-name group))))
  nil)


(defmethod RESIZE-HEIGHT-OF-VIEW ((Self badged-image-group-list-manager-view) &key (call-set-size t))
  
  (let ((height 0))
    (dolist (group (groups self))
      (if (is-disclosed group)
        (incf height (+  (row-height self)  (* (row-height self) (length (group-items group)))))
        (incf height  (row-height self))))  
    ;(if (> height 200)
    (setf (height self) height)
    ;)

    (unless (equal (#/superview (native-view self)) +null-ptr+)
      (progn
        
        #-cocotron
        (if call-set-size
          (progn
            
            (set-size (lui-view (#/superview (#/superview (native-view self))))   (width (lui-view (#/superview (#/superview (native-view self))))) (height (lui-view (#/superview (#/superview (native-view self))))))
            (layout (lui-view (#/superview (#/superview (native-view self)))))
            ))
        
        (if (lui-view (#/superview (#/superview (native-view self))))
          (progn
            (if (> (height (lui-view (#/superview (#/superview (native-view self))))) (height self))
              (setf (height self) (height (lui-view (#/superview (#/superview (native-view self))))))
              )
            
            ))
        
        ; #-cocotron 
        ))))


;;*********************************
;; LAYOUT VIEW                    *
;;*********************************


(defclass LAYOUT-VIEW (ns:ns-view)
  ()
  (:metaclass ns:+ns-object
	      :documentation ""))


(objc:defmethod (#/isFlipped :<BOOL>) ((self layout-view))
  ;; Flip to coordinate system to 0, 0 = upper left corner
  #$YES)


(defmethod LAYOUT ((Self layout-view)) 
  (let ((subviews (gui::list-from-ns-array (#/subviews self))))  
    (dolist (subview subviews)
      (unless (or 
               (equal (type-of subview) 'LUI::NATIVE-IMAGE)
               (equal (type-of subview) 'LUI::NATIVE-STATUS-BAR))
        (layout subview)))))


;;**********************************************
;; NATIVE BADGED IMAGE GROUP LIST MANAGER VIEW *
;;**********************************************


(defclass NATIVE-BADGED-IMAGE-GROUP-LIST-MANAGER-VIEW (layout-view)
  ((lui-view :accessor lui-view :initform nil :initarg :lui-view)
   )
  (:metaclass ns:+ns-object
	      :documentation ""))

#|
(objc:defmethod (#/drawRect: :void) ((self native-badged-image-group-list-manager-view) (rect :<NSR>ect))
  (call-next-method rect)
      (#/set (#/colorWithDeviceRed:green:blue:alpha: ns:ns-color .2 .2 .2 1.0))
      (#/fillRect: ns:ns-bezier-path rect)
)
|#

(defmethod LAYOUT ((Self native-badged-image-group-list-manager-view))
  (let ((y 0)) 
    (dolist (group (groups (lui-view self)))
      (let ((group-height (row-height (lui-view self))))
        (if (is-disclosed group)
          (incf group-height (* (row-height (lui-view self)) (length (group-items group)))))
        (ns:with-ns-size (Size (width (lui-view self)) group-height)
          (#/setFrameSize:  (group-view group ) Size))
        (ns:with-ns-size (Size (width (lui-view self)) (NS:NS-RECT-HEIGHT (#/frame (item-view group))))
          (#/setFrameSize:  (item-view group ) Size))
        #| (dolist (item-view (item-detection-views group))
             (#/setFrameSize:  item-view Size))|#          
        (ns:with-ns-point (Point 0 y)
          (#/setFrameOrigin: (group-view group ) Point ))
        (if (is-disclosed group)
          (progn           
            (#/setHidden: (item-view group) #$NO))
          (progn
            (#/setHidden: (item-view group) #$YES)))
        (#/setNeedsDisplay: (item-view group) #$YES)
        (if (is-highlighted group)
          (#/setHidden: (selection-view group ) #$NO)
          (#/setHidden: (selection-view group ) #$YES))
        ; #-cocotron (setf (width (lui-view self)) (NS:NS-RECT-WIDTH (#/frame (#/superview (#/superview (native-view self))))))
        (ns:with-ns-point (Point (NS:NS-RECT-X (#/frame (group-view group))) (+ 0  (NS:NS-RECT-Y (#/frame (group-view group)))) )
          (#/setFrameOrigin: (selection-view group) Point ))
        (ns:with-ns-size (Size (width (lui-view self)) (row-height (lui-view self)))
          (#/setFrameSize: (selection-view group) Size ))
        (#/setNeedsDisplay: (selection-view group) #$YES)
        (#/setNeedsDisplay: (group-view group) #$YES)
        (incf y   group-height)))) 
  (#/setNeedsDisplay: self #$YES)
  (resize-height-of-view (lui-view self))
  (call-next-method)
  (layout-changed (lui-view self)))


(objc:defmethod (#/mouseDown: :void) ((self native-badged-image-group-list-manager-view) Event)
  (declare (ignore Event))
  (set-selected (lui-view self) nil))



;;*********************************
;; GROUP DETECITON VIEW           *
;;*********************************


(defclass GROUP-DETECTION-VIEW (layout-view)
  ((container :accessor container :initform nil :initarg :container)
   (group-name :accessor group-name :initform nil :initarg :group-name)
   (list-group :accessor list-group :initform nil :initarg :list-group))
  (:metaclass ns:+ns-object
              :documentation "This is a view that will detect mouse input fo the group and will also containt all the other views for the group"))


(defmethod UPDATE-IMAGE ((self group-detection-view) &key (image-name nil))
  (let ((subviews (gui::list-from-ns-array (#/subviews self))))  
    (dolist (subview subviews)
      (if (or
           (equal (type-of subview) 'lui::group-item-image-view)
           (equal (type-of subview) 'lui::badged-image-view))
        (update-image subview :image-name image-name)))))


(objc:defmethod (#/mouseDown: :void) ((self group-detection-view) Event)
  ;(declare (ignore Event))
  (remove-background-and-end-editting-for-all-text-fields (native-view (container self)))
  (call-next-method Event)
  (if (container self)
    (set-selected (container self) (group-name self))))


(defmethod LAYOUT ((Self group-detection-view))
  (let ((group (get-group-with-name (container self) (group-name self))))
    (if group
      (unless (selected-item-name group)
        (if (item-selection-view group)
          (#/setHidden: (item-selection-view group) #$YES)))))
  (let ((subviews (gui::list-from-ns-array (#/subviews self))))
    (dolist (subview subviews)
      (#/setNeedsDisplay: subview #$YES)
      (if (equal (type-of subview) 'LUI::mouse-detection-text-field)
        (if (equal (type-of (#/superview self)) 'LUI::ITEM-CONTAINER-VIEW)
          
          (ns:with-ns-point (Point (NS:NS-RECT-X (#/frame subview))  (* (- (row-height (lui-view (#/superview(#/superview (#/superview self))))) (text-height (lui-view (#/superview(#/superview (#/superview self)))))) .5 ) )
            (#/setFrameOrigin: subview Point ))  
          
          (ns:with-ns-point (Point (NS:NS-RECT-X (#/frame subview))  (* (- (row-height (lui-view (#/superview self))) (text-height (lui-view (#/superview self)))) .5 ) )
            (#/setFrameOrigin: subview Point )))
        (unless (equal (type-of subview) 'LUI::ITEM-CONTAINER-VIEW )
          (ns:with-ns-point (Point (NS:NS-RECT-X (#/frame subview)) 0)
            (#/setFrameOrigin: subview Point ))))))
  (call-next-method))


;;*********************************
;; ITEM CONTAINER VIEW            *
;;*********************************


(defclass ITEM-CONTAINER-VIEW (layout-view)
  ()
  (:metaclass ns:+ns-object
              :documentation "View to detect the selection of an item."))


(defmethod LAYOUT ((Self item-container-view))
  (let ((subviews (gui::list-from-ns-array (#/subviews self))))
    (let ((y 0))
      (dolist (subview subviews)
        (if (equal (type-of subview) 'LUI::ITEM-DETECTION-VIEW )
          (ns:with-ns-point (Point (NS:NS-RECT-X (#/frame subview)) y)
            (#/setFrameOrigin: subview Point )
            (incf y 30))))
      (call-next-method))))


;;*********************************
;; ITEM DETECITON VIEW            *
;;*********************************


(defclass ITEM-DETECTION-VIEW (group-detection-view)
  ((container :accessor container :initform nil :initarg :container)
   (item-name :accessor item-name :initform nil :initarg :item-name)
   (list-item :accessor list-item :initform nil :initarg :list-item))
  (:metaclass ns:+ns-object
              :documentation "View to detect the selection of an item."))


(objc:defmethod (#/mouseDown: :void) ((self item-detection-view) Event)
 ;(declare (ignore Event))
  (remove-background-and-end-editting-for-all-text-fields (native-view (container self)))
  (call-next-method Event)
  (if (container self)
    (progn
      (set-selected-item (container self) (group-name (#/superview (#/superview self)))(item-name self) ))))


;;*********************************
;; MOUSE DETECTING IMAGE VIEW     *
;;*********************************


(defclass MOUSE-DETECTING-IMAGE-VIEW (ns:ns-image-view)
  ()
  (:metaclass ns:+ns-object
              :documentation "A view that display an image and detects mouse events."))


;;*********************************
;; GROUP ITEM IMAGE VIEW          *
;;*********************************


(defclass GROUP-ITEM-IMAGE-VIEW (group-detection-view)
  ((image-name :accessor image-name :initform nil :initarg :image-name)
   (image-path :accessor image-path :initform "lui:resources;images" :initarg :image-path)
   (image-size :accessor image-size :initform 28 :initarg :image-size)
   (x :accessor x :initform 0 :initarg :x :documentation "screen position, pixels")
   (y :accessor y :initform 00 :initarg :y :documentation "screen position, pixels")
   (item-name :accessor item-name :initform nil :initarg :item-name))
  (:metaclass ns:+ns-object
              :documentation "Group Item Image View"))


(defmethod SELECT ((self group-item-image-view))
  (set-selected-item (container self)  (group-name  (#/superview(#/superview (#/superview self))))(item-name self) ))


(defmethod DOUBLE-CLICKED ((self group-item-image-view))
  (edit-group-item (container self) (group-name self)(item-name self)))


(defmethod CREATE-IMAGE ((self group-item-image-view)(list-item list-group-item))
  (ns:with-ns-point (Point (x self) (y self))
    (#/setFrameOrigin: Self Point))
  (ns:with-ns-size (Size (image-size self) (image-size self))
    (#/setFrameSize:  Self Size))
  (let ((image (make-instance 'image-control :image-path (image-path list-item)  :src (image-name self) :x 0 :y 0 :width (image-size self) :height (image-size self)))) 
    (let ((image-view (#/alloc mouse-detecting-image-view)))
      (ns:with-ns-rect (Frame 0 0 (image-size self) (image-size self))
        (#/initWithFrame: image-view Frame )
        (#/setImage: image-view (#/image (native-view image)))
        (#/setImageScaling: image-view #$NSScaleToFit)
        (#/addSubview: self image-view))
      self)))


(defmethod UPDATE-IMAGE ((self group-item-image-view) &key (image-name nil)) 
  
  (if image-name
    (setf (image-name self) image-name))
  (let ((subviews (gui::list-from-ns-array (#/subviews self))))   
    (dolist (subview subviews)
      (if (equal (type-of subview) 'lui::mouse-detecting-image-view)
        (progn
          (#/removeFromSuperviewWithoutNeedingDisplay subview)
          (let ((image (make-instance 'image-control  :image-path (image-path self)  :src (image-name self) :x 0 :y 0 :width (image-size self) :height (image-size self)))) 
            (let ((image-view (#/alloc mouse-detecting-image-view)))
              (ns:with-ns-rect (Frame 0 0 (image-size self) (image-size self))
                (#/initWithFrame: image-view Frame )
                (#/setImage: image-view (#/image (native-view image)))
                (#/setImageScaling: image-view #$NSScaleToFit)
                (#/addSubview: self image-view)))))))))


(objc:defmethod (#/isFlipped :<BOOL>) ((self mouse-detecting-image-view))
  ;; Flip to coordinate system to 0, 0 = upper left corner
  #$YES)


(objc:defmethod (#/mouseDown: :void) ((self mouse-detecting-image-view) Event)
  (remove-background-and-end-editting-for-all-text-fields (native-view (container (#/superview (#/superview  self)))))
  (call-next-method Event)
  (select (#/superview self)) 
  (if (> ( #/clickCount Event) 1)
    (double-clicked (#/superview self))))


(defmethod LAYOUT ((Self mouse-detecting-image-view))
  ;do nothing 
)


;;*********************************
;; BADGED IMAGE VIEW              *
;;*********************************


(defclass BADGED-IMAGE-VIEW (group-detection-view)
  ((head-image-name :accessor head-image-name :initform nil :initarg :head-image-name)
   (badge-image-name :accessor badge-image-name :initform nil :initarg :badge-image-name)
   (head-image-size :accessor head-image-size :initform 28 :initarg :head-image-size)
   (badge-image-size :accessor badge-image-size :initform 16 :initarg :badge-image-size)
   (x :accessor x :initform 0 :initarg :x :documentation "screen position, pixels")
   (y :accessor y :initform 00 :initarg :y :documentation "screen position, pixels"))
  (:metaclass ns:+ns-object
              :documentation "A view that will be composed of two images one for the head-image and one for the badge"))


(defmethod CREATE-BADGED-IMAGE ((self badged-image-view))
  (ns:with-ns-point (Point (x self) (y self))
    (#/setFrameOrigin: Self Point))
  (ns:with-ns-size (Size (head-image-size self) (head-image-size self))
    (#/setFrameSize:  Self Size))
  (let ((head-image (make-instance 'image-control :src (head-image-name self) :x 0 :y 0 :width (head-image-size self) :height (head-image-size self)))
        (badge-image (make-instance 'image-control :src (badge-image-name self) :x (- (head-image-size self) (badge-image-size self))  :y 0 :width (badge-image-size self) :height (badge-image-size self)))) 
    (let ((head-image-view (#/alloc mouse-detecting-image-view))(badge-image-view (#/alloc mouse-detecting-image-view)))
      (ns:with-ns-rect (Frame 0 0 (head-image-size self) (head-image-size self))
        (ns:with-ns-rect (badge-frame (- (head-image-size self) (badge-image-size self)) 0 (badge-image-size self) (badge-image-size self))
          (#/initWithFrame: head-image-view Frame )
                      (#/setImage: head-image-view (#/image (native-view head-image)))
          (#/setImageScaling: head-image-view #$NSScaleToFit)
          (#/addSubview: self head-image-view)
          (#/initWithFrame: badge-image-view badge-frame)
          (unless (equal +null-ptr+ (#/superview self))
          (if (and 
               (list-group (#/superview self))
               (group-items (list-group (#/superview self)))
               (elt (group-items (list-group (#/superview self))) 0))
            (progn
              (let ((image (make-instance 'image-control :image-path (image-path (elt (group-items (list-group (#/superview self))) 0)) :src (image-name (elt (group-items (list-group (#/superview self))) 0)) :x 0 :y 0 :width (badge-image-size self) :height (badge-image-size self)))) 
                (#/setImage: badge-image-view (#/image (native-view image)))
                ))
            (#/setImage: badge-image-view (#/image (native-view badge-image)))))
          (#/setImageScaling: badge-image-view #$NSScaleToFit)
          (#/addSubview: self badge-image-view)))))
  self) 


(defmethod UPDATE-IMAGE ((self badged-image-view)&key (image-name nil))
  (let ((subviews (gui::list-from-ns-array (#/subviews self))))  
    (dolist (subview subviews)
      (#/removeFromSuperviewWithoutNeedingDisplay subview)))
  (setf self (create-badged-image self))
  (#/setNeedsDisplay: self #$YES))


(defmethod SELECT ((self badged-image-view))
  (make-selection (container self) (group-name (#/superview self)) ))


(defmethod DOUBLE-CLICKED ((self badged-image-view))
  (edit-group (container self) (group-name self)))



;;*********************************
;; MOUSE DETECTION TEXT FIELD     *
;;*********************************


(defclass MOUSE-DETECTION-TEXT-FIELD (ns:ns-text-view)
  ((container :accessor container :initform nil :initarg :container)
   (group :accessor group :initform nil :initarg group)
   (item-name :accessor item-name :initform nil :initarg :item-name))
  (:metaclass ns:+ns-object
              :documentation "A text field that detects mouse events.  "))


(defmethod CALCULATE-WIDTH-FOR-TEXT-FIELD ((self mouse-detection-text-field)) 
  ;(if (< (NS:NS-SIZE-WIDTH (#/sizeWithAttributes: (#/string self) (#/dictionary ns:ns-dictionary)))  (- (width (container self)) 50))
    ; (let ((width (* 1.25 (NS:NS-SIZE-WIDTH (#/sizeWithAttributes: (#/stringValue self) (#/dictionary ns:ns-dictionary))))))
    (let ((width (* 1.25 (NS:NS-SIZE-WIDTH (#/sizeWithAttributes: (#/string self) (#/dictionary ns:ns-dictionary))))))
      (if (< width 50)
        (setf width 50))
      width))
    ;)


(objc:defmethod (#/isFlipped :<BOOL>) ((self mouse-detection-text-field))
  ;; Flip to coordinate system to 0, 0 = upper left corner
  #$YES)


(defmethod LAYOUT ((Self mouse-detection-text-field))
  ;do nothing 
  )


(objc:defmethod (#/mouseDown: :void) ((self mouse-detection-text-field) Event)     
  (remove-background-and-end-editting-for-all-text-fields (native-view (container self)))
  (unless (item-name self)
    (progn
      (if (is-highlighted (group self))
        (progn
          (#/setDrawsBackground: self #$YES)
          (#/setEditable: self #$YES)
          (call-next-method event)))
      (set-selected (container self) (group-name (#/superview self)) :resign nil )))
  (if (item-name self)
    (progn 
      (if (and
           (equal (string-capitalize (selected-item-name (group self))) (string-capitalize (item-name self)))
           (and (selected-item-name (group self)) (item-name self)))
        (progn
          (#/setDrawsBackground: self #$YES)
          (#/setEditable: self #$YES)
          (call-next-method event)))
      (set-selected-item (container self) (group-name (#/superview (#/superview (#/superview self)))) (item-name self) ))))


(objc:defmethod (#/textDidChange: :void) ((self mouse-detection-text-field) Notification)
   (let ((width (calculate-width-for-text-field self)))
    (ns:with-ns-size (Size  width (NS:NS-RECT-HEIGHT (#/frame self)))
      (#/setFrameSize: self Size )))
  (setf (group-name (group self))(ccl::lisp-string-from-nsstring (#/stringValue self)))
  (setf (group-name (group-view (group self)))(ccl::lisp-string-from-nsstring (#/stringValue self)))
  (call-next-method Notification))

#|
(objc:defmethod (#/becomeFirstResponder  :void) ((self mouse-detection-text-field))
  ;(remove-background-from-text-fields (native-view (container self)))
  (remove-background-and-end-editting-for-all-text-fields (native-view (container self)))
  (unless (item-name self)
    (progn
      (if (is-highlighted (group self))
        (progn
          (#/setDrawsBackground: self #$YES)
          (call-next-method)))
      (set-selected (container self) (group-name (#/superview self)) :resign nil )))
  (if (item-name self)
    (progn 
      (if (and
           (equal (string-capitalize (selected-item-name (group self))) (string-capitalize (item-name self)))
           (and (selected-item-name (group self)) (item-name self)))
        (progn
          (#/setDrawsBackground: self #$YES)
          (call-next-method )))
      (set-selected-item (container self) (group-name (#/superview (#/superview (#/superview self)))) (item-name self) ))))

|#
;;*********************************
;; GROUP DISCLOSURE BUTTON        *
;;*********************************


(defclass GROUP-DISCLOSURE-BUTTON (bevel-button-control)
  ((group :accessor group :initform nil :initarg :group)
   (container :accessor container :initform nil :initarg :container))
  (:documentation "A button that is meant to display a disclosure item and has a group ascociated with it.  "))


(defmethod INITIALIZE-INSTANCE :after  ((Self group-disclosure-button) &rest Initargs)
  (declare (ignore initargs))
  (#/setButtonType: (native-view self) #$NSOnOffButton)
  (#/setImagePosition: (native-view self) #$NSImageOnly)
  (#/setBezelStyle: (native-view self) #$NSDisclosureBezelStyle))


(defmethod initialize-event-handling ((Self group-disclosure-button))
  (#/setTarget: (native-view Self) 
                (make-instance 'native-disclosure-target 
                  :container (container self)
                  :native-control (native-view Self)
                  :lui-control Self))
  (#/setAction: (native-view Self) (objc::@selector #/activateAction)))


(defmethod DISCLOSURE-ACTION((self badged-image-group-list-manager-view) (button group-disclosure-button))
  (if (is-disclosed (group button))
    (progn 
      (setf (is-disclosed (group button)) nil)
     (layout (native-view self)))
    (progn
      (setf (is-disclosed (group button)) #$YES)
       (layout (native-view self)))))


(defmethod LAYOUT ((Self group-disclosure-button))
  ;do nothing 
  )


;;*********************************
;; NATIVE DISCLOSURE TARGET       *
;;*********************************


(defclass native-disclosure-target (ns:ns-object)
  ((native-control :accessor native-control :initarg :native-control)
   (lui-control :accessor lui-control :initarg :lui-control)
   (container :accessor container :initarg :container))
  (:metaclass ns:+ns-object)
  (:documentation "receives action events and forwards them to lui control"))


(objc:defmethod (#/activateAction :void) ((self native-disclosure-target))
  (funcall 'disclosure-action (container self) (lui-control self)))


(defmethod SET-SIZE ((Self badged-image-group-list-manager-view) Width Height)
  (declare (ignore Width Height))
  (call-next-method)
  
  ; #-cocotron(set-size (lui-view (#/superview (#/superview (native-view self)))) (width (lui-view (#/superview (#/superview (native-view self))))) (height (lui-view (#/superview (#/superview (native-view self))))))
  ;(call-next-method self width height)
  
  #-cocotron 
  (if (> (-(width (lui-view (#/superview (#/superview (native-view self)))))1)  (minimum-width self))
    (setf (width self) (-(width (lui-view (#/superview (#/superview (native-view self)))))1)))
  (dolist (group (groups  self))
    (ns:with-ns-size (Size (width self) (NS:NS-RECT-HEIGHT (#/frame (group-view group))))
      (#/setFrameSize:  (group-view group ) Size))
    (ns:with-ns-size (Size (width  self) (NS:NS-RECT-HEIGHT (#/frame (item-view group))))
      (#/setFrameSize:  (item-view group ) Size))
    (ns:with-ns-size (Size (width  self) (row-height self))
      (#/setFrameSize: (selection-view group) Size )
      (if (item-selection-view group)
        (progn
          (#/setFrameSize: (item-selection-view group) Size )
          (#/setNeedsDisplay: (item-selection-view group) #$YES)))) )  
  #|
  (if (and 
       (#/superview (native-view self)) 
       (#/superview (#/superview (native-view self))) 
       (lui-view (#/superview (#/superview (native-view self))))
       
       )
    (setf (height self) (- (height (lui-view (#/superview (#/superview (native-view self)))))1))) 
  |#
  (resize-height-of-view self :call-set-size nil)
  (display self)
  (#/setNeedsDisplay: (native-view self) #$YES))


(defmethod DELETE-GROUP ((Self badged-image-group-list-manager-view) group-name)
  (dolist (group (groups self))
    (if (equal (group-name group) group-name)
      (progn 
        (#/setHidden: (selection-view (get-group-with-name self group-name)) #$YES)
        (#/removeFromSuperviewWithoutNeedingDisplay (group-view (get-group-with-name self group-name)))
        (#/setNeedsDisplay: (native-view self) #$YES)     
        (setf (groups self) (delete group (groups self)))
        (layout (native-view self))))))


(defmethod DELETE-GROUP-ITEM ((Self badged-image-group-list-manager-view) group-name item-name)
  (dolist (group (groups self))
    (if (equal (group-name group) group-name)
      (let ((i 0))
      (dolist (item (group-items group))
        (if (equal (item-name item) item-name)
          (progn
            (#/removeFromSuperviewWithoutNeedingDisplay (elt (item-detection-views group) i))   
            (setf (item-detection-views group) (delete  (elt (item-detection-views group) i) (item-detection-views group)))          
            (setf (selected-item-name group) nil)
            (setf (group-items group) (delete item (group-items group)))          
            (layout (native-view self))))
        (incf i))))))


(defmethod ADD-ITEM-TO-GUI ((Self badged-image-group-list-manager-view) group group-item)
  (ns:with-ns-size (Size (NS:NS-RECT-WIDTH (#/frame (item-view group)))  (+ (row-height self) (NS:NS-RECT-HEIGHT (#/frame (item-view group)))))
    (#/setFrameSize: (item-view group) Size ))
  (let ((x (+ (left-margin self)(group-item-offset self))) (text-length 100) (item-y (- (* (row-height self) (length (group-items group))) (row-height self))))
    (unless (item-selection-view group)
      (let ((selection-image (make-instance 'image-control :src "blue-box.png" :x 0 :y item-y :width (width self) :height (row-height self))))
        (unless (eql (item-name group-item) (selected-item-name group))
          (#/setHidden: (native-view selection-image) #$YES))
        (setf (item-selection-view group) (native-view selection-image))
        (#/addSubview: (item-view group) (native-view selection-image))))
    #|
    (let ((item-counter (make-instance 'status-bar-control :text "0" :x x :y item-y :width 40 :height (- (row-height self) 2 ))))
      (#/addSubview: (item-view group) (native-view item-counter))
      (setf (item-counter group-item) item-counter)
      (incf x 45))
    |#
    (let ((detection-view-item (#/alloc item-detection-view)))
      (ns:with-ns-rect (detection-frame 0 item-y (width self) (row-height self))
        (setf (group-name detection-view-item) (group-name group))
        (setf (item-name detection-view-item) (item-name group-item))
        (setf (container detection-view-item) self)
        (#/initWithFrame: detection-view-item detection-frame)
        (case (item-detection-views group)
          (nil (setf (item-detection-views group) (list detection-view-item)))
          (t (setf (item-detection-views group) (append (item-detection-views group) (list detection-view-item)))))    
        (#/addSubview: (item-view group) detection-view-item))  
      
      
      (let ((image (make-instance 'group-item-image-view   :image-path (image-path group-item)   :group-name (group-name group) :container self :item-name (item-name group-item)  :x x :y item-y :image-name (image-name group-item)  ))) ;'image-control :src (group-image group) :x x :y y :width (row-height self) :height (row-height self)))) 
        (incf x (row-height self))
        
        (#/addSubview: #|items-container|# detection-view-item (create-image image group-item)))
      (let ((text (#/alloc mouse-detection-text-field))) 
        (ns:with-ns-rect (Frame x  item-y  text-length (text-height self))
          (setf (container text) self)
          ; (setf (group-name text) (group-name group))
          (setf (item-name text) (item-name group-item))
          (#/initWithFrame: text Frame)
          (incf x text-length)     
          ;(#/setStringValue: text (native-string (item-name group-item)))
          (#/insertText: text (native-string (item-name group-item)))
          (#/setBackgroundColor: text (#/whiteColor ns:ns-color))
          (#/setDrawsBackground:  text #$NO)
        ;  (#/setBezeled: text #$NO)
       ;   (#/setBordered: text #$NO)
          (#/setEditable: text #$NO)
          (setf (text-view group-item) text)
          (setf (group text) group)
          (let ((width (calculate-width-for-text-field text)))
              (ns:with-ns-size (Size  width (NS:NS-RECT-HEIGHT (#/frame text)))
                (#/setFrameSize: text Size )))
          (#/addSubview: #|items-container|# detection-view-item  text)
          (case (item-detection-views group)
            (nil (setf (item-text-views group) (list text)))
            (t (setf (item-text-views group) (append (item-text-views group) (list text)))))))
      (incf item-y (row-height self))
      (setf x (+ (left-margin self)(group-item-offset self)))
      (setf (list-item detection-view-item) group-item)
      (setf (item-detection-view group-item) detection-view-item)))
  
  (update-image (group-view group))
  
  )


(defmethod ADD-GROUP-TO-GUI ((Self badged-image-group-list-manager-view) group)
  (let ((x (left-margin self)) (text-length 100))
    (let ((height (row-height self)))
      (unless (eql (is-disclosed group) nil)
        (if (> (length (group-items group)) 0)
          (setf height (* (row-height self) (+ 1 (length (group-items group)))))))
      (let ((selection-image (make-instance 'image-control :src "blue-box.png" :x 0 :y 1 :width (width self) :height (- height 2))))
        (unless (is-selected group)
          (#/setHidden: (native-view selection-image) #$YES))
        (setf (selection-view group) (native-view selection-image))
        (#/addSubview: (native-view self) (native-view selection-image))))
    (let ((detection-view (#/alloc group-detection-view)))
      (let ((group-height (row-height self)))
        (if (is-disclosed group)
          (setf group-height (+ group-height (* (length (group-items group)) (row-height self)))))     
        (ns:with-ns-rect (detection-frame x 0 (width self) group-height)
          (setf (list-group detection-view) group)
         ; (setf (y group) (+ (row-height self)(- y group-height)))
          (setf (group-name detection-view) (group-name group))
          (setf (container detection-view) self)         
          (#/initWithFrame: detection-view detection-frame)
          (#/setFrame: detection-view detection-frame)
          (#/addSubview: (native-view self) detection-view))
        (let ((button (make-instance 'group-disclosure-button :container self :group group :x x :y 0 :width (row-height self) :height (row-height self) :action 'disclosure-action))); (#/alloc ns:ns-button)))
          (ns:with-ns-rect (button-frame x (- group-height (row-height self)) (row-height self) (row-height self))
            (incf x (row-height self))
            (if (is-disclosed group)
              (#/setState: (native-view button) #$NSOnState))
            (setf (button-view group) (native-view button))
            (#/addSubview: detection-view (native-view button))))     
        (let ((image (make-instance badged-image-view :container self :group-name (group-name group) :x x :y 0 :head-image-name (head-image-name self)  :badge-image-name (group-image group)))) 
          (incf x (row-height self))
          (let ((image-view (create-badged-image image)))
            (#/addSubview: detection-view image-view)
            (setf (image-view group) image-view)))
        (let ((text (#/alloc mouse-detection-text-field)))
          (ns:with-ns-rect (Frame x  (+ (* (- (row-height self) (text-height self)) .5 ) (- group-height (row-height self)))   text-length (text-height self))
            (setf (container text) self)
            (#/initWithFrame: text Frame)
            (incf x text-length)     
            ;(#/setStringValue: text (native-string (string-capitalize (validate-agent-name (group-name group)))))
            (#/insertText: text (native-string (string-capitalize (validate-agent-name (group-name group)))))
            (#/setBackgroundColor: text (#/whiteColor ns:ns-color))
            (#/setDrawsBackground:  text #$NO)
           ; (#/setBezeled: text #$NO)
          ;  (#/setBordered: text #$NO)
            (#/setEditable: text #$NO)
            (setf (group text) group)
            (setf (text-view group) text)
            (let ((width (calculate-width-for-text-field text)))
              (ns:with-ns-size (Size  width (NS:NS-RECT-HEIGHT (#/frame text)))
                (#/setFrameSize: text Size )))
            (#/addSubview: detection-view  text))))    
   ;   (incf y (- 0(row-height self)))
      (setf X (left-margin self))      
      (let ((items-container (#/alloc item-container-view)) )
        (ns:with-ns-rect (item-detection-frame x (row-height self) (width self) (* (length (group-items group)) (row-height self)))  
          (#/initWithFrame: items-container item-detection-frame))
        (setf x (+ (left-margin self)(group-item-offset self)))
        (#/addSubview: detection-view items-container)
        (setf (item-view group) items-container)
        (dolist (group-item (group-items group))
          (add-item-to-gui self group group-item)))
      (setf x (left-margin self))
      (setf (group-view group) detection-view))))


(defmethod ADD-GROUP ((Self badged-image-group-list-manager-view) group)
  (if (< (width self) (width (lui-view (#/superview (#/superview (native-view self))))))
    (setf (width self) (width (lui-view (#/superview (#/superview (native-view self)))))))
  
  (dolist (old-group (groups self))
    (if (or
         (equal (first group) (group-name old-group))
         (equal (first group) (string-capitalize (group-name old-group))))
      (progn 
        (print "Cannot add group, a group with this name already exists")
        ;(warn "Cannot add group, a group with this name already exists")
        (return-from add-group nil))))                                                                                             ;need to do a dolist for the following
  (let ((list-group (make-instance 'list-group :group-name (first group) :group-image (second group) :group-items (create-list-of-items-from-list-of-strings (third group))))); (third group)))) ;(make-instance 'list-group-item :item-name (first (third group)) :image-name (second (third group))))))
    (case (groups self)
      (nil (setf (groups self) (list list-group)))
      (t (setf (groups self) (append (groups self) (list (make-instance 'list-group :group-name (first group) :group-image (second group) :group-items (create-list-of-items-from-list-of-strings (third group))))))))
    (if (native-view self)
      (progn
        (if (< (width self) (width (lui-view (#/superview (#/superview (native-view self))))))
          (setf (width self) (width (lui-view (#/superview (#/superview (native-view self)))))))
        (add-group-to-gui self (get-group-with-name self (group-name list-group)))
        (#/setNeedsDisplay: (native-view self) #$YES)
        (layout (native-view self))))
    list-group))


(defmethod ADD-GROUP-ITEM ((Self badged-image-group-list-manager-view) group-name item &key (image-path "lui:resources;images;"))
  (setf (first item) (string-capitalize (first item)))
  
  
  ;(setf group-name (string-capitalize group-name))
  (let ((list-item (make-instance 'list-group-item :item-name (first item) :image-path image-path :image-name (second item))))

  (let ((group (get-group-with-name self group-name)))
    (if (group-items group)
    (dolist (group-item (group-items group))
      (if (equal (first item) (item-name group-item))
        (progn
          ;(warn "Cannot add group item, a group item with this name already exists")
          (print "Cannot add group item, a group item with this name already exists")
          (return-from add-group-item nil)))))

    (setf (group-items group) (append (group-items group) (list list-item)  ))

    (add-item-to-gui self group list-item)

    (#/setHidden: (item-view group) #$NO))
    (layout (native-view self))
    
  "SUCCESS"))


(defmethod MAKE-NATIVE-OBJECT ((Self badged-image-group-list-manager-view))
  (let ((View (make-instance 'native-badged-image-group-list-manager-view :lui-view Self))) 
    view))


(defmethod SET-SELECTED ((Self badged-image-group-list-manager-view) group-name &key (highlight t) (resign "YES"))
  ;(remove-background-from-text-fields (native-view  self))
  (let ((previously-selected-group-name  (selected-group self)))
  (with-simple-restart (cancel-pop "Stop to create view for ~s" Self)    
    (dolist (group (groups self))
      (setf (selected-item-name group) nil) 
      (if (equal group-name (group-name group))
        (progn   
          (setf (selected-item-name group) nil )
          (if (item-selection-view group)
            (#/setHidden: (item-selection-view group) #$YES))
          (setf (is-selected group) "YES")
          (setf (is-highlighted group) highlight)
          (ns:with-ns-point (Point (NS:NS-RECT-X (#/frame (group-view group))) (+ 0  (NS:NS-RECT-Y (#/frame (group-view group)))) )
            (#/setFrameOrigin: (selection-view group) Point ))
          (ns:with-ns-size (Size (width self)  (row-height self) ) 
            (#/setFrameSize: (selection-view group) Size ))
          (if (is-highlighted group)
            (#/setHidden: (selection-view group) #$NO))
         ; (#/setEditable:  (text-view group) #$YES)
          (#/setNeedsDisplay: (selection-view group) #$YES))
        (progn
          (setf (is-highlighted group) nil)
          (if resign
            (if (or
                 (equal (type-of (#/firstResponder (#/window (native-view self)))) 'NS:NS-TEXT-VIEW )
                 (equal (type-of (#/firstResponder (#/window (native-view self)))) 'LUI::MOUSE-DETECTION-TEXT-FIELD ))
              (progn
                (#/endEditingFor: (#/window (native-view self)) nil ))))
          (setf (selected-item-name group) nil)
          (#/setDrawsBackground:  (text-view group) #$NO)
          (#/setEditable:  (text-view group) #$NO)
          (if (item-selection-view group)
            (#/setHidden: (item-selection-view group) #$YES))
          (setf (is-selected group) nil)
          (setf (is-highlighted group) nil)
          (#/setHidden: (selection-view group) #$YES)
          (#/setNeedsDisplay: (selection-view group) #$YES)))))
    (unless (equal previously-selected-group-name  (selected-group self))
      (selected-group-changed self )))
  (layout-changed self))


(defmethod SELECTED-GROUP-CHANGED ((Self badged-image-group-list-manager-view) ) 
  (funcall (selected-group-changed-action self) (lui-window (#/window (native-view self) )) self ))


(defmethod SET-SELECTED-ITEM ((Self badged-image-group-list-manager-view) group-name item-name)
  (let ((i 1)
        (previously-selected-group-name  (selected-group self)))
    (unless (equal (selected-group self) group-name)
            (selected-group-changed self ))
    (dolist (group (groups self))    
      (setf (is-selected group) nil)
      (setf (is-highlighted group) nil)
      (if (equal group-name (group-name group))
        (progn 
          
          (dolist (item (group-items group))
            (if (equal item-name (item-name item))
              (progn
                (let ((y-offset   (position item-name (group-items group) :key #'item-name :test #'equal)))
                  (setf (selected-item-name group) (item-name item))
                  (setf (is-selected group) "YES")
                  (#/setHidden: (selection-view group) #$YES)
                  (ns:with-ns-point (Point (NS:NS-RECT-X (#/frame (group-view group)))  (* 30 y-offset)  )
                    (#/setFrameOrigin: (item-selection-view group) Point ))
                  (#/setHidden: (item-selection-view group) #$NO)
                  (#/setNeedsDisplay: (item-selection-view group) #$YES)
                  (setf (selected-item-name group) (item-name item) ))
                ;(#/setEditable: (text-view item) #$YES)
                )
              (progn
                (#/setEditable: (text-view item) #$NO)))))
        (progn 
          (setf (selected-item-name group) nil)
          (#/setHidden: (selection-view group) #$YES)
          (setf (is-highlighted group) nil)
          (if (item-selection-view group)
            (progn
              (#/setHidden: (item-selection-view group) #$YES)
              (#/setNeedsDisplay: (item-selection-view group) #$YES)
              ))))
      (incf i))
    (unless (equal previously-selected-group-name  (selected-group self))
      (selected-group-changed self )))
  (layout-changed self)
  (layout (native-view self)))


(defmethod INCREMENT-ITEM-COUNTER ((Self badged-image-group-list-manager-view) group-name item-name &key (number-of-agents 1))
  (dolist (group (groups self))     
    (if (equal group-name (group-name group))
      (dolist (item (group-items group))      
        (if (equal item-name (item-name item))   
          (progn
            (let ((item-counter-value (read-from-string (value (item-counter item)))))
              (incf item-counter-value number-of-agents)
              (setf (VALUE (item-counter item))  (write-to-string item-counter-value )))
            )
          )))))


(defmethod CLEAR-ITEM-COUNTERS ((Self badged-image-group-list-manager-view) )
  (dolist (group (groups self))     
    (dolist (item (group-items group))          
      (setf (VALUE (item-counter item))  (write-to-string 0)))))
  

#|
(objc:defmethod (#/becomeFirstResponder  :void) ((self mouse-detection-text-field))
  ;(remove-background-from-text-fields (native-view (container self)))
  
  ;(#/setDrawsBackground: self #$YES)
  (call-next-method))
|#


(defmethod LAYOUT ((Self lui::native-bevel-button))
  ;do nothing 
  )






