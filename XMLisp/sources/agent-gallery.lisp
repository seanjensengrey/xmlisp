(in-package :lui)
(export '(agent-gallery-view))

(defclass AGENT-GALLERY-VIEW (BADGED-IMAGE-GROUP-LIST-MANAGER-VIEW)
  (
   (window :accessor window :initform nil :initarg :window )
   (update-action :accessor update-action :initarg :update-action :initform 'xlui::update-buttons))
   (:documentation "This class will is a view that contains groups of items that are identified by a badge image."))


(defmethod layout-changed ((self agent-gallery-view))
  
  (unless (equal (#/window (native-view self)) +null-ptr+)
    (funcall (update-action self) self (lui-window (#/window (native-view self)))))
    
    ;(inspect  (lui-window (#/window (native-view self)))))

  )



(defmethod EDIT-GROUP-ITEM ((self agent-gallery-view) group-name item-name)
  ;(declare (ignore group-name item-name))
  (print "Editting group item.  Please override me.  GALLERY!" )
  
  
    
;  (defparameter *Inflatable-Icon-Editor2* (xlui::new-inflatable-icon-editor-window-from-image (print (easygui::choose-file-dialog)))))
  (defparameter *Inflatable-Icon-Editor* (xlui::new-inflatable-icon-editor-window   :width 32 :height 32))
  (setf (file *Inflatable-Icon-Editor*) "lui:resources;images;redlobster.png")
  (setf (container *Inflatable-Icon-Editor*) self ))


(defmethod SAVE-BUTTON-PRESSED ((self agent-gallery-view))
  (print "SAVE BUTTON PRESSED")
  (let ((i 0)(group (get-group-with-name self (selected-group self))))
    (dolist (item (group-items group))
      (if (equal (item-name item) (selected-group-item self))
        (progn
          (update-image (elt (item-detection-views group) i))
          (print "I")
          (print i)
          (if (equal i 0)
            (progn
              (print "IN")
              (update-image (group-view group)))))
        (print "NOT IT")
        )
      
      (incf i))
    ))

(in-package :xlui)


(defmethod add-agent-action ((w application-window) (Button bevel-button))
 ; (SHOW-AGENT-INPUT-WINDOW button)
  (let ((image-badge (value (view-named w "image-badge"))) (user-input (get-string-from-user "Please select a name and image for your agent" :initial-string "Unitled Agent")))
    ;(setf (window image-badge) w)
    (add-group image-badge `(,(second user-input) nil nil )); (("shape5" "redlobster.png")( "shape4" "redlobster.png"))))
    ))

(defmethod delete-selected-agent ((w application-window) (Button bevel-button))
  (let ((image-badge (value (view-named w "image-badge"))) (name (value (view-named w "text-field"))))
    (if (selected-group-item image-badge)
      (delete-group-item image-badge  (selected-group image-badge) (selected-group-item image-badge))
      (delete-group image-badge  (selected-group image-badge))))) 

(defmethod add-shape-action ((w application-window) (Button bevel-button))
  (let ((image-badge (value (view-named w "image-badge"))) (user-input (get-string-from-user "Please select a name and image for your agent" :initial-string "Unitled Agent")))
    ;(setf (window image-badge) w)
    (add-group-item image-badge (selected-group image-badge) `(,(second user-input) ,(first user-input) ))
    ))

(defclass GET-STRING-FROM-USER-WINDOW (dialog-window)
  ()
  (:documentation "Used to get a string from user"))


(defmethod OK-ACTION ((Window get-string-from-user-window) (Button button))
  (print "OK")
  (print (value (view-named Window "image-selection")))
  (stop-modal Window (value (list (value (view-named Window "image-selection"))(value (view-named Window "text"))))))


(defmethod CANCEL-ACTION ((Window get-string-from-user-window) (Button button))
  (cancel-modal Window))


(defmethod SHOW ((Self get-string-from-user-window))
  ;; do not actually show
  )



(defmethod update-buttons((self agent-gallery-view)(w application-window))
  (let ((delete-button   (view-named w "delete"))(add-shape-button (view-named w "add-shape")))
    ;(#/setEnabled: (lui::native-view delete-button) #$NO)
    (if (selected-group self)
      (#/setEnabled: (lui::native-view add-shape-button) #$YES)
      (#/setEnabled: (lui::native-view add-shape-button) #$NO)
    )
    (if (or (selected-group self)(selected-group-item self))
      (#/setEnabled: (lui::native-view delete-button) #$YES)
      (#/setEnabled: (lui::native-view delete-button) #$NO)
    ))
    
  
  ; (lui::disable delete-button))
  ; (print delete-button))
  ; (inspect delete-button))
  (print "update buttons"))

(defmethod layout-changed ((self agent-gallery-view))
  ;(print "Layout has changed, please override me??????")
  (funcall 'test-wtf self)
  )


(defmethod READ-RETURN-VALUE ((Self get-string-from-user-window))
  Self)


(defun GET-STRING-FROM-USER (Message &key Initial-String)
  (let ((Window (load-object "lui:resources;windows;agent-input.window" :package (find-package :xlui))))
    (when Initial-String
      (setf (value (view-named Window "text")) Initial-String))
    (setf (title Window) Message)
    (show-and-run-modal Window)))


(defmethod layout-changed ((self badged-image-group-list-manager-view))
  (print "Layout has changed, please override me!!!!!")
  )


<application-window title="agent-gallery"  margin="0" height="500">
  <column align="stretch" valign="stretch">  
    <scroll-box vflex="1">
      <agent-gallery name="image-badge">
        <image-badge-group-list-item name="lobster!!" />
      </agent-gallery>
    </scroll-box>
    <row align="stretch" valign="stretch" height="30">
      <bevel-button text="Add Agent" name="add-agent" action="add-agent-action" width="95" />
      <bevel-button text="Add Shape" name="add-shape" action="add-shape-action" width="95" />
      <bevel-button text="Delete"    name="delete" action="delete-selected-agent" width="80" />
    </row>
  </column>
</application-window> 

