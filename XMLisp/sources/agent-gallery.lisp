(in-package :lui)
;(export '(agent-gallery-view ))

(defclass AGENT-GALLERY-VIEW (BADGED-IMAGE-GROUP-LIST-MANAGER-VIEW)
  ((project-manager-reference :accessor project-manager-reference :initform nil :initarg :project-manager-reference )
   (window :accessor window :initform nil :initarg :window )
   (update-action :accessor update-action :initarg :update-action :initform 'xlui::update-buttons))
   (:documentation "This class will is a view that contains groups of items that are identified by a badge image."))




(defmethod layout-changed ((self agent-gallery-view))
  
  (unless (equal (#/window (native-view self)) +null-ptr+)
    ;(funcall (update-action self) self (lui-window (#/window (native-view self))))
    )
    
    ;(inspect  (lui-window (#/window (native-view self)))))

  )



(defmethod EDIT-GROUP-ITEM ((self agent-gallery-view) group-name item-name)
  ;(declare (ignore group-name item-name))
  (let ((*Inflatable-Icon-Editor* (xlui::new-inflatable-icon-editor-window   :width 32 :height 32)))
    (let ((shape-manager (xlui::find-shape-manager (xlui::project-manager-reference self)  (agentcubes:agentcubes-class-symbol group-name) (agentcubes:agentcubes-symbol item-name))))
      (let ((image-name (xlui::icon (xlui::shape shape-manager))))
        (setf (file *Inflatable-Icon-Editor*) (make-pathname :name (subseq image-name 0 (- (length image-name) #.(length ".png"))) :directory  (pathname-directory  (file shape-manager)  ))) ;"lui:resources;images;redlobster.png")
        (setf (container *Inflatable-Icon-Editor*) self )))))


(defmethod SAVE-BUTTON-PRESSED ((self agent-gallery-view))
  (print "SAVE BUTTON PRESSED")
  (inspect self)
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

(export '(update-buttons))


(defmethod add-agent-action ((w application-window) (Button bevel-button))
 ; (SHOW-AGENT-INPUT-WINDOW button)
  (let ((image-badge (value (view-named w "image-badge"))) (user-input (get-string-from-user "Please select a name and image for your agent" :initial-string "Unitled_Agent")))
    ;(setf (window image-badge) w)
    (if (add-group image-badge `(,(second user-input) nil nil )) ; (("shape5" "redlobster.png")( "shape4" "redlobster.png"))))
      (progn
        (print "1")
        (ccl::recursive-copy-directory
         (truename "lui:resources;templates;agents;default")
         (make-pathname :name (second user-input)  :directory (append (pathname-directory  (file (project-manager-reference image-badge))  )(list "agents")) )
         )
        (print "2")
        (add-subobject (project-manager-reference image-badge) (make-instance 'agent-manager :name (agentcubes:agentcubes-class-symbol (second user-input))
                                                :file   (make-pathname :name "index.agent" :directory (append (pathname-directory  (file (project-manager-reference image-badge))  )`( "agents" ,(second user-input))))))
        (print "3")
        ;(print (file (project-manager self)))
        )
      )
    ))


(defmethod delete-selected-agent ((w application-window) (Button bevel-button))
  (let ((image-badge (value (view-named w "image-badge"))) (name (value (view-named w "text-field"))))
    (if (selected-group-item image-badge)
      (delete-group-item image-badge  (selected-group image-badge) (selected-group-item image-badge))
      (delete-group image-badge  (selected-group image-badge))))) 

(defmethod add-shape-action ((w application-window) (Button bevel-button))
  (let ((image-badge (value (view-named w "image-badge"))) (user-input (get-string-from-user "Please select a name and image for your agent" :initial-string "Unitled_Agent")))
    (ccl::recursive-copy-directory
     (truename "lui:resources;templates;shapes;default")
     (make-pathname :name (second user-input)  :directory (append (pathname-directory  (file (project-manager-reference image-badge))  )`( "agents" , (selected-group image-badge) "shapes" )) )
     )
    (if (add-group-item image-badge (selected-group image-badge) `(,(second user-input) ,(first user-input) ) :image-path (make-pathname :directory (append (pathname-directory  (file (project-manager-reference image-badge))  )`( "agents" ,(selected-group image-badge) "shapes" ,(second user-input)))))
      (progn
        (print "Before")
        (add-subobject (find-agent-manager (project-manager-reference image-badge) (agentcubes:agentcubes-class-symbol(selected-group image-badge))) (make-instance 'shape-manager :name (agentcubes:agentcubes-symbol(second user-input))
                                                                                                                                      :file (make-pathname :name "index.shape" :directory (append (pathname-directory  (file (project-manager-reference image-badge))  )`( "agents" ,(selected-group image-badge) "shapes" ,(second user-input))))))
        (print "?????")
        (inspect (project-manager-reference image-badge))
       )
      
      ))
  )


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

#|
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

|#