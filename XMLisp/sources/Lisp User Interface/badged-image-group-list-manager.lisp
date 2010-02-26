(in-package :lui)


;(export '( badged-image-group-list-manager-view group-name row-height group-list superview group-item-offset edit-group group-view ))


(defclass BADGED-IMAGE-GROUP-LIST-MANAGER-VIEW (view)
  ((native-window :accessor native-window :initform nil)
   (native-view :accessor native-view :initform nil)
   (x :accessor x :initform 400 :initarg :x :documentation "screen position, pixels")
   (y :accessor y :initform 890 :initarg :y :documentation "screen position, pixels")
   (width :accessor width :initform 300 :initarg :width :documentation "width in pixels")
   (height :accessor height :initform 300 :initarg :height :documentation "height in pixels")
   (group-list :accessor group-list :initform nil :initarg :group-list :documentation "a list of the groups")
   (groups :accessor groups :initform nil :initarg :groups :documentation "a list of group objects")
   (row-height :accessor row-height :initform 30 :initarg :row-height :documentation "the height of each row in the list")
   (text-height :accessor text-height :initform 15 :initarg :text-height :documentation "the height of the text field")
   (superview :accessor superview :initform nil :documentation "the superview of this view")
   (group-item-offset :accessor group-item-offset :initform 60 :documentation "the X offset of the group items")
   (head-image-name :accessor head-image-name :initform "color-wheel-button.png" :initarg :head-image-name :documentation "the image that will be used as the group image")
  ; (update-action :accessor update-action :initarg :update-action :initform 'layout-changed)
   (window :accessor window :initform nil)
   (left-margin :accessor left-margin :initform 0))
   (:documentation "This class will is a view that contains groups of items that are identified by a badge image."))


(defclass LIST-GROUP ()
  ((x :accessor x :initform 400 :initarg :x :documentation "screen position, pixels")
   (y :accessor y :initform 890 :initarg :y :documentation "screen position, pixels")
   (group-name :accessor group-name :initform nil :initarg :group-name)
   (group-image :accessor group-image :initform nil :initarg :group-image)
   (is-disclosed :accessor is-disclosed :initform nil )
   (is-selected :accessor is-selected :initform nil :initarg :is-selected )
   (group-items :accessor group-items :initform nil :initarg :group-items)
   (item-view :accessor item-view :initform nil :documentation "A reference to the view that contains this groups items")
   (group-view :accessor group-view :initform nil :documentation "A reference to the view that contains this group")
   (image-view :accessor image-view :initform nil :documentation "A reference to the view that contains this group's image")
   (button-view :accessor button-view :initform nil :documentation "A reference to the view that contains this group's disclosure button")
   (text-view :accessor text-view :initform nil :documentation "A reference to the view that contains this group's text field")
   (selection-view :accessor selection-view :initform nil :documentation "A reference to the view that  annotates the group as selected")
   (item-selection-view :accessor item-selection-view :initform nil :documentation "A reference to the view that annotates which item is selected.")
   (item-detection-views :accessor item-detection-views :initform nil :documentation "A list of the groups item detection views")
   (item-text-views :accessor item-text-views :initform nil :documentation "A list of the groups item text views")
   (selected-item-name :accessor selected-item-name :initform nil))
   (:documentation "A group consists of a name an image and a list of group items."))


(defclass LIST-GROUP-ITEM ()
  (
   (item-name :accessor item-name :initform nil :initarg :item-name :documentation "the name of the item")
   (image-name :accessor image-name :initform nil :initarg :image-name :documentation "the name of the item's image file")
   (image-path :accessor image-path :initform "lui:resources;images;" :initarg :image-path :documentation "the path to the item's image file")
   (item-detection-view :accessor item-detection-view :initform nil :initarg :item-detection-view :documentation "a reference the items detection view")
    )
   (:documentation "A group consists of a name an image and a list of group items."))


(defgeneric add-group (badged-image-group-list-manager-view group)
  (:documentation "Adds a new group to the list maanger.  The input group will be a list that contains: first an image name second the group name
                  and finally a list of group items.  This method will return the group of it was sucesfully added or nil if it could not be added.  "))


(defgeneric add-group-item (badged-image-group-list-manager-view group-name group-item &key image-path )
  (:documentation "Adds a new group item to the group defined by group-name.  This method will return sucess if the item was added corectly or nil if it was not. "))


(defgeneric edit-group (badged-image-group-list-manager-view group-name)
  (:documentation "This method is called when a group image is double clicked on.  This method should be overriden to provide whatever behavior is appopriate."))


(defgeneric edit-group-item (badged-image-group-list-manager-view group-name item-name)
  (:documentation "This method is called when a group item image is double clicked on.  This method should be overriden to provide whatever behavior is appopriate."))


(defgeneric delete-group (badged-image-group-list-manager-view group-name)
  (:documentation "Deletes the group specified by group-name."))


(defgeneric delete-group-item (badged-image-group-list-manager-view group-name group-item-name)
  (:documentation "Deletes the group specified group-item from the specificed group."))


(defgeneric selected-group (badged-image-group-list-manager-view)
  (:documentation "Return the group which is selected."))


(defgeneric selected-group-item (badged-image-group-list-manager-view)
  (:documentation "Returns the selcted group-item and which group it is a part of."))


(defgeneric group-badge-image (badged-image-group-list-manager-view group-name)
  (:documentation "Returns the image to use for the badge of the specified group."))


(defgeneric select-group (badged-image-group-list-manager-view group-name item-name)
  (:documentation "Select the specifiecd group."))


(defgeneric select-group-item (badged-image-group-list-manager-view group-name group-item-name)
  (:documentation "Select the specifiecd group-item of the specified group."))


(defgeneric layout-changed (badged-image-group-list-manager-view)
  (:documentation "This method will be called everytime the layout changes.  This method should be overrided to do things like update the status of buttons that are dependent on selections of groups or items.  "))


(defmethod MAKE-SELECTION ((self badged-image-group-list-manager-view) group-name)
  (set-selected self group-name))


(defmethod layout-changed ((self badged-image-group-list-manager-view))
  (print "Layout has changed, please override me."))


(defmethod EDIT-GROUP ((self badged-image-group-list-manager-view) group-name)
  (declare (ignore group-name))
  (print "Editting group.  Please override me" ))


(defmethod EDIT-GROUP-ITEM ((self badged-image-group-list-manager-view) group-name item-name)
  (declare (ignore group-name item-name))
  (print "Editting group item.  Please override me" ))

  
(defmethod MAP-SUBVIEWS ((Self badged-image-group-list-manager-view) Function &rest Args)
  (declare (ignore Function Args))
  ;; no lui-views in here
  )


(defmethod SELECTED-GROUP ((Self badged-image-group-list-manager-view))
  (dolist (group (groups self))
    (if (is-selected group)
      (return-from selected-group (group-name group)))))


(defmethod SELECTED-GROUP-ITEM ((Self badged-image-group-list-manager-view))
  (dolist (group (groups self))
    (if (is-selected group)       
      (return-from selected-group-item (selected-item-name group)))))


(defmethod SELECT-GROUP ((Self badged-image-group-list-manager-view)group-name group-item-name)
  (dolist (group (groups self))
    (setf (is-selected group) nil)
    (if (equal (group-name group) group-name)
      (progn
        (setf (is-selected group) "YES")
        (if group-item-name
          (progn
            (dolist (item (group-items group))
              (setf (selected-item-name group) nil)
              (if (equal (item-name item) group-item-name)
                (setf (selected-item-name group) (item-name item))))))))))


#|
Example
(in-package :xlui)

(defparameter *group-list-manager* (make-instance 'badged-image-group-list-manager-view))

(defmethod pop-up-image-menu-now ((w application-window) (Button button)) 

  (add-group *group-list-manager* '("lobster1" "redlobster.png") )
  (print (group-list *group-list-manager*))
  
  (add-group *group-list-manager* '("lobster2" "redlobster.png"  (("shape1" "redlobster.png")( "shape2" "redlobster.png"))) )
  (print (group-list *group-list-manager*))
  
  (add-group *group-list-manager* '("lobster3" "redlobster.png"  (("shape3" "redlobster.png")( "shape4" "redlobster.png"))) )
  ;(setf (lui::is-disclosed (elt  (lui::groups *group-list-manager*) 1)) "YES")
  (select-group *group-list-manager* "lobster3" nil) 
  (print "selected group")
  (print (lui::selected-group  *group-list-manager*))
  (delete-group-item  *group-list-manager* "lobster2" "shape2")
 ; (inspect (groups *group-list-manager*))
  (print (make-test-window *group-list-manager* )))

<application-window title="Currency Converter" width="300" height="180">
  <column align="stretch" valign="stretch" padding="9">
    <row align="stretch" minimize="vertical" valign="bottom">
      <button text="make-event" action="pop-up-image-menu-now" width="100" default-button="true"/>
    </row>
  </column>
</application-window>
    
|#