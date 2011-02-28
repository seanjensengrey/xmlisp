(in-package :lui)


;;*********************************
;; BADGED IMAGE GROUP LIST MANAGER*
;;*********************************


(defclass BADGED-IMAGE-GROUP-LIST-MANAGER-VIEW (view)
  ((selected-group-changed-action :accessor selected-group-changed-action  :type symbol :initform 'default-selected-group-changed-action)
   (native-window :accessor native-window :initform nil)
   (native-view :accessor native-view :initform nil)
   (x :accessor x :initform 400 :initarg :x :documentation "screen position, pixels")
   (y :accessor y :initform 890 :initarg :y :documentation "screen position, pixels")
   (width :accessor width :initform 300 :initarg :width :documentation "width in pixels")
   (height :accessor height :initform 300 :initarg :height :documentation "height in pixels")
   (minimum-width :accessor minimum-width :initform 200 :documentation "the minimum bounds for the width of this view")
   (group-list :accessor group-list :initform nil :initarg :group-list :documentation "a list of the groups")
   (groups :accessor groups :initform nil :initarg :groups :documentation "a list of group objects")
   (row-height :accessor row-height :initform 30 :initarg :row-height :documentation "the height of each row in the list")
   (text-height :accessor text-height :initform 16 :initarg :text-height :documentation "the height of the text field")
   (superview :accessor superview :initform nil :documentation "the superview of this view")
   (group-item-offset :accessor group-item-offset :initform 40 :documentation "the X offset of the group items")
   (head-image-name :accessor head-image-name :initform "color-wheel-button.png" :initarg :head-image-name :documentation "the image that will be used as the group image")
  ; (update-action :accessor update-action :initarg :update-action :initform 'layout-changed)
   (window :accessor window :initform nil)
   (left-margin :accessor left-margin :initform 0)
   (item-category-label :accessor item-category-label :initarg :item-category-label :initform "items:")
   (item-category-label-height :accessor item-category-label-height :initarg :item-category-label-height :initform 15))
  (:documentation "This class will is a view that contains groups of items that are identified by a badge image."))


;;*********************************
;; LIST GROUP                     *
;;*********************************


(defclass LIST-GROUP ()
  ((x :accessor x :initform 400 :initarg :x :documentation "screen position, pixels")
   (y :accessor y :initform 890 :initarg :y :documentation "screen position, pixels")
   (group-name :accessor group-name :initform nil :initarg :group-name)
   (group-image :accessor group-image :initform nil :initarg :group-image)
   (is-disclosed :accessor is-disclosed :initform nil :initarg :is-disclosed)
   (is-selected :accessor is-selected :initform nil :initarg :is-selected )
   (is-highlighted :accessor is-highlighted :initform nil :initarg :is-highlighted)
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


;;*********************************
;; LIST GROUP ITEM                *
;;*********************************


(defclass LIST-GROUP-ITEM ()
  ((item-name :accessor item-name :initform nil :initarg :item-name :documentation "the name of the item")
   (image-name :accessor image-name :initform nil :initarg :image-name :documentation "the name of the item's image file")
   (image-path :accessor image-path :initform "lui:resources;images;" :initarg :image-path :documentation "the path to the item's image file")
   (item-detection-view :accessor item-detection-view :initform nil :initarg :item-detection-view :documentation "a reference the items detection view")
   (text-view :accessor text-view :initform nil :documentation "a reference to the item's text field")
   (item-counter :accessor item-counter :initform nil :documentation "a reference to the item's item counter"))
   (:documentation "A group consists of a name an image and a list of group items."))


;---------------------------------
; Specification                  |
;________________________________


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


(defgeneric set-selected (badged-image-group-list-manager-view group-name &key highlight resign)
  (:documentation "Select the specifiecd group."))


(defgeneric set-selected-item (badged-image-group-list-manager-view group-name item-name)
  (:documentation "Select the specifiecd group-item of the specified group."))


(defgeneric layout-changed (badged-image-group-list-manager-view)
  (:documentation "This method will be called everytime the layout changes.  This method should be overrided to do things like update the status of buttons that are dependent on selections of groups or items.  "))


(defgeneric group-deselected (badged-image-group-list-manager-view)
  (:documentation "This method will be called when an item or group is deselected in the gallery.  "))


(defgeneric group-name-changed (badged-image-group-list-manager-view group-name new-name)
  (:documentation "This method will be called when a groups name has been changed.  "))

(defgeneric item-name-changed (badged-image-group-list-manager-view group-name item-name new-name)
  (:documentation "This method will be called when a item's name has been changed.  "))

(defgeneric take-focus (badged-image-group-list-manager-view)
  (:documentation "A method to be called when another view takes focus from this view."))

;---------------------------------
; Implementation                  |
;________________________________


(defmethod DEFAULT-SELECTED-GROUP-CHANGED-ACTION ((window window)(self badged-image-group-list-manager-view))
  (print "DEFAULT SELECTED GROUP CHANGED ACTION"))


(defmethod MAKE-SELECTION ((self badged-image-group-list-manager-view) group-name)
  (set-selected self group-name))


(defmethod layout-changed ((self badged-image-group-list-manager-view))
  ;nothing
  )


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
    (when (is-selected group)
      (return-from selected-group (group-name group)))))


(defmethod SELECTED-GROUP-ITEM ((Self badged-image-group-list-manager-view))
  (dolist (group (groups self))
    (if (is-selected group)       
      (return-from selected-group-item (selected-item-name group)))))


(defmethod SELECT-GROUP ((Self badged-image-group-list-manager-view)group-name group-item-name)
  (if group-item-name
    (set-selected-item self group-name group-item-name)
    (set-selected self group-name)))


(defmethod GROUP-DESELECTED ((Self badged-image-group-list-manager-view))
  (set-selected self nil))


(defmethod group-name-changed ((Self badged-image-group-list-manager-view) group-name new-name)
  (declare (ignore group-name new-name))
  (print "GROUP NAME CHANGED"))


(defmethod item-name-changed ((Self badged-image-group-list-manager-view) group-name item-name new-name)
  (declare (ignore group-name item-name new-name))
  (print "ITEM NAME CHANGED"))


(defmethod TAKE-FOCUS ((Self badged-image-group-list-manager-view))
  (declare (ftype function remove-background-and-end-editting-for-all-text-fields)) ;; implemented in cocoa specific file
  (remove-background-and-end-editting-for-all-text-fields (native-view self)))

#|
Example:

(in-package :xlui)

<application-window title="agent-gallery"  margin="0" height="250">
  <column align="stretch" valign="stretch">  
    <scroll-box vflex="1">
      <badged-image-group-list-manager name="image-badge">
        <image-badge-group-list-item name="lobster!!" />
        <image-badge-group-list-item name="lobster2" />
        <image-badge-group-list-item name="lobster3" />
      </badged-image-group-list-manager>
    </scroll-box>
  </column>
</application-window> 
    
|#