(in-package :lui)
;;_______________________________
;; Pop Up Image Menu            |
;;_______________________________


(export '(pop-up-image-group-menu))
(defclass POP-UP-IMAGE-GROUP-MENU ()
  ((native-window :accessor native-window :initform nil)
   (native-view :accessor native-view :initform nil)
   (x :accessor x :initform 200 :initarg :x :documentation "screen position, pixels")
   (y :accessor y :initform 590 :initarg :y :documentation "screen position, pixels")
   (width :accessor width :initform 500 :initarg :width :documentation "width in pixels")
   (height :accessor height :initform 300 :initarg :height :documentation "height in pixels")
   (image-height :accessor image-height :initform 20 :documentation "height of image")
   (image-width :accessor image-width :initform 20 :documentation "height of image")
   (window-height :accessor window-height :initform 200 :documentation "height of window")
   (image-file-extension :accessor image-file-extension :initform "png" :initarg :image-file-extension :documentation "the extension of the image file")
   (image-pathname :accessor image-pathname :initform "lui:resources;buttons;" :initarg :image-pathname :documentation "the path to where the image files are stored")
   (label-width :accessor label-width :initform 0.0 :documentation "Width of the labels")
   (index-of-selection :accessor index-of-selection :initform nil :documentation "the index of the selected item")
   (name-of-selection :accessor name-of-selection :initform nil :initarg :name-of-selection :documentation "the name of the selected item")
   (window-offset-x :accessor window-offset-x :initform 0 :documentation "the X offset needed to display the window in the propper location")
   (window-offset-y :accessor window-offset-y :initform 0 :documentation "the Y offset needed to display the window in the propper location")
   (images :accessor images :initform nil :initarg :images :documentation "A nested list of groups with images"))
  (:documentation "This class will pop up an an image menu that is seperated into groups.  It displays the given list of strings sorted by group."))


(defmethod INITIALIZE-INSTANCE :after  ((Self pop-up-image-group-menu) &rest Initargs)
  (declare (ignore initargs))
  (setf (window-height self) (* (image-height self) (length (image-names self))))
  (setf (width self) (get-window-width self)))


(defgeneric POP-UP-GROUP-MENU (pop-up-image-group-menu)
  (:documentation "Pops up the menu and returns the value of the selected image"))


(defgeneric IMAGE-NAMES (pop-up-image-group-menu)
  (:documentation "Returns the list of groups and their images that will be used by the menu"))


(defmethod IMAGE-NAME-PATHNAME ((Self pop-up-image-menu) Name)
  (native-path  (image-pathname self)  Name))


(defmethod image-names ((Self pop-up-image-group-menu))
  '(("Tools" ("draw-button" "erase-button"))
    ("Selection" ("magic-wand-button" "arrow-button" "select-ellipse-button"))
    ("Nagivation" ("zoom-button" "zoom-in-button" "zoom-out-button"))))


(export '(pc-pop-up-image-group-menu))
(defclass PC-POP-UP-IMAGE-GROUP-MENU (POP-UP-IMAGE-GROUP-MENU)
  ())

(defmethod image-names ((self PC-POP-UP-IMAGE-GROUP-MENU))
  '(("Tools" ("direction_center" "direction_center"))
    ("Selection" ("direction_center" "direction_center" "direction_center"))
    ("Nagivation" ("direction_center" "direction_center" "direction_center"))))
#|Example
(in-package :xlui)

(defparameter *Pop-Up-Menu* (make-instance 'POP-UP-IMAGE-GROUP-MENU :name-of-selection  "direction_north_west"))

(defmethod pop-up-image-menu-now ((w application-window) (Button button))
  (print (display-Pop-Up-menu *Pop-Up-Menu* )))

<application-window title="Currency Converter" width="300" height="180">
  <column align="stretch" valign="stretch" padding="9">
    <row align="stretch" minimize="vertical" valign="bottom">
      <button text="make-event" action="pop-up-image-menu-now" width="100" default-button="true"/>
    </row>
  </column>
</application-window>
    
|#

#|Example
(in-package :xlui)

(defparameter *Pop-Up-Menu* (make-instance 'PC-POP-UP-IMAGE-GROUP-MENU :name-of-selection "direction_north_west"  :image-pathname "lui:resources;buttons;direction;" ))

(defmethod pop-up-image-menu-now ((w application-window) (Button button))
  (print (display-Pop-Up-menu *Pop-Up-Menu* )))

<application-window title="Currency Converter" width="300" height="180">
  <column align="stretch" valign="stretch" padding="9">
    <row align="stretch" minimize="vertical" valign="bottom">
      <button text="make-event" action="pop-up-image-menu-now" width="100" default-button="true"/>
    </row>
  </column>
</application-window>
    
|#