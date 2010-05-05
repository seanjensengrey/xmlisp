(in-package :lui)

;;*********************************
;; Pop Up Image Menu              *
;;*********************************


(defclass POP-UP-IMAGE-GROUP-MENU ()
  ((native-window :accessor native-window :initform nil)
   (native-view :accessor native-view :initform nil)
   (x :accessor x :initform 200 :initarg :x :documentation "screen position, pixels")
   (y :accessor y :initform 590 :initarg :y :documentation "screen position, pixels")
   (width :accessor width :initform 500 :initarg :width :documentation "width in pixels")
   (height :accessor height :initform 300 :initarg :height :documentation "height in pixels")
   (image-height :accessor image-height :initform 20 :documentation "height of image")
   (image-width :accessor image-width :initform 20 :documentation "height of image")
   (shape-text-box :accessor shape-text-box :initform nil :documentation "a reference to the text field which is used to display the selcted shape")
   (shape-text-box-height :accessor shape-text-box-height :initform 20)
   (image-preview-view :accessor image-preview-view :initform nil)
   (image-preview-height :accessor image-preview-height :initform 32)
   (window-height :accessor window-height :initform 200 :documentation "height of window")
   (image-file-extension :accessor image-file-extension :initform "png" :initarg :image-file-extension :documentation "the extension of the image file")
   (image-pathname :accessor image-pathname :initform "lui:resources;buttons;" :initarg :image-pathname :documentation "the path to where the image files are stored")
   (image-border-thickness :accessor image-border-thickness :initform 2 :documentation "the thickness of the border of each image in the menu")
   (label-width :accessor label-width :initform 0.0 :documentation "Width of the labels")
   (index-of-selection :accessor index-of-selection :initform nil :documentation "the index of the selected item")
   (name-of-selection :accessor name-of-selection :initform nil :initarg :name-of-selection :documentation "the name of the selected item")
   (window-offset-x :accessor window-offset-x :initform 0 :documentation "the X offset needed to display the window in the propper location")
   (window-offset-y :accessor window-offset-y :initform 0 :documentation "the Y offset needed to display the window in the propper location")
   (images :accessor images :initform nil :initarg :images :documentation "A nested list of groups with images")
   )
  (:documentation "This class will pop up an an image menu that is seperated into groups.  It displays the given list of strings sorted by group."))


(defmethod INITIALIZE-INSTANCE :after  ((Self pop-up-image-group-menu) &rest Initargs)
  (declare (ignore initargs))
  (setf (window-height self) (+ (image-preview-height self)(shape-text-box-height self) (* (image-height self) (length (image-names self)))))
  (setf (width self) (get-window-width self)))


;---------------------------------
; Specification                  |
;________________________________


(defgeneric DISPLAY-POP-UP-MENU (pop-up-image-group-menu &key x y)
  (:documentation "Pops up the menu and returns the value of the selected image"))


(defgeneric IMAGE-NAMES (pop-up-image-group-menu)
  (:documentation "Returns the list of groups and their images that will be used by the menu"))


;---------------------------------
; Implementation                  |
;________________________________

(defmethod IMAGE-NAME-PATHNAME ((Self pop-up-image-menu) Name)
  (native-path  (image-pathname self)  Name))


(defmethod IMAGE-NAMES ((Self pop-up-image-group-menu))
  '(("Tools" ("draw-button" "erase-button"))
    ("Selection" ("magic-wand-button" "arrow-button" "select-ellipse-button"))
    ("Nagivation" ("zoom-button" "zoom-in-button" "zoom-out-button"))))


#| Examples:


(defclass TOOL-POP-UP-IMAGE-GROUP-MENU (pop-up-image-group-menu)
  ()
  (:documentation "select toolbar tools"))

;("blue-box" "lui:resources;images;blue-box.png")
(defmethod IMAGE-NAMES ((Self tool-pop-up-image-group-menu))
  '(("Tools" ("draw-button" "erase-button"))
    ("Selection" ("magic-wand-button" "arrow-button" "select-ellipse-button"))
    ("Nagivation" ("zoom-button" "pan-button" "rotate-button"))))


(defmethod SIZE-CHANGED-EVENT-HANDLER ((Self tool-pop-up-image-group-menu) Width Height)
  (declare (ignore Width Height))
  ;; nothing
  )

(defparameter *Pop-Up-Menu* (make-instance 'tool-pop-up-image-group-menu) )

(display-pop-up-menu *Pop-Up-Menu* :y 800 :x 800)


|#

