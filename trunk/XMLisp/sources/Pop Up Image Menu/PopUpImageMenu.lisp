(in-package :lui)

;(export '(GENERAL-POP-UP-IMAGE-MENU DISPLAY-POP-UP-MENU DISPLAY-POP-UP))
;;_______________________________
;; Pop Up Image Menu            |
;;_______________________________

(defclass POP-UP-IMAGE-MENU ()
  ()
  (:documentation "An image pop up menu"))

(defgeneric POP-UP (pop-up-image-menu)
  (:documentation "Pops up the menu and returns the value of the selected image"))

(defgeneric CLOSE-WINDOW (pop-up-image-menu)
  (:documentation "Is called when the window needs to be closed, this method will stop the application from running the window modally."))

(defgeneric POP-UP (pop-up-image-menu)
  (:documentation "Pops up the menu and returns the value of the selected image"))

;;_______________________________
;; General Pop Up Image Menu    |
;;_______________________________

(defclass GENERAL-POP-UP-IMAGE-MENU (pop-up-image-menu)
  (
   (native-window :accessor native-window :initform nil)
   (native-view :accessor native-view :initform nil)
   (x :accessor x :initform 200 :initarg :x :documentation "screen position, pixels")
   (y :accessor y :initform 690 :initarg :y :documentation "screen position, pixels")
   (width :accessor width :initform 500 :initarg :width :documentation "width in pixels")
   (height :accessor height :initform 500 :initarg :height :documentation "height in pixels")
   (image-height :accessor image-height :initform 20 :documentation "height of image")
   (image-width :accessor image-width :initform 20 :documentation "height of image")
   (image-file-extension :accessor image-file-extension :initform "png" :initarg :image-file-extension :documentation "the extension of the image file")
   (image-pathname :accessor image-pathname :initform "lui:resources;buttons;direction;" :initarg :image-pathname :documentation "the path to where the image files are stored")
   (index-of-selection :accessor index-of-selection :initform nil :documentation "the index of the selected item")
   (name-of-selection :accessor name-of-selection :initform nil :initarg :name-of-selection :documentation "the name of the selected item")
   (window-offset-x :accessor window-offset-x :initform 0 :documentation "the X offset needed to display the window in the propper location")
   (window-offset-y :accessor window-offset-y :initform 0 :documentation "the Y offset needed to display the window in the propper location")
   (images :accessor images :initform nil :initarg :images :documentation "A list of images for the Menu")
   )
  (:documentation "Image Pop Up Menu"))

(defmethod image-names ((Self general-pop-up-image-menu))
  (images self))

(defmethod IMAGE-NAME-PATHNAME ((Self general-pop-up-image-menu) Name)
  (native-path  (image-pathname self)  Name))

(defmethod NUMBER-OF-COLUMNS ((Self general-pop-up-image-menu))
  (case (length (image-names Self))
    (0 1)
    (t (isqrt (length (image-names Self))))))

(defmethod NUMBER-OF-ROWS ((Self general-pop-up-image-menu))
  (case (length (image-names Self))
    (0 1)
    (t (ceiling (length (image-names Self)) (number-of-columns Self)))))



(defmethod GET-X-GRID-POSITION ((Self general-pop-up-image-menu))
  (mod (index-of-selection self) (number-of-columns self)))

(defmethod GET-Y-GRID-POSITION ((Self general-pop-up-image-menu))
  (number-of-rows self) (floor (/ (index-of-selection self) (number-of-columns self))))

(defmethod GET-SELECTION-INDICES ((Self general-pop-up-image-menu))"
  Returns the Vertex Texture-vertex and normal seperatley from the face"
    (values 
     (GET-X-GRID-POSITION self)
     (GET-Y-GRID-POSITION self)))




