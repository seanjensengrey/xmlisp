;; Start XMlisp
;; 04/24/09 Alexander Repenning
;; 5/20/09 + speech
;; for CCL 1.3 
;; 5/28/09
;; 07/15/09 + font support
;; 02/17/10 + inflatable icons and inflatable icon editor window

(in-package :cl-user)

;<application-window><agent-3d-view><text-3d/></agent-3d-view></application-window>

;; temporary fix for CCL's issue with turning warnings into errors
;; should remove with new release
(setq ccl::*objc-error-return-condition* 'error)

(objc:defmethod (#/invokeLispFunction: :void) ((self ns:ns-application) id)
  (gui::invoke-lisp-function self id))


;***************** Settings

;; edit to point to root folder containing /sources  /resources  etc.
#-cocotron
(setf (logical-pathname-translations "lui")
      '(("**;*.*" "home:working copies;Xmlisp svn;trunk;XMLisp;**;")))

#+cocotron
(setf (logical-pathname-translations "mac-home")
`((,(make-pathname :host "mac-home"
                   :directory '(:absolute :wild-inferiors)
                   :name :wild
                   :type :wild
                   :version :wild)
   #p"z:/**/*.*")))


#+cocotron
(setf (logical-pathname-translations "lui")
      '(("**;*.*" "mac-home:working copies;Xmlisp svn;trunk;XMLisp;**;")))


#-cocotron
(setq gui::*paren-highlight-background-color*
      (#/retain (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.9 0.8 0.8 1.0)))

#-cocotron
(setq GUI::*Editor-keep-backup-files* nil)

(setq ccl::*verbose-eval-selection* t)

;; Load

;; IDE
  
#-cocotron (load "lui:sources;IDE;specific;Mac CCL;anticipat-symbol-complete")
#-cocotron (load "lui:sources;IDE;specific;Mac CCL;ns timer")
#-cocotron (load "lui:sources;IDE;specific;Mac CCL;GLDocs")
#-cocotron (load "lui:sources;IDE;specific;Mac CCL;hemlock extensions")


;; Open

;;(ed "lui:sources;XLUI;examples;2D;test case examples.lisp")
;; (ed "lui:sources;XLUI;examples;AgentCubes Design.lisp")

;;(ed "lui:sources;Lisp User Interface;LUI.lisp")
;;(ed "lui:sources;Lisp User Interface;specific;Mac CCL;LUI Cocoa.lisp")


;;****** INFIX

(defpackage :INFIX
  (:use :common-lisp))


;;****** OpenGL

(defpackage :OPENGL
  (:use :common-lisp))

(load "lui:sources;OpenGL;specific;Mac CCL;OpenGL-interface")


;****** XMLisp

(defpackage :XML
  (:use :common-lisp)
  (:export "XML-SERIALIZER" "PARSE-FILE-NAME"))

(load "lui:sources;XMLisp;XMLisp")

;; temporarily set xmlisp package to only be :xml to avoid confusion with xml-like structures in Cocoa
(setf xml::*XMLisp-Packages* (list (find-package :xml)))

;;****** LUI

(defpackage :LUI
  (:use :common-lisp :ccl :opengl)
  (:export 
   "MOUSE-EVENT" "TRACK-MOUSE" "VIEW-MOUSE-MOVED-EVENT-HANDLER" "VIEW-LEFT-MOUSE-DOWN-EVENT-HANDLER" "VIEW-RIGHT-MOUSE-DOWN-EVENT-HANDLER"  "KEY-EVENT-HANDLER" "*CURRENT-EVENT*"
   "KEY-EVENT-HANDLER" "KEY-EVENT" "KEY-CODE"
   "VIEW-LEFT-MOUSE-UP-EVENT-HANDLER" "VIEW-LEFT-MOUSE-DRAGGED-EVENT-HANDLER"
   "COMMAND-KEY-P" "SHIFT-KEY-P" "ALT-KEY-P" "CONTROL-KEY-P" "DOUBLE-CLICK-P"
   "WINDOW" "VIEW" "X" "Y" "WIDTH" "HEIGHT" "SHOW" "DO-SHOW-IMMEDIATELY" "SHOW-AND-RUN-MODAL" "STOP-MODAL" "CANCEL-MODAL" "HIDE"  "HAS-BECOME-MAIN-WINDOW" "WINDOW-WILL-CLOSE" "WINDOW-SHOULD-CLOSE" "MAKE-KEY-WINDOW" "START-ACCEPTING-MOUSE-MOUVED-EVENTS"
   "SWITCH-TO-FULL-SCREEN-MODE" "SWITCH-TO-WINDOW-MODE" "FULL-SCREEN" "WINDOW-CLOSE"
   "FIND-WINDOW-AT-SCREEN-POSITION" "FIND-VIEW-CONTAINING-POINT" "FIND-VIEW-AT-SCREEN-POSITION"
   "SCROLL-VIEW" "HAS-HORIZONTAL-SCROLLER" "HAS-VERTICAL-SCROLLER"
   "SCROLL-VIEW-ADJUSTING-CONTENTS"
   "SET-SIZE" "SET-POSITION" "DISPLAY" "DRAW" "PREPARE-OPENGL" "CLEAR-BACKGROUND" "RECURSIVE-MAP-SUBVIEWS"
   "WINDOW-X" "WINDOW-Y"
   "RECTANGLE-VIEW" "SET-COLOR"
   "ZOOMABLE" "MINIMIZABLE" "RESIZABLE" "CLOSEABLE" "BORDERLESS" "TITLE" "WINDOW-NEEDS-SAVING-P"
   "SCREEN-WIDTH" "SCREEN-HEIGHT" "CONTROL" "VALUE" "INITIALIZE-EVENT-HANDLING" "SIZE-CHANGED-EVENT-HANDLER"
   "ACTION" "SUBVIEWS" "DO-SUBVIEWS" "MAP-SUBVIEWS" "ADD-SUBVIEW" "SUPERVIEW" "ADD-SUBVIEWS" "SWAP-SUBVIEW" "SUBVIEWS-SWAPPED" "SET-FRAME" 
   "BUTTON-CONTROL" "DEFAULT-BUTTON"
   "INVOKE-ACTION"
   "BEVEL-BUTTON-CONTROL" "DISABLE" "SLIDER-CONTROL" "TICK-MARKS" "MIN-VALUE" "MAX-VALUE" "ENABLE"
   "LABEL-CONTROL" "TEXT" "ALIGN"
   "EDITABLE-TEXT-CONTROL"
   "STATUS-BAR-CONTROL"
   "PROGRESS-INDICATOR-CONTROL" "START-ANIMATION" "STOP-ANIMATION"
   "DETERMINATE-PROGRESS-INDICATOR-CONTROL" "MAX-VALUE" "MIN-VALUE" "INCREMENT-BY"
   "VALUE"
   "IMAGE-CONTROL" "SRC" "SOURCE" "IN-CLUSTER"  "IMAGE-PATH" "CHANGE-IMAGE" "SCALE-PROPORTIONALLY";; "FILE"->"SOURCE"
   "RADIO-BUTTON-CONTROL" 
   "IMAGE-BUTTON-CLUSTER-CONTROL" "SELECTED-IN-CLUSTER"
   "IMAGE-BUTTON-CLUSTER" "CHANGE-CLUSTER-SELECTIONS"
   "DEFAULT-ACTION"
   "CHECKBOX-CONTROL" "START-CHECKED" "IMAGE-ON-RIGHT"
   "IMAGE-BUTTON-CONTROL" "IMAGE" "CONTAINER" "CLUSTER-ACTION" "USER-ACTION" "SET-BUTTON-OFF" "SET-BUTTON-ON" "KEY-EQUIVALENT"
   "RADIO-BUTTON-CONTROL" "ADD-ITEM" "FINALIZE-CLUSTER" "RADIO-ACTION"
   "POPUP-BUTTON-CONTROL"  "POPUP-ACTION" "SET-SELECTED-ITEM-WITH-TITLE"
   "POPUP-IMAGE-BUTTON-CONTROL" "POPUP-MENU-CELL" "ENABLE-ITEM" "DISABLE-ITEM" "ENABLE-ITEM-WITH-TITLE" "DISABLE-ITEM-WITH-TITLE" "DRAW-DISCLOSURE"
   "POPUP-IMAGE-BUTTON-ITEM-CONTROL" "ENABLE-PREDICATE" "ADD-POPUP-ITEM" "ADD-POPUP-SUBMENU" "ADD-POPUP-SUBMENU2" "SMALL-SCROLLER-SIZE"
   "POPUP-IMAGE-BUTTON-SUBMENU-CONTROL" "ADD-ITEM-TO-SUBMENU" "ADD-SUBMENU-TO-SUBMENU"
   "ADD-GROUP" "ADD-GROUP-ITEM" "DELETE-GROUP" "DELETE-GROUP-ITEM" "SELECTED-GROUP" "SELECTED-GROUP-ITEM" "SELECT-GROUP" "SELECT-GROUP-ITEM"
   "SCROLLER-CONTROL" "SET-SCROLLER-POSITION" "KNOB-PROPORTION"
   "SHOW-STRING-POPUP"
   "STRING-LIST-CONTROL" "SELECTED-STRING"
   "ATTRIBUTE-VALUE-LIST-VIEW-CONTROL" "ATTRIBUTE-EDITOR-VIEW" "ATTRIBUTE-VALUE-LIST-TEXT-VIEW" "ATTRIBUTE-CHANGED-ACTION"
   "STRING-LIST-VIEW-CONTROL" "ADD-STRING-LIST-ITEM" "SET-LIST" "SELECTED-STRING"
   "TAB-VIEW-CONTROL" "ADD-TAB-VIEW-ITEM"
   "TAB-VIEW-ITEM-CONTROL" "ADD-TAB-VIEW-ITEM-VIEW"
   "CHOICE-BUTTON-CONTROL" "ADD-MENU-ITEM" "GET-SELECTED-ACTION" "CHOICE-BUTTON-ACTION"
   "POP-UP-IMAGE-MENU" "DISPLAY-POP-UP-MENU" "DISPLAY-POP-UP" "SELECTED-IMAGE-NAME"
   "POP-UP-IMAGE-GROUP-MENU" "IMAGE-NAMES"
   "DIRECTION-POP-UP-IMAGE-MENU" "DISPLAY-POP-UP"
   "COLOR-WELL-CONTROL" "COLOR" "GET-RED" "GET-GREEN" "GET-BLUE" "GET-ALPHA" "SHOW-ALPHA"
   "WEB-BROWSER-CONTROL" "URL"
   "OPENGL-VIEW" "FRAME-RATE" "ANIMATE" "DELTA-TIME" "TEXTURES" "START-ANIMATION" "STOP-ANIMATION" "IS-ANIMATED" "FULL-SCENE-ANTI-ALIASING" "ANIMATE-OPENGL-VIEW-ONCE"
   "OBJECT-COORDINATE-VIEW-WIDTH" "OBJECT-COORDINATE-VIEW-HEIGHT"
    "USE-TEXTURE"
    "CREATE-TEXTURE-FROM-FILE"
    "SEPERATOR-CONTROL"
    "native-view"
   "CAMERA" "EYE-X" "EYE-Y" "EYE-Z" "CENTER-X" "CENTER-Y" "CENTER-Z"  "UP-X" "UP-Y" "UP-Z"
   "FOVY" "ASPECT" "NEAR" "FAR" "AZIMUTH" "ZENITH"
   "AIM-CAMERA" "WITH-GLCONTEXT" "WITH-GLCONTEXT-NO-FLUSH" "RENDER-FOR-SELECTION-MODE"
   "SHARED-OPENGL-VIEW"
   "NATIVE-PATH"
   ;; Dialogs
   "STANDARD-ALERT-DIALOG"
   ;; colors
   "*SYSTEM-SELECTION-COLOR*"
   ;; Multimedia
   "PLAY-SOUND" "SOUND-FILES-IN-SOUND-FILE-DIRECTORY" "SHUT-UP-SOUNDS"
   "SYNTHESIZER" "SPEAK" "WILL-SPEAK-WORD" "WILL-SPEAK-PHONEME" "DID-FINISH-SPEAKING" "AVAILABLE-VOICES"
   ;; native support
   "NATIVE-STRING"
   ;; badged image
   "BADGED-IMAGE-GROUP-LIST-MANAGER-VIEW" "ITEM-CATEGORY-LABEL" "ADD-GROUP"  "ADD-GROUP-ITEM" "DELETE-GROUP" "DELETE-GROUP-ITEM" "SELECTED-GROUP" "SELECTED-GROUP-ITEM" "SELECT-GROUP" "SELECT-GROUP-ITEM" "GROUP-NAME" "GROUPS" "TAKE-FOCUS"
   "SELECTED-GROUP-CHANGED-ACTION" "GROUP-ITEMS" "GROUP-NAME-CHANGED" "ITEM-NAME-CHANGED"
   "INCREMENT-ITEM-COUNTER" "CLEAR-ITEM-COUNTERS"
   "PROJECT-MANAGER-REFERENCE" "AGENT-GALLERY-VIEW"
   )
  (:import-from "XML"
                "FILE"))



(defun LUI::NATIVE-PATH (Directory-Name File-Name) "
  in: Directory-Name logical-pathname-string, e.g., ''lui:resources;textures;''
      File-Name string.
  out: Native-Path-String
  Create a native, OS specific, path from a platform independend URL style path"
  (format nil "~A~A" (truename Directory-Name) File-Name))




;; frameworks

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ccl:use-interface-dir :GL)
  #-windows-target (open-shared-library "/System/Library/Frameworks/OpenGL.framework/OpenGL")
  #+windows-target (open-shared-library "opengl32.dll"))


;; files

(load "lui:sources;Lisp User Interface;specific;Mac CCL;memory")
(load "lui:sources;Lisp User Interface;error-handling")
(load "lui:sources;Lisp User Interface;LUI")
(load "lui:sources;Lisp User Interface;Camera")
(load "lui:sources;Lisp User Interface;OpenGL-view")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;LUI Cocoa")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;standard-alert-dialog")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;get-color-from-user")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;image-import")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;image-export")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;OpenGL-view Cocoa")

(load "lui:sources;Lisp User Interface;specific;Mac CCL;Transparent-OpenGL-Window")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;speech")
(load "lui:sources;Lisp User Interface;pop-up-image-menu")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;pop-up-image-menu-cocoa")
(load "lui:sources;Lisp User Interface;pop-up-image-group-menu")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;pop-up-image-group-menu-cocoa")

;; media
(load "lui:sources;Lisp User Interface;specific;Mac CCL;play-sound")


;; add :xlui package
(setf xml::*XMLisp-Packages* (append xml::*XMLisp-Packages* (list (find-package :xlui))))



;****** XLUI

(defpackage :XLUI
  (:use :common-lisp :XML :LUI :opengl)
  ;; import MOP
  (:import-from "CCL"
                "SLOT-DEFINITION-NAME" "SLOT-DEFINITION-TYPE")
  (:import-from "XML"
                "FILE")
  (:export "INFLATABLE-ICON"
           "SAVE-THE-WORLD"
           "DEPTH"
           "PROJECT-CHOOSER" "OPEN-PROJECT-ACTION" "CREATE-NEW-PROJECT-ACTION" "OPEN-PROJECT"))


(setq xml::*xmlisp-packages* (list (find-package :xlui) (find-package :xml)))


;; badged image group list manager
(load "lui:sources;Lisp User Interface;badged-image-group-list-manager")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;badged-image-group-list-manager-cocoa")


(load "lui:sources;XLUI;xml-layout")
(load "lui:sources;XLUI;controls")      ;; LOTS of undefined functions still!
(load "lui:sources;XLUI;application-window")
(load "lui:sources;XLUI;dialog-window")
(load "lui:sources;XLUI;Font-Manager")
(load "lui:sources;XLUI;String-Shape")
(load "lui:sources;XLUI;Cursor-Manager")
(load "lui:sources;XLUI;agent-3D")
(load "lui:sources;XLUI;shapes")
(load "lui:sources;XLUI;keyboard")
;; inflatable icons
(load "lui:sources;XLUI;image editor;selection-mask")
(load "lui:sources;XLUI;image editor;image-editor")
(load "lui:sources;XLUI;image editor;Inflatable-Icon")
(load "lui:sources;XLUI;image editor;Inflatable-Icon-Window")


;; Agent Warp Engine
(load "lui:sources;XLUI;AWE;infix")
(load "lui:sources;XLUI;AWE;VAT-Formulas")
(load "lui:sources;XLUI;AWE;Equation")
(load "lui:sources;XLUI;AWE;Morph")


;; Dialogs

(load "lui:sources;XLUI;dialogs;get-string-from-user")
(load "lui:sources;XLUI;dialogs;yes-no-dialog")
(load "lui:sources;XLUI;dialogs;get-new-agent-info-from-user-window")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;chooser-file-dialogs")

(load "lui:sources;XLUI;progress-meter")


;;*************** Build functions

(defun ccl::CCL-CONTENTS-DIRECTORY ()
  (let* ((heap-image-path (ccl::%realpath (ccl::heap-image-name))))
    (make-pathname :directory (butlast (pathname-directory heap-image-path))
                   :device (pathname-device heap-image-path))))


(defun RESTORE-XMLISP ()
  (print "RESTORE XMLISP")
  (let* ((contents-dir (ccl::ccl-contents-directory))
         (toplevel-dir (make-pathname :directory (butlast (pathname-directory contents-dir) 2)
                                      :device (pathname-device contents-dir))))
    (setf (logical-pathname-translations "lui")
          `(("examples;**;*.*" ,(merge-pathnames "examples/**/*.*" toplevel-dir))
            ("**;*.*" ,(merge-pathnames "**/*.*" contents-dir)))))
  #+windows-target (open-shared-library "opengl32.dll")
  ;(xlui::restore-font-manager)
  )



(defclass xmlisp-application (gui::cocoa-application)
  ())

(defmethod ccl::application-init-file ((app xmlisp-application))
  '("home:xmlisp-init" "home:\\.xmlisp-init"))


(defun FINISH-XMLISP ()
  (format t "~%- copy image resources")
  (ccl::recursive-copy-directory
   (truename "lui:resources;images;")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/Resources/images/" (user-homedir-pathname)))
  (format t "~%- copy sounds resources")
  (ccl::recursive-copy-directory
   (truename "lui:resources;sounds;")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/Resources/sounds/" (user-homedir-pathname)))
  (format t "~%- copy texture resources")
  (ccl::recursive-copy-directory
   (truename "lui:resources;textures;")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/Resources/textures/" (user-homedir-pathname)))
  (format t "~%- copy font resources")
  (ccl::recursive-copy-directory
   (truename "lui:resources;fonts;")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/Resources/fonts/" (user-homedir-pathname)))
  (format t "~%- copy cursor resources")
  (ccl::recursive-copy-directory
   (truename "lui:resources;cursors;")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/Resources/cursors/" (user-homedir-pathname)))
  (format t "~%- copy window resources")
  (ccl::recursive-copy-directory
   (truename "lui:resources;windows;")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/Resources/windows/" (user-homedir-pathname)))
  (format t "~%- copy button resources")
  (ccl::recursive-copy-directory
   (truename "lui:resources;buttons;")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/Resources/buttons/" (user-homedir-pathname)))
  (format t "~%- copy template resources")
  (ccl::recursive-copy-directory
   (truename "lui:resources;templates;")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/Resources/templates/" (user-homedir-pathname)))
  (format t "~%- patch info plist resources")
  (copy-file 
   (truename "lui:resources;English.lproj;InfoPlist.strings")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/Resources/English.lproj/InfoPlist.strings" (user-homedir-pathname))
   :if-exists :supersede))


(defun BUILD-XMLISP ()
  (setq *Package* (find-package :xlui))
  (require :build-application)
  ;; load a different init file
  ;; create LUI host pointing to application bundle
  (pushnew 'restore-xmlisp *restore-lisp-functions*)

  (format t "~%- create directories and files")
  (multiple-value-bind (Path Exists)
                       (create-directory (format nil "~ADesktop/XMLisp/" (user-homedir-pathname)))
    (declare (ignore Path))
   (unless Exists
      (error "XMLisp folder on desktop already exists")))

  (format t "~%- copy examples")
  (ccl::recursive-copy-directory
   (truename "lui:sources;XLUI;examples;")
   (format nil "~ADesktop/XMLisp/examples/" (user-homedir-pathname)))

  (finish-xmlisp)

  (ccl::build-application 
   :name "XMLisp"
   :directory (format nil "~ADesktop/XMLisp/" (user-homedir-pathname))
   :application-class 'xmlisp-application
   :nibfiles '("lui:resources;English.lproj;MainMenu.nib")))



#| 

(build-xmlisp)

|#

