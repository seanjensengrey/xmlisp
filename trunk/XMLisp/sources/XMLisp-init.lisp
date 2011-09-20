;; Start XMlisp
;; 04/24/09 Alexander Repenning
;; 5/20/09 + speech
;; for CCL 1.3 
;; 5/28/09
;; 07/15/09 + font support 
;; 02/17/10 + inflatable icons and inflatable icon editor window

(in-package :cl-user)

;;___________________________________________
;;  Hacks                                    |
;;___________________________________________

;; temporary fix for CCL's issue with turning warnings into errors
;; should remove with new release 

(setq ccl::*objc-error-return-condition* 'error)

;; this should make output show up earlier on the console
(ccl:add-auto-flush-stream ccl::*stdout*)


(objc:defmethod (#/invokeLispFunction: :void) ((self ns:ns-application) id)
  (gui::invoke-lisp-function self id))

;;___________________________________________
;;  Pahthnames                               |
;;___________________________________________

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
   #p"w:/**/*.*")))


#+cocotron
(setf (logical-pathname-translations "lui")
      '(("**;*.*" "mac-home:Xmlisp svn;trunk;XMLisp;**;")))



;;___________________________________________
;;  Editor Settings                          |
;;___________________________________________

#-cocotron
(setq gui::*paren-highlight-background-color*
      (#/retain (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.9 0.8 0.8 1.0)))


#-cocotron
(setq GUI::*Editor-keep-backup-files* nil)


(setq ccl::*verbose-eval-selection* t)

;;___________________________________________
;;  Load IDE extensions                       |
;;___________________________________________
  
#-cocotron (load "lui:sources;IDE;specific;Mac CCL;anticipat-symbol-complete")
(load "lui:sources;IDE;specific;Mac CCL;ns timer")
#-cocotron (load "lui:sources;IDE;specific;Mac CCL;GLDocs")
#-cocotron (load "lui:sources;IDE;specific;Mac CCL;hemlock extensions")


;;___________________________________________
;;  Infix                                    |
;;___________________________________________

(defpackage :INFIX
  (:use :common-lisp))


;;___________________________________________
;;  OpenGL                                   |
;;___________________________________________

(defpackage :OPENGL
  (:use :common-lisp))


(load "lui:sources;OpenGL;specific;Mac CCL;OpenGL-interface")

;; frameworks

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ccl:use-interface-dir :GL)
  #-windows-target (open-shared-library "/System/Library/Frameworks/OpenGL.framework/OpenGL")
  #+windows-target (open-shared-library "opengl32.dll"))

;;___________________________________________
;;  XMLisp                                   |
;;___________________________________________

(defpackage :XML
  (:use :common-lisp)
  (:export "XML-SERIALIZER" "PARSE-FILE-NAME"))


(load "lui:sources;XMLisp;XMLisp")

8921063
;; temporarily set xmlisp package to only be :xml to avoid confusion with xml-like structures in Cocoa
(setf xml::*XMLisp-Packages* (list (find-package :xml)))

;;___________________________________________
;;  LUI (Lisp User Interface)                |
;;___________________________________________

(defpackage :LUI
  (:use :common-lisp :ccl :opengl)
  (:export 
   "SIZEOF" "LONG"
   "MOUSE-EVENT" "TRACK-MOUSE" "GESTURE-EVENT" "GET-MODIFIER-FLAGS" "GET-CHARACTERS"
   ;; Mouse events
   "VIEW-MOUSE-MOVED-EVENT-HANDLER" 
   "VIEW-LEFT-MOUSE-DOWN-EVENT-HANDLER" "VIEW-RIGHT-MOUSE-DOWN-EVENT-HANDLER" 
   "VIEW-LEFT-MOUSE-DRAGGED-EVENT-HANDLER"
   "VIEW-LEFT-MOUSE-UP-EVENT-HANDLER" 
   "VIEW-MOUSE-ENTERED-EVENT-HANDLER" "VIEW-MOUSE-EXITED-EVENT-HANDLER" 
   "VIEW-MOUSE-SCROLL-WHEEL-EVENT-HANDLER"
   ;; key events
   "KEY-EVENT-HANDLER" 
   ;; Gesture Events
   "GESTURE-MAGNIFY-EVENT-HANDLER" "GESTURE-ROTATE-EVENT-HANDLER"
   "*CURRENT-EVENT*" "VIEW-CURSOR" "CURRENT-CURSOR"
   "KEY-EVENT-HANDLER" "KEY-EVENT" "KEY-CODE"
   "COMMAND-KEY-P" "SHIFT-KEY-P" "ALT-KEY-P" "CONTROL-KEY-P" "DOUBLE-CLICK-P"
   "WINDOW" "VIEW" "X" "Y" "WIDTH" "HEIGHT" "SHOW" "CENTER" "DO-SHOW-IMMEDIATELY" "SHOW-AND-RUN-MODAL" "STOP-MODAL" "CANCEL-MODAL" "BEFORE-GOING-MODAL" "HIDE"  "HAS-BECOME-MAIN-WINDOW" "WINDOW-WILL-CLOSE" "WINDOW-OF-VIEW-WILL-CLOSE" "WINDOW-DID-FINISH-RESIZE" "WINDOW-SHOULD-CLOSE" "MAKE-KEY-WINDOW" "ADD-TRACKING-RECT" "REMOVE-TRACKING-RECT""VIEW-DID-MOVE-TO-WINDOW" "VIEW-DID-END-RESIZE" "VISIBLE-P" "USE-CUSTOM-WINDOW-CONTROLLER" "SET-DOCUMENT-EDITTED" "SHOW-MAIN-MENU-ON-WINDOWS" "SET-SIZE-AND-POSITION-ENSURING-WINDOW-WILL-FIT-ON-SCREEN"
   "SWITCH-TO-FULL-SCREEN-MODE" "SWITCH-TO-WINDOW-MODE" "FULL-SCREEN-P" "WINDOW-CLOSE" "MIN-WIDTH" "MIN-HEIGHT"
   "FIND-WINDOW-AT-SCREEN-POSITION" "FIND-VIEW-CONTAINING-POINT" "FIND-VIEW-AT-SCREEN-POSITION"
   "SCROLL-VIEW" "HAS-HORIZONTAL-SCROLLER" "HAS-VERTICAL-SCROLLER"
   "SCROLL-VIEW-ADJUSTING-CONTENTS"
   "SET-SIZE" "SET-POSITION" "DISPLAY"  "DRAW" "PREPARE-OPENGL" "CLEAR-BACKGROUND" "RECURSIVE-MAP-SUBVIEWS"
   "WINDOW-X" "WINDOW-Y"
   "RECTANGLE-VIEW" "SET-COLOR"
   "COLOR-PALETTE-VIEW"
   "PLOT-VIEW"
   "ZOOMABLE" "MINIMIZABLE" "RESIZABLE" "CLOSEABLE" "BORDERLESS" "TITLE" "WINDOW-NEEDS-SAVING-P"
   "SCREEN-WIDTH" "SCREEN-HEIGHT" "CONTROL" "VALUE" "INITIALIZE-EVENT-HANDLING" "SIZE-CHANGED-EVENT-HANDLER" "START-DISABLED"
   "ACTION" "SUBVIEWS" "DO-SUBVIEWS" "MAP-SUBVIEWS" "ADD-SUBVIEW" "SUPERVIEW" "ADD-SUBVIEWS" "SWAP-SUBVIEW" "SUBVIEWS-SWAPPED" "SET-FRAME" 
   "BUTTON-CONTROL" "DEFAULT-BUTTON" "TURN-ON" "TURN-OFF" "IS-ON-P"
   "INVOKE-ACTION"
   "BEVEL-BUTTON-CONTROL" "DISABLE" "SLIDER-CONTROL" "TICK-MARKS" "MIN-VALUE" "MAX-VALUE" "ENABLE" "DISABLE" "BEZEL-STYLE"
   "JOG-SLIDER-CONTROL" "START-JOG" "STOP-JOG" "STOP-VALUE" "ACTION-INTERVAL"
   "LABEL-CONTROL" "TEXT" "ALIGN" "SIZE"
   "EDITABLE-TEXT-CONTROL" "SECURE"
   "TYPE-INTERACTOR-CONTROL" "AGENT-NAME-TYPE-CONTROL" "NUMBER-TYPE-CONTROL" "FORMULA-TYPE-CONTROL" "VALIDATE-TEXT-CHANGE" "ATTRIBUTE-NAME-TYPE-CONTROL"  "ATTRIBUTE-PROPERTY-NAME-TYPE-CONTROL" "INTEGER-TYPE-CONTROL"
   "STATUS-BAR-CONTROL"
   "PROGRESS-INDICATOR-CONTROL" "START-ANIMATION" "STOP-ANIMATION" "ANIMATION-ABORTED" "ANIMATION-CYCLE-ABORTED" "WITH-ANIMATION-LOCKED" "WITH-ANIMATION-UNLOCKED" "GRAB-ANIMATION-LOCK" "RELEASE-ANIMATION-LOCK"
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
   "POPUP-BUTTON-CONTROL"  "POPUP-ACTION" "SET-SELECTED-ITEM-WITH-TITLE" "REMOVE-ITEM"
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
   "COLOR-WELL-BUTTON-CONTROL"
   "WEB-BROWSER-CONTROL" "URL"
   "OPENGL-VIEW" "FRAME-RATE" "DISPLAY-FRAME-RATE" "ANIMATE" "DELTA-TIME" "TEXTURES" "START-ANIMATION" "STOP-ANIMATION" "IS-ANIMATED" "FULL-SCENE-ANTI-ALIASING" "ANIMATE-OPENGL-VIEW-ONCE" 
   ;; Shader Support
   "SET-SHADER-SOURCE" "GET-SHADER-INFO-LOG"
   "INITIALIZE-CAMERA"
   "OBJECT-COORDINATE-VIEW-WIDTH" "OBJECT-COORDINATE-VIEW-HEIGHT"
    "USE-TEXTURE"
    "CREATE-TEXTURE-FROM-FILE" "*DEFAULT-OPENGL-TEXTURE-MAGNIFICATION-FILTER*"
    "SEPERATOR-CONTROL"
    "native-view"
    "GET-CURSOR" "SET-CURSOR"
   "CAMERA" "EYE-X" "EYE-Y" "EYE-Z" "CENTER-X" "CENTER-Y" "CENTER-Z"  "UP-X" "UP-Y" "UP-Z"
   "FOVY" "ASPECT" "NEAR" "FAR" "AZIMUTH" "ZENITH"
   "AIM-CAMERA" "WITH-GLCONTEXT" "WITH-GLCONTEXT-NO-FLUSH" "RENDER-FOR-SELECTION-MODE" "SAME-SETTINGS"
   "SHARED-OPENGL-VIEW"
   "NATIVE-PATH"
   ;; Dialogs
   "STANDARD-ALERT-DIALOG" "CHOOSE-FILE-DIALOG"
   ;; colors
   "*SYSTEM-SELECTION-COLOR*"
   ;; Multimedia
   "PLAY-SOUND" "STOP-SOUND" "SOUND-FILES-IN-SOUND-FILE-DIRECTORY" "SHUT-UP-SOUNDS" "SET-VOLUME"
   "SYNTHESIZER" "SPEAK" "SPEAK-SYNCHRONOUSLY" "WILL-SPEAK-WORD" "WILL-SPEAK-PHONEME" "DID-FINISH-SPEAKING" "AVAILABLE-VOICES" "NATIVE-SYNTHESIZER" "SHOULD-START-SPEAKING"
   ;; native support
   "NATIVE-STRING"
   ;; badged image
   "BADGED-IMAGE-GROUP-LIST-MANAGER-VIEW" "ITEM-CATEGORY-LABEL" "ADD-GROUP" "EDIT-GROUP-ITEM" "ADD-GROUP-ITEM" "DELETE-GROUP" "DELETE-GROUP-ITEM" "SELECTED-GROUP" "SELECTED-GROUP-ITEM" "SELECT-GROUP" "SELECT-GROUP-ITEM" "GROUP-NAME" "GROUPS" "TAKE-FOCUS" "GET-GROUP-WITH-NAME" "ITEM-NAME" "GET-GROUP-ITEM-WITH-NAME" "SELECTED-GROUP-ITEM" "UPDATE-IMAGE" "GROUP-ITEMS" "GROUP-VIEW" "ITEM-DETECTION-VIEWS" "SIZE-CHANGED"
   "SELECTED-GROUP-CHANGED-ACTION" "GROUP-ITEMS" "GROUP-NAME-CHANGED" "ITEM-NAME-CHANGED" "GROUP-SELECTED" "GROUP-DESELECTED" "GET-MAIN-ITEM"
   "BROWSER-VIEW" "NODE-ITEM" "NODE-NAME" "NODES" "GET-VALUE-OF-SELECTED-CELL" "GET-SELECTED-COLUMN" "SET-TITLE-OF-COLUMN" "GET-VALUE-OF-SELECTED-CELL-AT-COLUMN" "SELECT-ROW-IN-COLUMN" "COLUMN-LIMIT"
   "TABLE-VIEW" "ADD-COLUMN" "ADD-ROW" "TEXT-CHANGED-ACTION-TARGET" "TEXT-CHANGED-ACTION" "SHOULD-END-EDITING-NOTIFICAITON" "RELOAD-DATA" "SET-COLOR-OF-CELL-AT-ROW-COLUMN" "GET-STRING-VALUE-OF-CELL-AT-ROW-COLUMN" "GET-COLUMN-OF-CELL-BEING-EDITED" "GET-ROW-OF-CELL-BEING-EDITED" "REMOVE-ROW" "GET-SELECTED-ROW"
   "INCREMENT-ITEM-COUNTER" "CLEAR-ITEM-COUNTERS"
   "PROJECT-MANAGER-REFERENCE" "AGENT-GALLERY-VIEW" "OPEN-ICON-WINDOWS"
   "CONVERT-IMAGE-FILE"
   "GET-TOOLTIP" "ENABLE-TOOLTIPS" "SET-ICON-OF-FILE-AT-PATH" "TOOLTIP" "ALWAYS-ENABLE-TOOLTIPS"
   "FULL-SCREEN" "ENTER-FULL-SCREEN-MODE" "EXIT-FULL-SCREEN-MODE"
   )
  (:import-from "XML"
                "FILE" "*XMLISP-PRINT-SYNOPTIC*"))


(defun LUI::NATIVE-PATH (Directory-Name File-Name) "
  in: Directory-Name logical-pathname-string, e.g., ''lui:resources;textures;''
      File-Name string.
  out: Native-Path-String
  Create a native, OS specific, path from a platform independend URL style path"
  (format nil "~A~A" (truename Directory-Name) File-Name))


(defun LUI::NATIVE-PATH (Directory-Name File-Name) "
  in: Directory-Name logical-pathname-string, e.g., ''lui:resources;textures;''
      File-Name string.
  out: Native-Path-String
  Create a native, OS specific, path from a platform independend URL style path"
  (format nil "~A~A" (truename Directory-Name) File-Name))
;;___________________________________________
;;  OS Version Management                     |
;;___________________________________________

(defun OS-VERSION-VALUES ()
 "Returns values of major and minor System version"
 (let* ((string (software-version))
        (1st-. (position #\. string)))
   (values (- (read-from-string string nil nil :end 1st-.) 4)
           (read-from-string string nil nil :start (1+ 1st-.)
                             :end (position #\. string :start (1+ 1st-.))))))


(defvar LUI::*OS-Name* #-:cocotron :osx #+:cocotron :windows "Keyword indicating operating system: :osx or :windows")


(defvar lui::*OS-VERSION-MAJOR* 10 "major OS version number")


(defvar lui::*OS-VERSION-MINOR* 0 "minor OS version number")


(defvar lui::*OS-VERSION-MAINTENANCE* 0 "maintenance OS version number")


(defun lui::Mac-OS-X-10.5-and-later ()
  (and (eq lui::*Os-name* :osx)
       (>= lui::*os-version-major* 10)
       (>= lui::*os-version-minor* 5)))


(defun lui::Mac-OS-X-10.6-and-later ()
  (and (eq lui::*Os-name* :osx)
       (>= lui::*os-version-major* 10)
       (>= lui::*os-version-minor* 6)))


#-:cocotron
(multiple-value-bind (Minor Maintenance)
                     (os-version-values)
  (setq lui::*os-version-minor* Minor)
  (setq lui::*os-version-maintenance* Maintenance))


(export '(lui::Mac-OS-X-10.5-and-later lui::Mac-OS-X-10.6-and-later LUI::*OS-Name* lui::*OS-VERSION-MAJOR* lui::*OS-VERSION-MINOR* lui::*OS-VERSION-MAINTENANCE*) :lui)


;; Helper function to avoid copying SVN folders in application, project, agent, shape, or world folders
(defun ccl::recursive-copy-directory-without-svn (source-path dest-path &key test (if-exists :error))
  (ccl::recursive-copy-directory source-path dest-path 
                                 :test #'(lambda (File)
                                           (and (or (null test) (funcall test File))
                                                (not (string= (first (last (pathname-directory File))) ".svn"))))
                                 :if-exists if-exists))


;;___________________________________________
;;  Load LUI                                 |
;;___________________________________________


(load "lui:sources;Lisp User Interface;in-main-thread")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;native-strings")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;standard-alert-dialog")
(load "lui:sources;Lisp User Interface;error-handling")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;memory")
(load "lui:sources;Lisp User Interface;LUI")
(load "lui:sources;Lisp User Interface;Camera")
(load "lui:sources;Lisp User Interface;OpenGL-view")
(load "lui:sources;Lisp User Interface;browser-view")
(load "lui:sources;Lisp User Interface;table-view")
(load "lui:sources;Lisp User Interface;plot-view")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;LUI Cocoa")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;image-import")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;OpenGL-view Cocoa")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;image-export")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;browser-view-cocoa")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;table-view-cocoa")
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


;;___________________________________________
;;  XLUI                                     |
;;___________________________________________

(defpackage :XLUI
  (:use :common-lisp :XML :LUI :opengl)
  ;; import MOP
  (:import-from "CCL"
                "SLOT-DEFINITION-NAME" "SLOT-DEFINITION-TYPE")
  (:import-from "XML" "FILE")
  (:import-from "CCL" "WITH-CSTR")
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

;;Image Tools
(load "lui:sources;XLUI;image-tools")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;get-color-from-user")

;; inflatable icons
(load "lui:sources;XLUI;image editor;selection-mask")
(load "lui:sources;XLUI;image editor;image-editor")
(load "lui:sources;XLUI;image editor;Inflatable-Icon")
(load "lui:sources;XLUI;image editor;Inflatable-Icon-Window")



;;___________________________________________
;;  Application Building                     |
;;___________________________________________

(defun ccl::CCL-CONTENTS-DIRECTORY ()
  (let* ((heap-image-path (ccl::%realpath (ccl::heap-image-name))))
    (make-pathname :directory (butlast (pathname-directory heap-image-path))
                   :device (pathname-device heap-image-path))))


(defun RESTORE-XMLISP ()
  (let* ((contents-dir (ccl::ccl-contents-directory))
         (toplevel-dir (make-pathname :directory (butlast (pathname-directory contents-dir) 2)
                                      :device (pathname-device contents-dir))))
    (setf (logical-pathname-translations "lui")
          `(("examples;**;*.*" ,(merge-pathnames "examples/**/*.*" toplevel-dir))
            ("**;*.*" ,(merge-pathnames "**/*.*" contents-dir)))))
  #+windows-target (open-shared-library "opengl32.dll")
  ;(xlui::restore-font-manager)
  )


(defclass XMLISP-APPLICATION (gui::cocoa-application)
  ())


(defmethod CCL::APPLICATION-INIT-FILE ((app xmlisp-application))
  '("home:xmlisp-init" "home:\\.xmlisp-init"))


(defun FINISH-XMLISP ()
  (format t "~%- copy image resources")
  (ccl::recursive-copy-directory-without-svn
   (truename "lui:resources;images;")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/Resources/images/" (user-homedir-pathname)))
  (format t "~%- copy sounds resources")
  (ccl::recursive-copy-directory-without-svn
   (truename "lui:resources;sounds;")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/Resources/sounds/" (user-homedir-pathname)))
  (format t "~%- copy texture resources")
  (ccl::recursive-copy-directory-without-svn
   (truename "lui:resources;textures;")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/Resources/textures/" (user-homedir-pathname)))
  (format t "~%- copy font resources")
  (ccl::recursive-copy-directory-without-svn
   (truename "lui:resources;fonts;")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/Resources/fonts/" (user-homedir-pathname)))
  (format t "~%- copy cursor resources")
  (ccl::recursive-copy-directory-without-svn
   (truename "lui:resources;cursors;")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/Resources/cursors/" (user-homedir-pathname)))
  (format t "~%- copy window resources")
  (ccl::recursive-copy-directory-without-svn
   (truename "lui:resources;windows;")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/Resources/windows/" (user-homedir-pathname)))
  (format t "~%- copy button resources")
  (ccl::recursive-copy-directory-without-svn
   (truename "lui:resources;buttons;")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/Resources/buttons/" (user-homedir-pathname)))
  (format t "~%- copy template resources")
  (ccl::recursive-copy-directory-without-svn
   (truename "lui:resources;templates;")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/Resources/templates/" (user-homedir-pathname)))
  (format t "~%- copy shaders")
  (ccl::recursive-copy-directory-without-svn
   (truename "lui:resources;shaders;")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/Resources/shaders/" (user-homedir-pathname)))
  #-cocotron 
  (format t "~%- copy the plist")
  #-cocotron 
  (copy-file
   (truename "lui:resources;plists;info.plist")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/info-plist" (user-homedir-pathname))
   :if-exists :supersede))


(defun BUILD-XMLISP ()
  (declare (ftype function ccl::build-application ccl::make-info-dict))
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
  (ccl::recursive-copy-directory-without-svn
   (truename "lui:sources;XLUI;examples;")
   (format nil "~ADesktop/XMLisp/examples/" (user-homedir-pathname)))
  (finish-xmlisp)
  (ccl::build-application 
   :name "XMLisp"
   :directory (format nil "~ADesktop/XMLisp/" (user-homedir-pathname))
   ;:application-class 'xmlisp-application
   ;#+cocotron :info-plist (ccl::make-info-dict)
   ;:nibfiles '("lui:resources;English.lproj;MainMenu.nib")
   ))

;;___________________________________________
;;      Also Load AgentCubes                 |
;;___________________________________________


;; If we find AgentCubes lets just load it unless user indicates no by holding down shift key
(unless (lui::shift-key-p)
  (when (probe-file "lui:sources;AgentCubes;AgentCubes-init.lisp")
    (load "lui:sources;AgentCubes;AgentCubes-init")))


#| 

(build-xmlisp)
(xlui::project-chooser)

|#

