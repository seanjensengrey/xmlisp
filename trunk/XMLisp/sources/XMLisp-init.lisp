;; Start XMlisp
;; 04/24/09 Alexander Repenning
;; 5/20/09 + speech
;; for CCL 1.3 
;; 5/28/09
;; 07/15/09 + font support


(in-package :cl-user)

;***************** Settings

;; edit to point to root folder containing /sources  /resources  etc.
(setf (logical-pathname-translations "lui")
      '(("**;*.*" "home:working copies;XMLisp svn;trunk;XMLisp;**;")))


(setq gui::*paren-highlight-background-color*
      (#/retain (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.9 0.8 0.8 1.0)))


(setq GUI::*Editor-keep-backup-files* nil)

;; Load

;; IDE

(load "lui:sources;IDE;specific;Mac CCL;anticipat-symbol-complete")
(load "lui:sources;IDE;specific;Mac CCL;ns timer")
(load "lui:sources;IDE;specific;Mac CCL;hemlock extensions")


;; Open

;;(ed "lui:sources;XLUI;examples;2D;test case examples.lisp")
;; (ed "lui:sources;XLUI;examples;AgentCubes Design.lisp")

;;(ed "lui:sources;Lisp User Interface;LUI.lisp")
;;(ed "lui:sources;Lisp User Interface;specific;Mac CCL;LUI Cocoa.lisp")


;;****** OpenGL

(defpackage :OPENGL
  (:use :common-lisp))

(load "lui:sources;OpenGL;specific;Mac CCL;OpenGL-interface")

;;****** LUI

(defpackage :LUI
  (:use :common-lisp :ccl :opengl)
  (:export 
   "MOUSE-EVENT" "TRACK-MOUSE" "VIEW-MOUSE-MOVED-EVENT-HANDLER" "VIEW-LEFT-MOUSE-DOWN-EVENT-HANDLER"
   "VIEW-LEFT-MOUSE-UP-EVENT-HANDLER"
   "COMMAND-KEY-P" "SHIFT-KEY-P" "ALT-KEY-P" "CONTROL-KEY-P"
   "WINDOW" "VIEW" "X" "Y" "WIDTH" "HEIGHT" "SHOW" "SHOW-AND-RUN-MODAL" "STOP-MODAL" "CANCEL-MODAL" "HIDE" 
   "SWITCH-TO-FULL-SCREEN-MODE" "SWITCH-TO-WINDOW-MODE"
   "SCROLL-VIEW" "HAS-HORIZONTAL-SCROLLER" "HAS-VERTICAL-SCROLLER"
   "SET-SIZE" "SET-POSITION" "DISPLAY" "DRAW" "INIT" "CLEAR-BACKGROUND" "RECURSIVE-MAP-SUBVIEWS"
   "RECTANGLE-VIEW" "SET-COLOR"
   "ZOOMABLE" "MINIMIZABLE" "RESIZABLE" "CLOSEABLE" "TITLE"
   "SCREEN-WIDTH" "SCREEN-HEIGHT" "CONTROL" "VALUE" "INITIALIZE-EVENT-HANDLING" "SIZE-CHANGED-EVENT-HANDLER"
   "ACTION" "SUBVIEWS" "DO-SUBVIEWS" "MAP-SUBVIEWS" "ADD-SUBVIEW" "ADD-SUBVIEWS" "SET-FRAME"
   "BUTTON-CONTROL" "DEFAULT-BUTTON"
   "BEVEL-BUTTON-CONTROL" "SLIDER-CONTROL" "TICK-MARKS" "MIN-VALUE" "MAX-VALUE"
   "LABEL-CONTROL" "TEXT" "ALIGN"
   "EDITABLE-TEXT-CONTROL"
   "VALUE"
   "IMAGE-CONTROL" "SRC" "FILE"
   "WEB-BROWSER-CONTROL" "URL"
   "OPENGL-VIEW" "FRAME-RATE" "ANIMATE" "START-ANIMATION" "STOP-ANIMATION" "FULL-SCENE-ANTI-ALIASING"
    "USE-TEXTURE"
    "CREATE-TEXTURE-FROM-FILE"
   "CAMERA" "AIM-CAMERA" "WITH-GLCONTEXT" "RENDER-FOR-SELECTION-MODE"
   "SHARED-OPENGL-VIEW"
   "NATIVE-PATH"
   ;; Dialogs
   "STANDARD-ALERT-DIALOG"
   ;; Multimedia
   "PLAY-SOUND"
   "SYNTHESIZER" "SPEAK" "WILL-SPEAK-WORD" "WILL-SPEAK-PHONEME" "DID-FINISH-SPEAKING" "AVAILABLE-VOICES"
   ))


(defun LUI::NATIVE-PATH (Directory-Name File-Name) "
  in: Directory-Name logical-pathname-string, e.g., ''lui:resources;textures;''
      File-Name string.
  out: Native-Path-String
  Create a native, OS specific, path from a platform independend URL style path"
  (format nil "~A~A" (truename Directory-Name) File-Name))




;; frameworks

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ccl:use-interface-dir :GL)
  (open-shared-library "/System/Library/Frameworks/OpenGL.framework/OpenGL"))


;; files

(load "lui:sources;Lisp User Interface;specific;Mac CCL;memory")
(load "lui:sources;Lisp User Interface;LUI")
(load "lui:sources;Lisp User Interface;OpenGL-view")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;LUI Cocoa")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;image-import")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;OpenGL-view Cocoa")
(load "lui:sources;Lisp User Interface;Camera")
(load "lui:sources;Lisp User Interface;specific;Mac CCL;speech")

;****** XMLisp

(defpackage :XML
  (:export "XML-SERIALIZER"))

(load "lui:sources;XMLisp;XMLisp")


;****** XLUI

(defpackage :XLUI
  (:use :common-lisp :XML :LUI :opengl))

(setq xml::*xmlisp-packages* (list (find-package :xlui) (find-package :xml)))


(load "lui:sources;XLUI;xml-layout")
(load "lui:sources;XLUI;controls")      ;; LOTS of undefined functions still!
(load "lui:sources;XLUI;application-window")
(load "lui:sources;XLUI;dialog-window")
(load "lui:sources;XLUI;Font-Manager")
(load "lui:sources;XLUI;String-Shape")
(load "lui:sources;XLUI;agent-3D")


;******** Multimedia

(defpackage :sound
  (:use :common-lisp :ccl)
  (:export "PLAY-SOUND"))


;;*************** Build functions

(defun ccl::CCL-CONTENTS-DIRECTORY ()
  (let* ((heap-image-path (ccl::%realpath (ccl::heap-image-name))))
    (make-pathname :directory (butlast (pathname-directory heap-image-path))
                   :device (pathname-device heap-image-path))))


(defun RESTORE-XMLISP ()
  (setf (logical-pathname-translations "lui")
      `(("**;*.*" ,(merge-pathnames "**/*.*" (ccl::ccl-contents-directory))))))


(defun BUILD-XMLISP ()
  (setq *Package* (find-package :xlui))
  (require :build-application)
  ;; load a different init file
  (defmethod ccl::application-init-file ((app ccl::lisp-development-system))
    '("home:xmlisp-init" "home:\\.xmlisp-init"))
  ;; create LUI host pointing to application bundle
  (pushnew 'restore-xmlisp *restore-lisp-functions*)

  (format t "~%- create directories and files")
  (multiple-value-bind (Path Exists)
                       (create-directory (format nil "~ADesktop/XMLisp/" (user-homedir-pathname)))
    (unless Exists
      (error "XMLisp folder on desktop already exists")))

  (format t "~%- copy examples")
  (ccl::recursive-copy-directory
   (truename "lui:sources;XLUI;examples;")
   (format nil "~ADesktop/XMLisp/examples/" (user-homedir-pathname)))

  (ccl::build-application 
   :name "XMLisp"
   :directory (format nil "~ADesktop/XMLisp/" (user-homedir-pathname))))


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
  (format t "~%- copy window resources")
  (ccl::recursive-copy-directory
   (truename "lui:resources;windows;")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/Resources/windows/" (user-homedir-pathname)))
  (format t "~%- patch menu resources")
  (ccl::recursive-copy-directory 
   (truename "lui:resources;English.lproj;MainMenu.nib;")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/Resources/English.lproj/MainMenu.nib/" (user-homedir-pathname))
   :if-exists :overwrite)
  (format t "~%- patch info plist resources")
  (copy-file 
   (truename "lui:resources;English.lproj;InfoPlist.strings")
   (format nil "~ADesktop/XMLisp/XMLisp.app/Contents/Resources/English.lproj/InfoPlist.strings" (user-homedir-pathname))
   :if-exists :overwrite))

#| 


(build-xmlisp)

(finish-xmlisp)

|#
