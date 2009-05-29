;; Start XMlisp
;; 04/24/09 Alexander Repenning
;; 5/20/09 + speech
;; for CCL 1.3

(in-package :cl-user)

;***************** Settings

;; edit to point to root folder containing /sources  /resources  etc.
(setf (logical-pathname-translations "lui")
      '(("**;*.*" "home:working copies;XMLisp svn;trunk;XMLisp;**;")))


(setq gui::*paren-highlight-background-color*
      (#/retain (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.9 0.8 0.8 1.0)))

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
   "COMMAND-KEY-P" "SHIFT-KEY-P" "ALT-KEY-P" "CONTROL-KEY-P"
   "WINDOW" "VIEW" "X" "Y" "WIDTH" "HEIGHT" "SHOW" "SHOW-AND-RUN-MODAL" "STOP-MODAL" "CANCEL-MODAL" "HIDE" 
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
   "CAMERA" "AIM-CAMERA" "WITH-GLCONTEXT" "RENDER-FOR-SELECTION-MODE"
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


(defun BUILD-XMLISP ()
  (require :build-application)
  ;; load a different init file
  (defmethod ccl::application-init-file ((app ccl::lisp-development-system))
    '("home:xmlisp-init" "home:\\.xmlisp-init"))
  ;; create LUI host pointing to application bundle
  (defmethod ccl::toplevel-function  ((a ccl::application) init-file)
    (declare (ignore init-file))
    (setf (logical-pathname-translations "lui")
      `(("**;*.*" ,(merge-pathnames "**/*.*" (ccl-contents-directory))))))
  (ccl::build-application 
   :name "XMLisp"
   :directory (format nil "~ADesktop/" (user-homedir-pathname))))


