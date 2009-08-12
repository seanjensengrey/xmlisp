;;-*- Mode: Lisp; Package: XLUI -*-
;*********************************************************************
;*                                                                   *
;*           C U R S O R   M A N A G E R                             *
;*                                                                   *
;*********************************************************************
;* Author       : Alexander Repenning, alexander@agentsheets.com     *
;*                http://www.agentsheets.com                         *
;* Copyright    : (c) 1996-2009, AgentSheets Inc.                    *
;* Filename     : Cursor-Manager.lisp                                * 
;* Last Update  : 08/11/09                                           *
;* Version      :                                                    *
;*    1.0       : 08/11/09                                           *
;* Systems      : G4, CCL 1.3, OS X 10.5.8                           *
;* Abstract     : access cursor stored in resources/cursors folder   *
;*   <cursor image="CopyArrowCursor.png" hot-x="8" hot-y="8"/>       *
;*                                                                   *
;*********************************************************************


(in-package :xlui)


(defclass CURSOR (xml-serializer)
  ((image :accessor image :documentation "RGBA image file. Typically png file")
   (hot-x :accessor hot-x :initform 0 :type integer :documentation "hot spot x")
   (hot-y :accessor hot-y :initform 0 :type integer :documentation "hot spot y")))


(defvar *Cursors* (make-hash-table :test #'equal) "all the cursors")


(defvar *Cursor-Resources-Path* "lui:resources;cursors" "location of cursor resources and images")


(defun FIND-CURSOR (Name) "
  in: Name 
  Find a cursor matching name in resources/cursors/ folder"
  (let ((Cursor (gethash Name *Cursors*)))
    (cond
     ;; cached
     (Cursor Cursor)
     ;; system cursors
     ((string-equal Name "arrowCursor") (#/arrowCursor ns:ns-cursor))
     ((string-equal Name "IBeamCursor") (#/IBeamCursor ns:ns-cursor))
     ((string-equal Name "crosshairCursor") (#/crosshairCursor ns:ns-cursor))
     ((string-equal Name "closedHandCursor") (#/closedHandCursor ns:ns-cursor))
     ((string-equal Name "openHandCursor") (#/openHandCursor ns:ns-cursor))
     ((string-equal Name "pointingHandCursor") (#/pointingHandCursor ns:ns-cursor))
     ((string-equal Name "resizeLeftCursor") (#/resizeLeftCursor ns:ns-cursor))
     ((string-equal Name "resizeRightCursor") (#/resizeRightCursor ns:ns-cursor))
     ((string-equal Name "resizeLeftRightCursor") (#/resizeLeftRightCursor ns:ns-cursor))
     ((string-equal Name "resizeUpCursor") (#/resizeUpCursor ns:ns-cursor))
     ((string-equal Name "resizeDownCursor") (#/resizeDownCursor ns:ns-cursor))
     ((string-equal Name "resizeUpDownCursor") (#/resizeUpDownCursor ns:ns-cursor))
     ((string-equal Name "disappearingItemCursor") (#/disappearingItemCursor ns:ns-cursor))
     ;; application cursors: load from resource folder + cache
     (t
      (let ((Cursor-Path (native-path *Cursor-Resources-Path* (format nil "~A.cursor" Name))))
        (when (probe-file Cursor-Path)
          (let ((Cursor-Manager (load-object Cursor-Path :package  (find-package :xlui))))
            (ns:with-ns-point (Hotspot (hot-x Cursor-Manager) (hot-y Cursor-Manager))
              (let ((Cursor (#/initWithImage:hotSpot: 
                             (#/alloc ns:ns-cursor)
                             (#/initByReferencingFile: 
                              (#/alloc ns:ns-image)
                              (native-string (native-path *Cursor-Resources-Path* (image Cursor-Manager))))
                             Hotspot)))
                (setf (gethash Name *Cursors*) Cursor)
                Cursor)))))))))


(defun SET-CURSOR (Name) "
  in: Name
  Set the cursor to new cursor."
  (let ((Cursor (find-cursor Name)))
    (when Cursor (#/set Cursor))))


#| Examples

(find-cursor "CopyArrow")

(find-cursor "ArrowCursor")

(find-cursor "NotHere")


|#