;;-*- mode: lisp; package: xlui -*-
;*********************************************************************
;*                                                                   *
;*                           M O R P H S                             *
;*                                                                   *
;*********************************************************************
   ;* Author      : Alexander Repenning (alexander@agentsheets.com)  *
   ;*               http://www.agentsheets.com                       *
   ;* Copyright   : (c) 1996-2008, Agentsheets Inc.                  *
   ;* Filename    : morph.lisp                                       *
   ;* Last Update : 07/29/07                                         *
   ;* Version     :                                                  *
   ;*    1.0      : 07/29/07                                         *
   ;* HW/SW       : PowerPC G4, MCL 5.2                              *
   ;* Abstract    : Morphs to be used in AWE simulators for          *
   ;*               interactively warping images                     *
   ;******************************************************************

(in-package :xlui)


;____________________________________
; morph                              |
;____________________________________

(defclass MORPH (agent-3d)
  ((image :accessor image :type string :documentation "filename")
   (file :accessor file :initform nil :documentation "file containing XML representation")
   (vertices :accessor vertices :initform (make-array 0 :adjustable t :fill-pointer 0) :documentation "vertex vector")
   (triangles :accessor triangles :initform nil :documentation "list of triangles"))
  (:documentation "A highly distortable image"))


(defmethod PRINT-SLOTS ((Self morph))
  '(image vertices triangles))


(defmethod DRAW ((self morph))
  (glEnable GL_TEXTURE_2D)
  (glColor4f 1.0 1.0 1.0 1.0)
  (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
  (use-texture Self (image self))
  (dolist (triangle (triangles self))
    (glBegin GL_TRIANGLES)
    (glNormal3f 0.0 0.0 1.0)
    (dolist (vertex-index (vertices triangle))
      (let ((vertex (aref (vertices self) vertex-index)))
        (glTexCoord2f (xt vertex) (yt vertex))
        (glVertex2f (x vertex) (y vertex))))
    (glend)))


;____________________________________
; vertex                             |
;____________________________________

(defclass VERTEX (xml-serializer)
  ((x  :type equation :initarg :x  :documentation "world coordinate x")
   (y  :type equation :initarg :y  :documentation "world coordinate y")
   (xt :type equation :initarg :xt :documentation "texture coordinate x")
   (yt :type equation :initarg :yt :documentation "texture coordinate y")
   (morph :accessor morph :initform nil :documentation "link to morph containng me"))
  (:default-initargs
      :x (make-instance 'equation :value 0.0)
      :y (make-instance 'equation :value 0.0)
    :xt (make-instance 'equation :value 0.0)
    :yt (make-instance 'equation :value 0.0))
  (:documentation "x, y, xt, yt 2D world + texture coordinate vertex"))


(defmethod PRINT-SLOTS ((Self vertex))
  '(x y xt yt))


(defmethod ADD-SUBOBJECT ((Morph morph) (Vertex vertex))
  (vector-push-extend vertex (vertices morph))
  (setf (morph vertex) morph))

;; accessors using equations

(defmethod GET-COMPUTED-SLOT-VALUE ((Self vertex) Name)
  (let ((equation (slot-value self Name)))
    (cond 
     ((not equation) nil)
     ((value equation))
     ((value-function equation)
      (funcall (value-function equation) (if (morph self) (view (morph self)) nil))))))


(defmethod X ((Self vertex))
  (get-computed-slot-value Self 'x))


(defmethod Y ((Self vertex))
  (get-computed-slot-value Self 'y))


(defmethod XT ((Self vertex))
  (get-computed-slot-value Self 'xt))


(defmethod YT ((Self vertex))
  (get-computed-slot-value Self 'yt))


;____________________________________
; triangle                           |
;____________________________________

(defclass TRIANGLE (xml-serializer)
  ((vertices :accessor vertices :initform nil :type list :documentation "e.g., 1 2 3")))


(defmethod SET-ATTRIBUTE-VALUE ((Self triangle) Name Value)
  (case name 
    (vertices (setf (slot-value self 'vertices) (read-from-string (concatenate 'string "(" value ")"))))
    (t (call-next-method))))


(defmethod PRINT-SLOT-NAME-VALUE-TYPE-AS-ATTRIBUTE ((Self triangle) Name Value Type Stream)
  (declare (ignore Type))
  (case name 
    (vertices
     (format stream " vertices=\"")
     (when (first value)
       (format stream "~A" (first value)))
     (when (rest value)
       (dolist (element (rest value))
         (format stream " ~A" element)))
     (format stream "\""))
    (t (call-next-method))))