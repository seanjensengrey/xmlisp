;;-*- Mode: Lisp; Package: XLUI -*-
;*********************************************************************
;*                                                                   *
;*                 X M L    L A Y O U T                              *
;*                                                                   *
;*********************************************************************
   ;* Author    : Alexander Repenning (alexander@agentsheets.com)    *
   ;*             http://www.agentsheets.com                         *
   ;* Copyright : (c) 1996-2008, AgentSheets Inc.                    *
   ;* Filename  : XML-layout.lisp                                    *
   ;* Updated   : 12/31/08                                           *
   ;* Version   :                                                    *
   ;*    1.0    : 07/20/06                                           *
   ;*    1.0.1  : 01/19/07 print-slots align, valign, padding        *
   ;*    1.0.2  : 06/13/08 valign="spread" works for rows            *
   ;*    2.0    : 11/14/08 XUL inspired, CCL                         *
   ;* HW/SW     : G4, OS X 10.5.5, Clozure CL 1.2                    *
   ;* Abstract  : base classes for XML layout                        *
   ;* Portable  : white                                              *
   ;*                                                                *
   ;******************************************************************

(in-package :XLUI)

;***************************************
;* XML Layout Interface                *
;***************************************

(defclass XML-LAYOUT-INTERFACE (xml-serializer)
  ((name :accessor name :initform "" :type string :initarg :name :documentation "a name used to refer to the object")
   (flex :accessor flex :initform 0 :type integer :initarg :flex :documentation "flexibility: if 0 then unflexible, larger value -> larger flexibility")
   (vflex :accessor vflex :initform 0 :type integer :initarg :vflex :documentation "vertical flexibility: if 0 then unflexible, larger value -> larger flexibility"))
  (:documentation "Interface to create XML based layouts"))
  

;; Specfication

(defgeneric INITIALIZE-LAYOUT (Xml-Layout-Interface)
  (:documentation "Initialize layout including its content. Called when all the layouts exist"))


;; Implementation


(defmethod INITIALIZE-LAYOUT ((Self xml-layout-interface))
  (do-subviews (Subview Self)
    (initialize-layout Subview)))


(defmethod PRINT-SLOTS ((Self xml-layout-interface))
  '(name x y width height text flex subviews))


(defmethod ADD-SUBOBJECT ((View xml-layout-interface) (Subview xml-layout-interface))
  (add-subviews View Subview))


;***************************************
;* Canvas                              *
;***************************************

(defclass CANVAS (control xml-layout-interface)
  ()
  (:default-initargs
      :width 500
    :height 100)
  (:documentation "Canvas with no layout: put controls any place"))


(defmethod initialize-event-handling ((Self canvas))
  ;; no event handling for rows
  )

;***************************************
;* Layout-Container                    *
;***************************************

(defclass LAYOUT-CONTAINER (xml-layout-interface)
  ((minimize :accessor minimize :initform :dont :initarg :minimize :type keyword :documentation ":dont :vertical, :horizontal, :box")
   (padding :accessor padding :initform 0 :initarg :padding :documentation "margins between views")
   (align :accessor align :initform :left :initarg :align :type keyword :documentation ":left, :center, :right, :distribute, :stretch")
   (valign :accessor valign :initform :top :initarg :valign :type keyword :documentation ":top, :middle, :bottom, :distribute, :stretch"))
  (:documentation "contains spatially organized subviews"))
   

(defmethod PRINT-SLOTS ((Self layout-container))
  '(subviews x y width height flex vflex align valign padding))


(defmethod MAX-SUBVIEW-HEIGHT ((Self layout-container))
  (let ((Height 0))
    (do-subviews (Subview Self)
      (setq Height (max Height (height Subview))))
    Height))


(defmethod MAX-SUBVIEW-WIDTH ((Self layout-container))
  (let ((Width 0))
    (do-subviews (Subview Self)
      (setq Width (max Width (width Subview))))
    Width))


(defmethod TOTAL-SUBVIEWS-HEIGHT ((Self layout-container) &key (Include-Padding t))
  ;; total height if all the subviews are layed out vertically with padding
  (let ((Height 0))
    (do-subviews (Subview Self)
      (incf Height (height Subview)))
    (incf Height (* (1- (length (subviews Self))) (if Include-Padding (padding Self) 0)))
    Height))


(defmethod TOTAL-SUBVIEWS-WIDTH ((Self layout-container) &key (Include-Padding t))
  ;; total width if all the subviews are layed out horiontally with padding
  (let ((Width 0))
    (do-subviews (Subview Self)
      (incf Width (width Subview)))
    (incf Width (* (1- (length (subviews Self))) (if Include-Padding (padding Self) 0)))
    Width))

;***************************************
;* Row                                 *
;***************************************

(defclass ROW (image-control layout-container)
  ()
  (:default-initargs
      :width 500
    :height 100)
  (:documentation "Layout content horizontally"))


(defmethod INITIALIZE-INSTANCE :after ((Self row) &rest Args)
  ;; width will depend on container but we can determine height
  (declare (ignore Args))
  (case (minimize Self)
    ((:vertical :box) (setf (height Self) (max-subview-height Self)))))


(defmethod PRINT-SLOTS ((Self row))
  '(x y width height align valign subviews))


(defmethod initialize-event-handling ((Self row))
  ;; no event handling for rows
  )


(defmethod SET-SIZE ((Self row) W H)
  ;; adjust own size and re-layout content
  ;; (format t "~%row ~A: resize to w=~A, h=~A" (view-nick-name Self) W H)
  (unless (subviews Self) 
    (setf (height Self) H)
    (setf (width Self) W)
    (return-from set-size))
  (let* ((Height (case (minimize Self) 
                   ((:vertical :box) (max-subview-height Self))
                   ((:dont t) H)))
         (Constant-Width 0)
         (Flex-Sum 0))
    (setq H Height)
    
    ;; when stretching compute sum of flex
    (case (align Self)
      (:stretch 
       (do-subviews (Subview Self)
         (if (> (flex Subview) 0)
           (incf Flex-Sum (flex Subview))
           (incf Constant-Width (width Subview))))))

    ;; size subviews:
    ;; some subviews may minimize
    (do-subviews (Subview Self)
      (set-size 
       Subview
       (case (align Self)
         (:stretch 
          (if (> (flex Subview) 0) 
            (ceiling (* (flex Subview)  ;; proportional to flex
                         (/ (- w 
                               (* (1+ (length (subviews Self))) (padding Self))
                               Constant-Width)
                            Flex-Sum)))
            (width Subview))) 
         (t (width Subview)))
       (case (valign Self)
         (:stretch H)  ;; stretch one
         (t (height Subview)))))

    ;; position subviews
    (let ((Distance (case (align Self)
                      (:distribute (if (> (length (subviews Self)) 1)
                                    (truncate (- w (total-subviews-width Self :include-padding nil))
                                              (1- (length (subviews Self))))
                                    (padding Self)))
                      (t (padding Self))))
          (X (ecase (align Self)
               (:left 0)
               ((:middle :center) (truncate (- w (total-subviews-width Self)) 2))
               (:right (- w (total-subviews-width Self) (padding Self)))
               (:distribute 0)
               (:stretch (padding Self)))))
      (do-subviews (Subview Self)
        (set-position 
         Subview
         x
         (ecase (valign Self)
           (:top 0)
           (:middle (truncate (- Height (height Subview)) 2))
           (:bottom (- Height (height Subview) (padding Self)))
           (:stretch 0)))
        (incf x (+ (width Subview) Distance))))
    
    ;; Self
    (setf (width Self) 
          (case (minimize Self)
            ((:horizontal :box)
             (total-subviews-width Self))
            (t W)))
    (setf (height Self) H)))


(defmethod LAYOUT ((Self row))
  (set-size Self (width Self) (height Self)))


;***************************************
;* Column                              *
;***************************************

(defclass COLUMN (image-control layout-container)
  ()
  (:default-initargs
      :width 100
    :height 500)
  (:documentation "Layout content vertically"))


(defmethod INITIALIZE-INSTANCE :after ((Self column) &rest Args)
  ;; height will depend on container but we can determine width
  (declare (ignore Args))
  (case (minimize Self)
    ((:horizontal :box) (setf (width Self) (max-subview-width Self)))))


(defmethod PRINT-SLOTS ((Self column))
  '(x y width height align valign subviews))


(defmethod initialize-event-handling ((Self column))
  ;; no event handling for column
  )


(defmethod SET-SIZE ((Self column) W H)
  ;; adjust own size and re-layout content
  (unless (subviews Self) 
    (setf (height Self) H)
    (setf (width Self) W)
    (return-from set-size))
  (let* ((Width (case (minimize Self) 
                   ((:horizontal :box) (max-subview-width Self))
                   ((:dont t) W)))
         (Constant-Height 0)
         (Flex-Sum 0))
    (setq W Width)

    ;; when stretching compute sum of flex
    (case (valign Self)
      (:stretch 
       (do-subviews (Subview Self)
         (if (> (vflex Subview) 0)
           (incf Flex-Sum (vflex Subview))
           (incf Constant-Height (height Subview))))))

    ;; size subviews:
    ;; some subviews may minimize
    (do-subviews (Subview Self)
      (set-size 
       Subview
       (case (align Self)
          (:stretch w)  ;; stretch one
          (t (width Subview))) 
        (case (valign Self)
          (:stretch 
           (if (> (vflex Subview) 0) 
            (ceiling (* (vflex Subview)  ;; proportional to vertical flex
                         (/ (- h 
                               (* (1+ (length (subviews Self))) (padding Self))
                               Constant-Height)
                            Flex-Sum)))
             (height Subview)))
          (t (height Subview)))))

    ;; position subviews
    (let ((Distance (case (valign Self)
                      (:distribute (if (> (length (subviews Self)) 1)
                                    (truncate (- h (total-subviews-height Self :include-padding nil))
                                              (1- (length (subviews Self))))
                                    (padding Self)))
                      (t (padding Self))))
          (Y (ecase (valign Self)
               (:top 0)
               ((:middle :center) (truncate (- h (total-subviews-height Self)) 2))
               (:bottom (- h (total-subviews-height Self) (padding Self)))
               (:distribute 0)
               (:stretch (padding Self)))))
      (do-subviews (Subview Self)
        ;; position
        (set-position 
         Subview
         (ecase (align Self)
           (:left 0)
           ((:middle :center) (truncate (- Width (width Subview)) 2))
           (:right (- Width (width Subview) (padding Self)))
           (:stretch 0))
         y)
        (incf y (+ (height Subview) Distance))))

    ;; Self
    (setf (width Self) w)
    (setf (height Self)
           (case (minimize Self)
             ((:vertical :box)
              (total-subviews-height Self))
             (t h)))))


(defmethod LAYOUT ((Self column))
  (set-size Self (width Self) (height Self)))




#| Examples:

(align <row align="left" minimize="vertical">
      <bevel-button text="a"/>
      <bevel-button text="b"/>
      <bevel-button text="c"/>
    </row>)
|#
