;;-*- mode: lisp; package: xlui -*-
;*********************************************************************
;*                                                                   *
;*                   E Q U A T I O N                                 *
;*                                                                   *
;*********************************************************************
   ;* Author      : Alex Repenning (alex@agentsheets.com)            *
   ;*               http://www.agentsheets.com                       *
   ;* Copyright   : (c) 1996-2008, Agentsheets Inc.                  *
   ;* Filename    : equations.lisp                                   *
   ;* Last Update : 07/29/07                                         *
   ;* Version     :                                                  *
   ;*    1.0      : 07/29/07                                         *
   ;* HW/SW       : PowerPC G4, MCL 5.2                              *
   ;* Abstract    : Equations for use with variables                 *
   ;******************************************************************

(in-package :xlui)


;____________________________________
; equation type                      |
;____________________________________


(defclass EQUATION ()
  ((value :accessor value :initform nil :initarg :value :documentation "equation can be constant value")
   (value-function :accessor value-function :initform nil)
   (formula :accessor formula :initarg :formula :documentation "unedited user provided formula"))
  (:documentation "equations can be used as slot types"))


(defmethod XML-PRINTABLE-AS-ATTRIBUTE-VALUE-P ((Self equation)) t)


(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'equation)) Stream)
  ;; print only the formula of an equation
  (typecase Value
    (string (format stream "~A" (formula value)))
    (number (format stream "\"~A\"" Value))
    (t (format stream "\"~A\"" (formula value)))))


(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'equation)))
  (let ((equation (make-instance 'equation :formula value)))
    (let ((prefix (expand value)))
      (if (numberp prefix)
        (setf (value equation) prefix)
        (setf (value-function equation)
              (compile nil (append '(lambda (Self)) (list prefix))))))
    equation))
