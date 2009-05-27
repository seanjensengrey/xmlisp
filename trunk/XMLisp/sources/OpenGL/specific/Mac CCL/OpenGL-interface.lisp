;;;;-*- Mode: Lisp; Package: opengl -*-
;*********************************************************************
;*                                                                   *
;*        O p e n G L   I N T E R F A C E                            *
;*                                                                   *
;*********************************************************************
   ;* Author    : Alexander Repenning (alexander@agentsheets.com)    *
   ;*             http://www.agentsheets.com                         *
   ;* Copyright : (c) 1996-2009, AgentSheets Inc.                    *
   ;* Filename  : OpenGL-Interface.lisp                              *
   ;* Updated   :  05/15/09                                          *
   ;* Version   :                                                    *
   ;*   0.0     : 01/13/09                                           *
   ;*   0.1     : 03/17/09 gl functions, GL, GLU, GLUT constants     *
   ;*   0.2     : 05/15/09 preserve :api-names as symbol properties  *
   ;* SW/HW     : OS X 10.5.6, CCL 1.3, PowerPC G4,Intel-Mac         *
   ;* Abstract  : OS X function and constant wrappers                *
   ;*   readable (#_glBegin #$GL_TRIANGLES) (glBegin GL_TRIANGLES)   *
   ;*   symbols are interned -> symbol completable                   *
   ;* See also  : http://common-lisp.net/project/cl-opengl/          *
   ;*                                                                *
   ;******************************************************************

(in-package :opengl)

;********************************
;* Functions                    *
;********************************

(defun MAP-OPENGL-FUNCTIONS (Map-Function) "
  in:  Map-Function Name Entry-Name Arguments Result &optional Min-Args -> list.
  out: List-of-all-function-results."
  (let ((CDB (ccl::db-functions (ccl::use-interface-dir :gl))))
    (mapcar
     #'(lambda (Name) 
         (let ((Info (ccl::db-lookup-function CDB Name)))
           (if Info
             (funcall 
              Map-Function
              Name 
              (ccl::efd-entry-name Info)
              (ccl::efd-arg-specs Info)
              (ccl::efd-result-spec Info))
             (warn "Odd.  There's no foreign function info for ~s" Name))))
     (ccl::cdb-enumerate-keys CDB (lambda (s) (and (> (length s) 2) (string= "gl" s :end2 2)))))))


(defun LISP-GL-NAME (Entry-Name)
  (intern (string-upcase Entry-Name)))


(defun GL-TYPE-NAME (Type)
  (remove #\> (remove #\< (symbol-name Type))))


(defun OPENGL-FUNCTION-CALL (Name Entry-Name Arguments Result-Type)
  `(progn
     (declaim (inline ,(lisp-gl-name Name)))
     (export '(,(lisp-gl-name Name)))
     (setf (get ',(lisp-gl-name Name) :api-name) ,Name)
     (defun ,(lisp-gl-name Name)
            ,(let ((I -1))
              (mapcar #'(lambda (Argument) (incf I) (intern (format nil "~A-~D" (gl-type-name Argument) I))) 
                      Arguments))
       (ccl::external-call 
        ,Entry-Name
        ,@(let ((I -1))
          (mapcan #'(lambda (Argument) 
                      (incf I)
                      (list Argument (intern (format nil "~A-~D" (gl-type-name Argument) I))))
                  Arguments))
        ,Result-Type))))

;; one line of code => 1000 OpenGL functions right here:
;; this compiles into a large but quickly loading binary

#.`(progn ,@(map-opengl-functions #'opengl-function-call))

;********************************
;* Constants                    *
;********************************

(defun MAP-OPENGL-CONSTANTS (Map-Function) "
  in:  Map-Function Name Value -> list.
  out: List-of-all-function-results."
  (let ((CDB (ccl::db-constants (ccl::use-interface-dir :gl))))
    (mapcar
     #'(lambda (Name) 
         (let ((Value (ccl::db-lookup-constant CDB Name)))
           (if Value
             (funcall Map-Function Name Value)
             (warn "Odd.  There's no foreign constant info for ~s" Name))))
     ;; GL, GLU and GLUT constants
     (ccl::cdb-enumerate-keys CDB #'(lambda (Item)
                                      (or (and (> (length Item) 3) (string= "GL_" Item :end2 3))
                                          (and (> (length Item) 4) (string= "GLU_" Item :end2 4))
                                          (and (> (length Item) 5) (string= "GLUT_" Item :end2 5))))))))


(defun OPENGL-CONSTANT-DEFINITION (Name Value)
  `(progn
     (export '(,(read-from-string Name)))
     (setf (get ',(lisp-gl-name Name) :api-name) ,Name)
     (defconstant ,(read-from-string Name) ,Value)))

;; one line of code > 2000 constant definitions

#.`(progn ,@(map-opengl-constants #'opengl-constant-definition))



#| Examples:

(length (map-opengl-functions #'opengl-function-call))

(length (map-opengl-constants #'opengl-constant-definition))


|#