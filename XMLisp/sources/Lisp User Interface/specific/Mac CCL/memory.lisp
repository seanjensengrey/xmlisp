;;;;-*- Mode: Lisp; Package: LUI -*-
;*********************************************************************
;*                                                                   *
;*       M E M O R Y                                                 *
;*                                                                   *
;*********************************************************************
   ;* Author    : Alexander Repenning (alexander@agentsheets.com)    *
   ;*             http://www.agentsheets.com                         *
   ;* Copyright : (c) 1996-2009, AgentSheets Inc.                    *
   ;* Filename  : memory.lisp                                        *
   ;* Updated   : 04/28/09                                           *
   ;* Version   :                                                    *
   ;*   1.0     : based on AGL.lisp v 1.2.6                          *
   ;*   1.1     : 01/10/09 using package instead of hash-table       *   
   ;* SW/HW     : CCL 1.2 OS X, Mac PPC, Intel                       *
   ;* Abstract  : Tools to create and access memory                  *
   ;*                                                                *
   ;******************************************************************

(in-package :lui)

(export '(sizeof make-vector with-vector with-vector-of-size dispose-vector 
                 make-byte-vector get-byte set-byte make-vector-of-size get-long SET-BYTE-VECTOR))

;;______________________________________________________________
;; Universally Unique Identifier (UUID)                         |
;; http://en.wikipedia.org/wiki/Universally_Unique_Identifier   |
;;_____________________________________________________________/

(defun UNIVERSALLY-UNIQUE-IDENTIFIER () "
  Return an Universally Unique Identifier (UUID). An UUID is an identifier standard 
  used in software construction, standardized by the Open Software Foundation (OSF) as
  part of the Distributed Computing Environment (DCE)"
  #-windows-target (ccl::lisp-string-from-nsstring (#_CFUUIDCreateString (%null-ptr) (#_CFUUIDCreate (%null-ptr))))
  #+windows-target (rlet ((uuid :<UUID>))
		     (#_UuidCreate uuid)
		     (format nil "~8,'0X-~4,'0X-~4,'0X-~4,'0X-~12,'0X"
			     (pref uuid :<UUID>.<D>ata1)
			     (pref uuid :<UUID>.<D>ata2)
			     (pref uuid :<UUID>.<D>ata3)
			     (dpb (%get-unsigned-byte (pref uuid :<UUID>.<D>ata4) 0)
				  (byte 8 8)
				  (%get-unsigned-byte (pref uuid :<UUID>.<D>ata4) 1))
			     (dpb (dpb (%get-unsigned-word (pref uuid :<UUID>.<D>ata4) 1)
				       (byte 16 16)
				       (%get-unsigned-word (pref uuid :<UUID>.<D>ata4) 2))
				  (byte 32 32)
				  (%get-unsigned-word (pref uuid :<UUID>.<D>ata4) 3)))))

;;_____________________
;; Sizeof             |
;;____________________/

(defgeneric SIZEOF (Object)
  (:documentation "the size of object in memory. Object can be instance or type name"))


(defmethod SIZEOF ((Self float)) 4)
(defmethod SIZEOF ((Self double-float)) 8)
(defmethod SIZEOF ((Self fixnum)) 4)

(defmethod SIZEOF ((Type (eql 'long))) 4)
(defmethod SIZEOF ((Type (eql 'float))) 4)
(defmethod SIZEOF ((Type (eql 'double-float))) 8)
(defmethod SIZEOF ((Type (eql 'fixnum))) 4)

(defmethod SIZEOF ((Self macptr))
  #-windows-target (#_GetPtrSize Self)
  #+windows-target (#_HeapSize (#_GetProcessHeap) 0 Self))

;;_____________________
;; Memory Vectors      |
;;____________________/

(defun MAKE-VECTOR (&rest Values) "
  in:  &rest Values 
  out: Vector. 
  Create a vector initialized with <Values>.
  Vector is not automatically deallocated."
  (let* ((Index 0)
         (Size (reduce #'+ Values :key #'sizeof))
         (&Vector #-windows-target (#_NewPtr Size)
		  #+windows-target (#_HeapAlloc (#_GetProcessHeap) 0 Size)))
    (dolist (Value Values &Vector)
      (etypecase Value
        (fixnum (setf (%get-long &Vector Index) Value))
        (single-float (setf (%get-single-float &Vector Index) Value))
        (double-float (setf (%get-double-float &Vector Index) Value)))
      (incf Index (sizeof Value)))))


(defun MAKE-VECTOR-OF-SIZE (Size) "
  in: Size int.
  out: Vector.
  Make vector of byte size"
  #-windows-target (#_NewPtr Size)
  #+windows-target (#_HeapAlloc (#_GetProcessHeap) 0 Size))


(defun MAKE-BYTE-VECTOR (&rest Values)"
  in:  &rest Values 
  out: Vector. 
  Create a vector initialized with <Values>.
  Vector is not automatically deallocated."
  (let* ((Index 0)
         (&Vector #-windows-target (#_NewPtr (length Values))
		  #+windows-target (#_HeapAlloc (#_GetProcessHeap) 0 (length Values))))
    (dolist (Value Values &Vector)
      (setf (%get-byte &Vector Index) Value)
      (incf Index))))


(defun SET-BYTE-VECTOR (Vector &rest Values) "
  in:  Vector *byte, &rest Values list of byte.
  out: Vector *byte.
  Set the bytes of vector to new values."
 (declare (dynamic-extent Values))
 (when (> (length Values) (sizeof Vector))
   (error "out of range"))
 (let ((Offset 0))
   (declare (fixnum Offset) 
            (optimize (speed 3) (safety 0)))
   (dolist (Value Values Vector)
     (setf (%get-byte Vector Offset) Value)
     (incf Offset))))
  

(defun DISPOSE-VECTOR (Vector) "
  in: Vector.
  Dispose of vector."
 #-windows-target (#_DisposePtr Vector)
 #+windows-target (#_HeapFree (#_GetProcessHeap) 0 Vector))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro WITH-VECTOR ((Vector &rest Values) &body Forms)
  `(let ((,Vector (make-vector ,@Values)))
     (unwind-protect 
         (progn
           ,@Forms)
       (dispose-vector ,Vector)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro WITH-VECTOR-OF-SIZE ((Vector Size) &body Forms)
  `(let ((,Vector (make-vector-of-size ,Size)))
     (unwind-protect
         (progn
           ,@Forms)
       (dispose-vector ,Vector)))))

;;_____________________
;; Vectors Access      |
;;____________________/

(proclaim '(inline get-single-float get-byte get-long))


(defun GET-BYTE (Vector &optional (Offset 0)) "
  in:  Vector *bye, &optional Offset int default 0.
  out: Byte byte.
  Return byte at offset."
 (%get-byte Vector Offset))


(defun SET-BYTE (Vector Value &optional (Offset 0)) "
  in:  Vector *byte, Value byte; &optional Offset int default 0.
  Set vector at byte offset to value."
 (setf (%get-byte Vector Offset) Value))


(defun GET-LONG (Vector &optional (Offset 0)) "
  in:  Vector *bye, &optional Offset int default 0.
  out: Float
  Return Float at byte offset."
 (%get-long Vector Offset))


(defun GET-SINGLE-FLOAT (Vector &optional (Offset 0)) "
  in:  Vector *bye, &optional Offset int default 0.
  out: Float
  Return Float at byte offset."
 (%get-single-float Vector Offset))


(defun GET-DOUBLE-FLOAT (Vector &optional (Offset 0)) "
  in:  Vector *bye, &optional Offset int default 0.
  out: Float
  Return Float at byte offset."
 (%get-double-float Vector Offset))

;;_________________________________
;; Vector Constants with Reader    |
;; e.g.  {0.5 0.6 0.7}             |
;;________________________________/


(defpackage STATIC-VECTORS)


(proclaim '(inline GET-CACHED-VECTOR))

(defun GET-CACHED-VECTOR (Key &rest Values) "
  in:  Key symbol; &rest Values.
  out: Vector
  Returns vector. Use key to cache and implement constant pointers"
  (or
   (and (boundp Key) (symbol-value Key))
   (set Key (apply #'make-vector Values))))


(defun READ-NUMBER (Stream)
  (read-from-string
   (with-output-to-string (Out)
     (loop
       (let ((Char (read-char Stream nil nil)))
         (case Char
          ((#\} #\Space)
           (unread-char Char Stream)
           (return))
          (t (write-char Char Out))))))))


(defun VECTOR-READER (Stream Char)
  (declare (ignore Char))
  (let ((Numbers nil))
    (loop
      (let ((Char (read-char Stream nil #\])))
        (case Char
          (#\} (return `(get-cached-vector 
                         ',(intern (universally-unique-identifier) :STATIC-VECTORS) 
                         ,@(reverse Numbers))))
          ((#\Space #\Tab #\Newline))
          (t (unread-char Char Stream)
             (push (read-number Stream) Numbers)))))))


(set-macro-character #\{ #'vector-reader)


#| Examples:

(sizeof pi)
(sizeof 3.14)


(make-vector 3.1415 7.5 6.7)

(with-vector (V 1.0 2.0 3.0) 
  (dotimes (i 3)
    (print (get-single-float v (* i #.(sizeof 0.0))))))


;; eval a couple of times: notice, pointer address is different every time
{3.14 5.0 3.0}

;; but

(defun TEST ()
  {3.14 5.0 3.0})

;; this function will return the SAME vector, i.e., pointer at same address 
;; with same content every time, until you eval the function definition again
;; consider this to be a constant: do not dispose
(test)


(get-single-float (test))
(get-single-float (test) #.(* 1 (sizeof 0.0)))
(get-single-float (test) #.(* 2 (sizeof 0.0)))

(defun TEST2 ()
  (make-vector 3.14 5.0 3.0))

(test2)

(sizeof {0.5d0 0.6d0 0.7d0})
(sizeof {0.5 0.6 0.7})
(sizeof {1 2 3})

;; compare speed

(defun VALUE-1 ()
  (with-vector (v 1.0 2.0 3.0)
    (get-single-float v)))

(value-1)

(defun VALUE-2 ()
  (get-single-float {1.0 2.0 3.0}))

(value-2)


(sizeof (make-byte-vector 3 4 5))

(with-vector-of-size (Selection 2048)
  (sizeof Selection))


|#