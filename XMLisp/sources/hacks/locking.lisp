(defvar *A-Lock* (make-lock "A"))

(defparameter *pa* (process-run-function '(:name "A")
                                   #'(lambda () 
                                       (loop 
                                         (with-lock-grabbed (*A-Lock*)
                                           (dotimes (i 10)
                                             (format t "~%La~A" i)
                                             (sleep 0.5)))
                                         (dotimes (i 10)
                                             (format t "~%Ua~A" i)
                                             (sleep 0.5))))))

(defparameter *pb* (process-run-function '(:name "B")
                                   #'(lambda () 
                                       (loop 
                                         (with-lock-grabbed (*A-Lock*)
                                           (dotimes (i 10)
                                             (format t "~%Lb~A" i)
                                             (sleep 0.5)))
                                         (dotimes (i 10)
                                             (format t "~%Ub~A" i)
                                             (sleep 0.5))))))

(grab-lock *A-Lock*)

(release-lock *A-Lock*)

(with-lock-grabbed (*A-Lock*)
  (* 7.5 7.7))

(defparameter *NSlock* (#/alloc ns:ns-lock))

(#/unlock *NSlock*)

(progn
  (#/lock *NSlock*)  ;;slow!!
  (* 3.4 5.6))



(process-kill *pa*)
(process-kill *pb*)

