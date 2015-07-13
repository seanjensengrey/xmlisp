
(defparameter *Submission-Coder* #x3F095872)



(defun ENCODE-SUBMISSION-KEY (Code)
  (format nil "~X" (logxor (read-from-string (format nil "#x~A~A" Code Code)) *Submission-Coder*)))


(defun DECODE-SUBMISSION-KEY (Key)
  (let ((Code (format nil "~X" (logxor (read-from-string (format nil "#x~A" Key)) *Submission-Coder*))))
    (let ((Valid (string= (subseq Code 0 (truncate (length Code) 2)) 
                          (subseq Code (truncate (length Code) 2) (length Code)))))
    (values
     (if Valid (read-from-string (subseq Code 0 (truncate (length Code) 2))) nil)
     Valid))))


(defun MAKE-VALID-OR-NOT-RANDOM-KEY ()
  (with-output-to-string (Key)
    (dotimes (i 8)
      (princ (char "01234567890ABCDEF" (random 16)) Key))))


(defun TEST-KEYS (&optional (n 1000))
  (let ((Valid-Ones 0))
    (dotimes (i n)
      (multiple-value-bind (Code Valid)
                           (decode-submission-key (make-valid-or-not-random-key))
        (when Valid
          (print Code)
          (incf Valid-Ones))))
    (/ Valid-Ones n)))
    




#| Examples:

(encode-submission-key #x000A)

(decode-submission-key 
  (encode-submission-key 100))


(make-valid-or-not-random-key)



(test-keys 100000)

(decode-submission-key "3F195972")
(decode-submission-key "3F155972")


|#