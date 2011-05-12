(in-package :lui)

;;*********************************
;; Native Strings                 *
;;*********************************

(defun NATIVE-STRING (String) "
  Return a native string"
  (#/autorelease (ccl::%make-nsstring String)))
