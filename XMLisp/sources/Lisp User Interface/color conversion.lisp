(in-package :lui)

(defun CONVERT-HEX-COLOR-STRING-TO-RGBA-LIST (RGBA-String)
  (let ((*Read-Base* 16))
    (values 
     (/ (read-from-string (subseq RGBA-String 0 2)) 256.0)
     (/ (read-from-string (subseq RGBA-String 2 4)) 256.0)
     (/ (read-from-string (subseq RGBA-String 4 6)) 256.0)
     (/ (read-from-string (subseq RGBA-String 6 8)) 256.0))))
  

#| Examples

(convert-hex-color-string-to-rgba-list "EDEDEDFF")
(convert-hex-color-string-to-rgba-list "000000FF")

|#

