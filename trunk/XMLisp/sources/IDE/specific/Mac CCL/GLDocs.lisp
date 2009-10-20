(in-package :hemlock)

(defparameter *accepted-gl-commands* (list "glTexCoord1" "glTexCoord2" "glTexCoord3" "glTexCoord4"
                                           ) "List of accepted open  gl commands that contain integers")

(defun MAKE-OPENGL-BROWSER (url)
  (let ((*Package* (find-package :xlui)))
    (let ((Window (read-from-string (format nil "<application-window margin=\"0\" height=\"600\" width=\"400\"><web-browser height=\"600\" name=\"browser\" width=\"400\" url=\"~A\"/></application-window>" url))))
      Window)))

(defun PARSE-STRING (string)
  (let ((integer-position (find-integer-position string)))
    (if integer-position
      (subseq string 0 integer-position)
      string)))
      


(defun FIND-INTEGER-POSITION (test-string)
  (or (position #\1 test-string) 
      (position #\2 test-string)
      (position #\3 test-string)
      (position #\4 test-string)))







(defcommand "Show GL Docs" (p)
  "Shows the documentation for the openGL command that was just typed"
  "Shows the documentation for the openGL command that was just typed"
  (declare (ignore p))
  (let ((command (buffer-current-string)))
    (if (eql 0 (count command *accepted-gl-commands* :test #'string=))
      (setf command (parse-string command))
      (print "dont cut"))
    (let ((url (concatenate 'string "http://www.opengl.org/sdk/docs/man/xhtml/" command ".xml")))
      (make-opengl-browser url))))   

(bind-key "Show GL Docs" #k"control-x control-h")