;; file dialogs
;; 12/12/08 Alexander Repenning

(in-package :lui)


(defun choose-file-dialog (&key 
                           (directory "cclDirectory") ;
                           ;mac-file-type
                           ;mac-file-creator
                           (button-string "Open") ;;
                           ;(cancel-button-string "Cancel")
                           (window-title "Choose a File") ;;
                           (allow-multiple-files nil)
                           prompt ;;
                           (window-position (ns:make-ns-point 100 100))
                           file-type-string-list ;;
                           )
  (let* ((panel (#/openPanel ns:ns-open-panel))
	 (dc (#/sharedUserDefaultsController ns:ns-user-defaults-controller))
         (values (#/values dc))
	 ;(key #@"cclDirectory")
         (file-types +null-ptr+))
    (when file-type-string-list
      (dotimes (i (length file-type-string-list))
        (unless (equal (type-of (elt file-type-string-list i)) (type-of (native-string "string")))
          (progn
            (setf (elt file-type-string-list i) (native-string (elt file-type-string-list i))))))
      (setf file-types (gui::ns-array-from-list file-type-string-list)))
    (when button-string
      (setf button-string (ccl::%make-nsstring button-string))
      (#/setPrompt: panel button-string))
    (when window-title
      (setf window-title (ccl::%make-nsstring window-title))
      (#/setTitle: panel window-title))
    (when prompt
      (setf prompt (ccl::%make-nsstring prompt))
      (#/setMessage: panel  prompt))
    (#/setAllowsMultipleSelection: panel allow-multiple-files)
    (#/setCanChooseDirectories: panel nil)
    (#/setCanChooseFiles: panel t)
    (#/setFrameOrigin: panel window-position)
    (#/setAllowedFileTypes:  panel (#/arrayWithObject: ns:ns-array (native-string "lisp")))
    (#/makeKeyAndOrderFront: panel +null-ptr+)
    (when (eql (#/runModalForDirectory:file:types: panel
						   (#/valueForKey: values (native-string directory))
						   +null-ptr+
						   file-types)
	       #$NSOKButton)
      (pathname (ccl::lisp-string-from-nsstring (#/objectAtIndex: (#/filenames Panel) 0))))))


(defun choose-directory-dialog (&key 
                           (directory "cclDirectory") ;
                           ;mac-file-type
                           ;mac-file-creator
                           (button-string "Open") ;;
                           ;(cancel-button-string "Cancel")
                           (window-title "Choose a File") ;;
                           prompt ;;
                           (window-position (ns:make-ns-point 100 100)))
  (let* ((panel (#/openPanel ns:ns-open-panel))
	 (dc (#/sharedUserDefaultsController ns:ns-user-defaults-controller))
         (values (#/values dc))
	 ;(key #@"cclDirectory")
         )
    (when button-string
      (setf button-string (ccl::%make-nsstring button-string))
      (#/setPrompt: panel button-string))
    (when window-title
      (setf window-title (ccl::%make-nsstring window-title))
      (#/setTitle: panel window-title))
    (when prompt
      (setf prompt (ccl::%make-nsstring prompt))
      (#/setMessage: panel  prompt))
    (#/setAllowsMultipleSelection: panel nil)
    (#/setCanChooseDirectories: panel t)
    (#/setCanChooseFiles: panel nil)
    (#/setFrameOrigin: panel window-position)
    (#/setAllowedFileTypes:  panel (#/arrayWithObject: ns:ns-array (native-string "lisp")))
    (#/makeKeyAndOrderFront: panel +null-ptr+)
    (when (eql (#/runModalForDirectory:file:types: panel
						   (#/valueForKey: values (native-string directory))
						   +null-ptr+
						   +null-ptr+)
	       #$NSOKButton)
      (pathname (ccl::lisp-string-from-nsstring (#/objectAtIndex: (#/filenames Panel) 0))))))




(defun PARSE-LISP-PATH-FROM-STIRNG (string)
  (count "/"  string :test #'string=)
  )


#| Examples:

(choose-file-dialog  :file-type-string-list (list  "lisp"   "window") )
(choose-directory-dialog)


|#

