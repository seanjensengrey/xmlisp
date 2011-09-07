;; file dialogs
;; 12/12/08 Alexander Repenning

(in-package :lui)

(defun ns-array-from-list (list)
  "The list must be an array of ns-objects"
  (let ((nsa (#/array ns:ns-array)))
    (dolist (list-item list)
      (setf nsa (#/arrayByAddingObject: nsa list-item)))
    nsa))


(defun choose-file-dialog (&key 
                           directory  
                           ;mac-file-type
                           ;mac-file-creator
                           (button-string "Open") 
                           cancel-button-string 
                           (window-title "Choose a File") 
                           (allow-multiple-files nil)
                           prompt 
                           (window-position (ns:make-ns-point 100 100))
                           file-type-string-list 
                           )
  (let* ((panel (#/openPanel ns:ns-open-panel))
	 ;(dc (#/sharedUserDefaultsController ns:ns-user-defaults-controller))
         ;(values (#/values dc))
	 ;(key #@"cclDirectory")
         (file-types +null-ptr+)
         )
    ;; Kind of nasty code just to change the title of the cancel button, need to go through two layers of subviews to find the button and change its title.  
    (if cancel-button-string
      (let ((subviews (gui::list-from-ns-array (#/subviews (#/contentView panel)))))
        (dolist (subview subviews)
          (if (equal (type-of subview) 'NS:NS-VIEW)
            (let ((subviews2 (gui::list-from-ns-array (#/subviews subview))))
              (dolist (subview2 subviews2)
                (if (equal (type-of subview2) 'NS:NS-BUTTON )
                  (if (equal (#/title subview2) (native-string "Cancel"))
                    (#/setTitle: subview2 (native-string cancel-button-string))))))))))
    (when file-type-string-list
      (setf file-type-string-list (mapcar #'native-string file-type-string-list))
      (setf file-types (ns-array-from-list file-type-string-list)))
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
    ;#-cocotron (#/setAllowedFileTypes:  panel (#/arrayWithObject: ns:ns-array (native-string "lisp")))
    ;#-cocotron (#/makeKeyAndOrderFront: panel +null-ptr+)
    #-cocotron (#/orderFront: panel +null-ptr+)
    (when (eql (#/runModalForDirectory:file:types: panel
						   (native-string directory)
						   +null-ptr+
						   file-types)
	       #$NSOKButton)
      (if allow-multiple-files
        (let ((filenames (gui::list-from-ns-array (#/filenames Panel))))
          (dotimes (i (length filenames))
            (setf (elt filenames i) (pathname (ccl::lisp-string-from-nsstring (elt filenames i)))))
          filenames)
        (pathname (ccl::lisp-string-from-nsstring (#/objectAtIndex: (#/filenames Panel) 0)))))))


(defun choose-new-file-dialog (&key 
                               directory  
                               ;mac-file-type
                               ;mac-file-creator
                               (button-string "Save") 
                               cancel-button-string 
                               (window-title "Create a New File") 
                               ;(format-list *save-as-cfarray* format-list-sup)
                               ;name
                               ;extended-reply-p
                               ;(format-query-p nil query-p-sup)
                               ;dont-confirm-replacement ;; omit the already-exists alert
                               prompt 
                               (window-position (ns:make-ns-point 100 100))
                               file-type-string-list 
                               )
  (let* ((panel (#/savePanel ns:ns-save-panel))
	 ;(dc (#/sharedUserDefaultsController ns:ns-user-defaults-controller))
         ;(values (#/values dc))
	 ;(key #@"cclDirectory")
         (file-types +null-ptr+))
    ;; Kind of nasty code just to change the title of the cancel button, need to go through two layers of subviews to find the button and change its title.  
    (if cancel-button-string
      (let ((subviews (gui::list-from-ns-array (#/subviews (#/contentView panel)))))
        (dolist (subview subviews)
          (if (equal (type-of subview) 'NS:NS-VIEW)
            (let ((subviews2 (gui::list-from-ns-array (#/subviews subview))))
              (dolist (subview2 subviews2)
                (if (equal (type-of subview2) 'NS:NS-BUTTON )
                  (if (equal (#/title subview2) (native-string "Cancel"))
                    (#/setTitle: subview2 (native-string cancel-button-string))))))))))
    (when file-type-string-list
      (dotimes (i (length file-type-string-list))
        (unless (equal (type-of (elt file-type-string-list i)) (type-of (native-string "string")))
          (progn
            (setf (elt file-type-string-list i) (native-string (elt file-type-string-list i))))))
      (setf file-types (ns-array-from-list file-type-string-list)))
    (when button-string
      (setf button-string (ccl::%make-nsstring button-string))
      (#/setPrompt: panel button-string))
    (when window-title
      (setf window-title (ccl::%make-nsstring window-title))
      (#/setTitle: panel window-title))
    (when prompt
      (setf prompt (ccl::%make-nsstring prompt))
      (#/setMessage: panel  prompt))
    (#/setCanChooseDirectories: panel nil)
    (#/setCanChooseFiles: panel nil)
    ;(#/setNameFieldLabel: panel (native-string "hello:"))
    (#/setFrameOrigin: panel window-position)
    #-cocotron (#/setAllowedFileTypes:  panel file-types)
    ;#-cocotron (#/makeKeyAndOrderFront: panel +null-ptr+)
    #-cocotron (#/orderFront: panel +null-ptr+)
    (when (eql (#/runModalForDirectory:file:types: panel
						   (native-string directory)
						   +null-ptr+
						   file-types)
	       #$NSOKButton)
      (pathname (ccl::lisp-string-from-nsstring (#/objectAtIndex: (#/filenames Panel) 0))))))


(defun choose-directory-dialog (&key 
                                directory 
                                ;mac-file-type
                                ;mac-file-creator
                                (button-string "Save") 
                                cancel-button-string 
                                (window-title "Choose a Directory") 
                                prompt 
                                name-field-string
                                (window-position (ns:make-ns-point 100 100)))
  (let* ((panel (#/openPanel ns:ns-open-panel))
	 ;(dc (#/sharedUserDefaultsController ns:ns-user-defaults-controller))
         ;(values (#/values dc))
	 ;(key #@"cclDirectory")
         )
    ;; Kind of nasty code just to change the title of the cancel button, need to go through two layers of subviews to find the button and change its title.  
    (if cancel-button-string
      (let ((subviews (gui::list-from-ns-array (#/subviews (#/contentView panel)))))
        (dolist (subview subviews)
          (if (equal (type-of subview) 'NS:NS-VIEW)
            (let ((subviews2 (gui::list-from-ns-array (#/subviews subview))))
              (dolist (subview2 subviews2)
                (if (equal (type-of subview2) 'NS:NS-BUTTON )
                  (if (equal (#/title subview2) (native-string "Cancel"))
                    (#/setTitle: subview2 (native-string cancel-button-string))))))))))
    (when button-string
      (setf button-string (ccl::%make-nsstring button-string))
      (#/setPrompt: panel button-string))
    (when window-title
      (setf window-title (ccl::%make-nsstring window-title))
      (#/setTitle: panel window-title))
    (when prompt
      (setf prompt (ccl::%make-nsstring prompt))
      (#/setMessage: panel  prompt))
    (when name-field-string
      (#/setNameFieldLabel: panel (ccl::%make-nsstring name-field-string)))
    (#/setAllowsMultipleSelection: panel nil)
    (#/setCanChooseDirectories: panel t)
    (#/setCanChooseFiles: panel nil)
    (#/setFrameOrigin: panel window-position)
    (#/setMaxSize: panel (ns:make-ns-size 800.0 619.0))
    ; #-cocotron (#/setAllowedFileTypes:  panel (#/arrayWithObject: ns:ns-array (native-string "lisp")))
    ;#-cocotron (#/makeKeyAndOrderFront: panel +null-ptr+)
    #-cocotron (#/orderFront: panel +null-ptr+)
    (when (eql (#/runModalForDirectory:file:types: panel
						   (native-string directory) ;(#/valueForKey: values (native-string directory))
						   +null-ptr+
						   +null-ptr+)
	       #$NSOKButton)
      (make-pathname :directory (ccl::lisp-string-from-nsstring (#/objectAtIndex: (#/filenames Panel) 0))))))


(defun choose-new-directory-dialog (&key 
                                    directory 
                                    ;mac-file-type
                                    ;mac-file-creator
                                    (button-string "Save") 
                                    cancel-button-string 
                                    (window-title "Create a New Directory") 
                                    prompt 
                                    name-field-string
                                    (window-position (ns:make-ns-point 100 100)))
  (let* ((panel (#/savePanel ns:ns-save-panel))
	 ;(dc (#/sharedUserDefaultsController ns:ns-user-defaults-controller))
         ;(values (#/values dc))
	 ;(key #@"cclDirectory")
         ;(file-types +null-ptr+)
         )
    ;; Kind of nasty code just to change the title of the cancel button, need to go through two layers of subviews to find the button and change its title.  
    (if cancel-button-string
      (let ((subviews (gui::list-from-ns-array (#/subviews (#/contentView panel)))))
        (dolist (subview subviews)
          (if (equal (type-of subview) 'NS:NS-VIEW)
            (let ((subviews2 (gui::list-from-ns-array (#/subviews subview))))
              (dolist (subview2 subviews2)
                (if (equal (type-of subview2) 'NS:NS-BUTTON )
                  (if (equal (#/title subview2) (native-string "Cancel"))
                    (#/setTitle: subview2 (native-string cancel-button-string))))))))))
    (when button-string
      (setf button-string (ccl::%make-nsstring button-string))
      (#/setPrompt: panel button-string))
    (when window-title
      (setf window-title (ccl::%make-nsstring window-title))
      (#/setTitle: panel window-title))
    (when prompt
      (setf prompt (ccl::%make-nsstring prompt))
      (#/setMessage: panel  prompt))
     (when name-field-string
       (#/setNameFieldLabel: panel (ccl::%make-nsstring name-field-string)))
    #-cocotron
    (#/setCanChooseDirectories: panel #$YES)
    ;(#/setCanChooseFiles: panel nil)
    ;(#/setNameFieldLabel: panel (native-string "hello:"))
    (#/setDirectory: panel (native-string directory))
    (#/setFrameOrigin: panel window-position)
    ;#-cocotron (#/setAllowedFileTypes:  panel (#/arrayWithObject: ns:ns-array (native-string "lisp")))
    ; #-cocotron (#/makeKeyAndOrderFront: panel +null-ptr+)
    ;(#/makeKeyWindow panel)
    #-cocotron (#/orderFront: panel +null-ptr+)
    #|
    (when (eql (#/runModalForDirectory:file:types: panel
						   (native-string directory)
						   +null-ptr+
						   file-types)
	       #$NSOKButton)
    |#
    (#/runModal  panel)
    (if (equal (#/filename Panel) +null-ptr+)
      nil
      (progn
        #+cocotron
        (concatenate 'string (ccl::lisp-string-from-nsstring (#/filename Panel)) "\\")
        #-cocotron
         (concatenate 'string (ccl::lisp-string-from-nsstring (#/filename Panel)) "/")))))


#| Examples:

(choose-new-file-dialog   :file-type-string-list (list "lisp" "window"))

(choose-new-directory-dialog)

(choose-directory-dialog :directory "/home")

(choose-file-dialog :directory "/home" :file-type-string-list (list  "lisp"   "window"))

|#

