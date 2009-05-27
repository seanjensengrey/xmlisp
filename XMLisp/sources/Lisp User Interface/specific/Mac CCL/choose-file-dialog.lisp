;; file dialogs
;; 12/12/08 Alexander Repenning

(in-package :lui)


(defun choose-file-dialog ()
  (let* ((panel (#/openPanel ns:ns-open-panel))
	 (dc (#/sharedUserDefaultsController ns:ns-user-defaults-controller))
         (values (#/values dc))
	 (key #@"cclDirectory"))
    (#/setAllowsMultipleSelection: panel nil)
    (#/setCanChooseDirectories: panel nil)
    (#/setCanChooseFiles: panel t)
    (when (eql (#/runModalForDirectory:file:types: panel
						   (#/valueForKey: values key)
						   +null-ptr+
						   +null-ptr+)
	       #$NSOKButton)
      (ccl::lisp-string-from-nsstring (#/objectAtIndex: (#/filenames Panel) 0)))))



#| Examples:

(choose-file-dialog)

|#

