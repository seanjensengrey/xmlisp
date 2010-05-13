(in-package :easygui)

(defun choose-directory-dialog (&key button-string)
  (gui::with-autorelease-pool 
      (let* ((panel (dcc (#/autorelease (dcc (#/openPanel ns:ns-open-panel)))))) ; allocate an NSOpenPanel
        (dcc (#/setAllowsMultipleSelection: panel nil)) ; return at most one filename
        (dcc (#/setCanChooseDirectories: panel #$YES))
        (dcc (#/setCanChooseFiles: panel #$NO))
        (when button-string
          (setf button-string (ccl::%make-nsstring button-string))
          (dcc (#/setPrompt: panel button-string)))
        (when (eql #$NSOKButton
                   (dcc (#/runModalForDirectory:file:types: panel
                           +null-ptr+ ; default to last dir used
                           +null-ptr+ ; no preselected file
                           ;; If not NIL below then an ObjC array containing NSStrings could be used
                           ;; to restrict the file types we're interested in
                           +null-ptr+)))
          ;; Because we told the panel to disallow multiple selection,
          ;; there should be exactly one object in this array, an
          ;; NSString describing the selected file.
          (let* ((files (dcc (#/filenames panel))) thing)
            (if (eql 1 (dcc (#/count files)))
              (progn
                (setf thing (dcc (#/objectAtIndex: files 0)))
                (gui::lisp-string-from-nsstring thing))
              "Don't know why we didn't get an NSArray containing exactly 1 file here."))))))

#| Examples:

(choose-directory-dialog :button-string "fart")

|#