(in-package :lui)

;**********************************
;* Table-View-Data-Source         *
;**********************************

(defclass TABLE-VIEW-DATA-SOURCE (ns:ns-object)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object
	      :documentation "Provides the methods to populate the table view"))


(objc:defmethod (#/numberOfRowsInTableView: :<NSI>nteger)
                ((self table-view-data-source)
                 table-view)
  (rows-in-table (lui-view table-view)))


(objc:defmethod #/tableView:objectValueForTableColumn:row: ((self table-view-data-source) table-view table-column (row :<NSI>nteger))
  (when (columns (lui-view table-view))
    (if (elt (columns (lui-view table-view)) (read-from-string (ccl::lisp-string-from-nsstring  (#/identifier table-column))))
      (let ((value (elt (elt (columns (lui-view table-view)) (read-from-string (ccl::lisp-string-from-nsstring  (#/identifier table-column)))) row)))
        
        (if (subtypep (type-of value) 'string)
          (native-string value)
          (native-string (write-to-string value))))
      (native-string "."))))

;**********************************
;* Table-View                     *
;**********************************

(defclass NATIVE-TABLE-VIEW (ns:ns-table-view)
  ((lui-view :accessor lui-view :initform nil :initarg :lui-view))
  (:metaclass ns:+ns-object
	      :documentation ""))


(objc:defmethod (#/textShouldEndEditing: :<BOOL>) ((self native-table-view) textObject)
  (setf (elt (elt (columns (lui-view self)) (#/editedColumn self)) (#/editedRow self)) (ccl::lisp-string-from-nsstring (#/string textObject)))
  (should-end-editing-notificaiton (lui-view self))
  (call-next-method textObject))


(defmethod make-native-object ((Self table-view))
  (let ((Native-Control (make-instance 'native-table-view :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (ns:with-ns-rect (Frame2 (x self) (+ 20 (y Self)) (width Self) (- (height Self) 20))
        (#/initWithFrame: Native-Control Frame)
        (#/sizeLastColumnToFit Native-Control)
        (#/setDataSource: Native-Control (make-instance 'table-view-data-source))
    Native-Control))))


(defmethod ADD-COLUMN ((self table-view) &key (editable nil) (Title "Field"))
  (let ((column (#/initWithIdentifier: (#/alloc ns:ns-table-column) (native-string (write-to-string (length (columns self)))))))
    (if editable 
      (#/setEditable: column #$YES)
      (#/setEditable: column #$NO))
    (#/setTitle: (#/headerCell column) (native-string Title))
    (#/addTableColumn: (native-view self) column))
  (setf (columns self) (append (columns self) (list (make-list (rows-in-table self) :initial-element "."))))
  (recalculate-rows self))


(defmethod RELOAD-DATA ((Self table-view))
  (#/reloadData (native-view self)))


(defmethod SET-COLOR-OF-CELL-AT-ROW-COLUMN ((Self table-view) row column red green blue)
  (#/setTextColor: (#/preparedCellAtColumn:row: (lui::native-view self) column row ) (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color red green blue 1.0)))


(defmethod GET-STRING-VALUE-OF-CELL-AT-ROW-COLUMN ((self table-view) row column)
  (ccl::lisp-string-from-nsstring (#/stringValue (#/preparedCellAtColumn:row: (native-view self) column row))))


(defmethod GET-ROW-OF-CELL-BEING-EDITED ((self table-view))
  (#/editedRow (native-view self)))


(defmethod GET-COLUMN-OF-CELL-BEING-EDITED ((self table-view))
  (#/editedColumn (native-view self)))