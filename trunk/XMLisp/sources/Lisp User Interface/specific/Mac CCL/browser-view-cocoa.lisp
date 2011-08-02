(in-package :lui)

(export '(add-column ))

;**********************************
;* MY-BROWSER-DELEGATE            *
;**********************************

(defclass MY-BROWSER-DELEGATE (ns:ns-object)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object
	      :documentation "delegate object receiving window events"))


(objc:defmethod (#/browser:numberOfRowsInColumn: :<NSI>nteger) ((self my-browser-delegate) browser (column :<NSI>nteger))
  (let ((selected-node-item (lui-view self)))
    (dotimes (i column)
      (when (get-selected-row-in-column (lui-view self) i)
        (setf selected-node-item (elt (nodes selected-node-item) (get-selected-row-in-column (lui-view self) i)))))
    (length (nodes selected-node-item))))


(objc:defmethod (#/browser:willDisplayCell:atRow:column: :void) ((self my-browser-delegate) browser cell (row :<NSI>nteger) (column :<NSI>nteger))
  (let ((node nil)
        (selected-node-item (lui-view browser)))
    (dotimes (i column)
      (when (get-selected-row-in-column (lui-view browser) i)
        (setf selected-node-item (elt (nodes selected-node-item) (get-selected-row-in-column (lui-view browser) i)))))
    (setf node (elt (nodes selected-node-item) row))
    (cond 
     ((subtypep (type-of node) 'node-item)
      (#/setStringValue: cell (native-string (node-name node)))
      (print (column-limit (lui-view browser)))
      (when (and (column-limit (lui-view browser)) (>= (+  1 column)(column-limit (lui-view browser)) ))
        (#/setLeaf: cell #$YES)))
     (t 
      (#/setStringValue: cell (native-string node))
      (#/setLeaf: cell #$YES)))))


(defclass NATIVE-BROWSER-VIEW (ns:ns-browser)
  ((lui-view :accessor lui-view :initform nil :initarg :lui-view))
  (:metaclass ns:+ns-object
	      :documentation ""))

;**********************************
;* BROWSER-VIEW                   *
;**********************************

(defmethod make-native-object ((Self browser-view))
  (let ((Native-Control (make-instance 'native-browser-view :lui-view Self)))
    (ns:with-ns-rect (Frame (x self) (y Self) (width Self) (height Self))
      (#/initWithFrame: Native-Control Frame)
      (#/setMaxVisibleColumns: Native-Control 3)
      (#/setDelegate: native-control (make-instance 'my-browser-delegate :lui-view self))
      (#/setTitled: native-control #$NO)
      (#/takesTitleFromPreviousColumn native-control)
    Native-Control)))


(defmethod GET-SELECTED-NODE-IN-COLUMN ((self browser-view) column)
  (let ((selected-node-item self))
    (dotimes (i column)
      (when (get-selected-row-in-column self i)
        (setf selected-node-item (elt (nodes selected-node-item) (get-selected-row-in-column self i)))))
    selected-node-item))


(defmethod GET-VALUE-OF-SELECTED-CELL ((Self browser-view))
  (unless (%null-ptr-p (#/selectedCell (native-view self)))
    (ccl::lisp-string-from-nsstring (#/objectValue (#/selectedCell (native-view self))))))


(defmethod GET-VALUE-OF-SELECTED-CELL-AT-COLUMN ((Self browser-view) column)
  (ccl::lisp-string-from-nsstring (#/objectValue (#/selectedCellInColumn: (native-view self) column))))


(defmethod GET-SELECTED-COLUMN ((Self browser-view))
  (#/selectedColumn (native-view self)))


(defmethod GET-SELECTED-ROW-IN-COLUMN ((Self browser-view) row)
  (#/selectedRowInColumn: (native-view self) row))

(defmethod SELECT-ROW-IN-COLUMN ((Self browser-view) row col)
  (#/selectRow:inColumn: (native-view self) row col))


(defmethod SET-TITLE-OF-COLUMN ((self browser-view) column title)
  (#/setTitled: (native-view self) #$YES)
  (#/setTitle:ofColumn: (native-view self) (native-string title) column)
  (#/setNeedsDisplay: (native-view self) #$YES)
  #-cocotron
  (#/displayAllColumns (native-view self))
  )



(defmethod MAP-SUBVIEWS ((Self browser-view) Function &rest Args)
  (declare (ignore Function Args))
  ;; no Cocoa digging
  )


(defmethod SUBVIEWS ((Self browser-view))
  ;; no Cocoa digging
  )
