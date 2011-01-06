(in-package :lui)

;**********************************
;* BROWSER-CELL-PROTOTYPE         *
;**********************************
(export '(add-column ))
#|
(defclass BROWSER-CELL-PROTOTYPE (ns:ns-cell)
  ((part-of :accessor part-of :initform nil :initarg :part-of))
  (:metaclass ns:+ns-object
             :documentation "prototype for the ns-cells of an NSBrowser, mostly used to provide actions when a cell is selected"))


(defmethod INITIALIZE-INSTANCE :after ((self browser-cell-prototype) &rest Args)
  (#/sendActionOn: self #$NSLeftMouseDownMask)
  (#/setTarget: self (make-instance 'native-target 
                       :native-control (native-view (part-of Self))
                       :lui-control (part-of Self)))
  (#/setAction: self (objc::@selector #/activateAction)))


(objc:defmethod (#/activateAction :void) ((self browser-cell-prototype))
  (print "ACTIVATE ACITON"))
|#

;**********************************
;* MY-BROWSER-DELEGATE            *
;**********************************

(defclass MY-BROWSER-DELEGATE (ns:ns-object)
  ((lui-view :accessor lui-view :initarg :lui-view))
  (:metaclass ns:+ns-object
	      :documentation "delegate object receiving window events"))


(objc:defmethod (#/browser:numberOfRowsInColumn: :<NSI>nteger) ((self my-browser-delegate) browser (column :<NSI>nteger))
  (#/setTitle:ofColumn: browser (native-string "HELLO") column)
  (if (equal column 0) 
    (length (nodes (lui-view browser)))
    (length (nodes (find (ccl::lisp-string-from-nsstring (#/stringValue (#/selectedCellInColumn: browser (- column 1)))) (nodes (lui-view browser)) :key 'name :test 'equal)))))


#|
(objc:defmethod (#/browser:titleOfColumn: :id) ((self my-browser-delegate) sender (column :<NSI>nteger))
  (print "TITLE OF COLUMN")
  (print sender)
  (print (#/hasHorizontalScroller sender))
  (print (#/isTitled sender))
  (native-string "TITLE"))
|#


(objc:defmethod (#/browser:willDisplayCell:atRow:column: :void) ((self my-browser-delegate) browser cell (row :<NSI>nteger) (column :<NSI>nteger))
  (let ((node nil))
    (if (equal column 0)
      (setf node (elt (nodes (lui-view browser)) row))
      (setf node (elt (nodes (find (ccl::lisp-string-from-nsstring (#/stringValue (#/selectedCellInColumn: browser (- column 1)))) (nodes (lui-view browser)) :key 'name :test 'equal)) row)))
    (cond 
     ((subtypep (type-of node) 'node-item)
      (#/setStringValue: cell (native-string (name node)))
      (unless (nodes node)
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
      (#/setMaxVisibleColumns: Native-Control 2)
      (#/setDelegate: native-control (make-instance 'my-browser-delegate))
      (#/setTitled: native-control #$YES)
      ;(#/setHasHorizontalScroller: Native-Control #$YES)
      (#/takesTitleFromPreviousColumn native-control)
    Native-Control)))


(defmethod GET-VALUE-OF-SELECTED-CELL ((Self browser-view))
  (unless (%null-ptr-p (#/selectedCell (native-view self)))
    (ccl::lisp-string-from-nsstring (#/objectValue (#/selectedCell (native-view self))))))


(defmethod GET-VALUE-OF-SELECTED-CELL-AT-COLUMN ((Self browser-view) column)
  (ccl::lisp-string-from-nsstring (#/objectValue (#/selectedCellInColumn: (native-view self) column))))


(defmethod GET-SELECTED-COLUMN ((Self browser-view))
  (#/selectedColumn (native-view self)))


(defmethod SET-TITLE-OF-COLUMN ((self browser-view) column title)
  (#/setTitled: (native-view self) #$YES)
  (#/setTitle:ofColumn: (native-view self) (native-string title) column)
  (#/setNeedsDisplay: (native-view self) #$YES)
  (#/displayAllColumns (native-view self)))


(defmethod MAP-SUBVIEWS ((Self browser-view) Function &rest Args)
  (declare (ignore Function Args))
  ;; no Cocoa digging
  )


(defmethod SUBVIEWS ((Self browser-view))
  ;; no Cocoa digging
  )
