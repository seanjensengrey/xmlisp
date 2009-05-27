
(in-package :lui)


(defclass toolbar-delegate (ns:ns-object)
  ((lui-view :accessor lui-view :initform nil :initarg :lui-view))
  (:metaclass ns:+ns-object
	      :documentation "the native NSView associated with the LUI view"))







(defparameter *w* (make-instance 'window))

(#/toolbar (native-window *w*))

(defparameter *tb* (#/initWithIdentifier: (#/alloc ns:ns-toolbar) #@"world"))

(#/setAllowsUserCustomization: *tb* #$YES)

(#/items *tb*)

(#/setDelegate: *TB* (#/alloc toolbar-delegate))

(defparameter *tbi* (#/initWithItemIdentifier: (#/alloc ns:ns-toolbar-item) #@"1stperson"))

(#/setLabel: *tbi* #@"warp speed")

(#/setImage: *tbi* (native-view (make-instance 'image-control :src "red-leaves.jpg")))


(#/setToolbar: (native-window *w*) *tb*)

(#/insertItemWithItemIdentifier:atIndex: *tb* #@"warp speed" 0)


#$SearchDocToolbarItemIdentifier

