(in-package :lui)
(export '(table-view add-column add-row text-changed-action-target text-changed-action SHOULD-END-EDITING-NOTIFICAITON reload-data set-color-of-cell-at-row-column))

;**********************************
;* Table-View                     *
;**********************************

(defclass TABLE-VIEW (control)
  ((rows-in-table :accessor rows-in-table :initform 0 :initarg :rows-in-table :documentation "The number of rows in the table, this will usually be the length of the longest column")
   (columns :accessor columns :initform '() :documentation "A list of columns for this table view")
   (text-changed-action-target :accessor text-changed-action-target :initform nil :initarg :text-changed-action-target :documentation "An owner can be associated with this object and if so, it will be notifed when this objects value-text-field is editted.  In order for this to work, you will need to an attribute-changed-action.")
   (text-changed-action :accessor text-changed-action :initform nil :initarg :text-changed-action :documentation "The action that should be called when the attribute's value has been changed" ))
  (:documentation "A view that can be used for displaying and organizing data"))


(defgeneric ADD-COLUMN (table-view &key editable identifier)
  (:documentation "Add a columns to this table view"))


(defgeneric ADD-ROW (table-view &key data-list)
  (:documentation "Adds a row to the table-view"))
  

(defgeneric SHOULD-END-EDITING-NOTIFCAITON (table-view)
  (:documentation "notification that this table-view has ended editing"))


(defmethod SHOULD-END-EDITING-NOTIFICAITON ((self table-view))
  ;; do nothing
  )


#| Example:

(defparameter *window*
  (make-instance 'window))
(defparameter table (make-instance 'table-view :width 300 :height 300))
(add-subview *window* table)
(add-column table)
(add-column table)
(add-row table :data-list '( 22))
(add-row table :data-list '( 22))

|#