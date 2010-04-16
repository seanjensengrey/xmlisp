;;-*- Mode: Lisp; Package: XLUI -*-
;*********************************************************************
;*                                                                   *
;*                  C O N T R O L S                                  *
;*                                                                   *
;*********************************************************************
   ;* Author    : Alexander Repenning (alexander@agentsheets.com)    *
   ;*             http://www.agentsheets.com                         *
   ;* Copyright : (c) 1996-2008, AgentSheets Inc.                    *
   ;* Filename  : controls.lisp                                      *
   ;* Updated   : 11/18/08                                           *
   ;* Version   :                                                    *
   ;*    1.0    : 07/20/06 MCL only version                          *
   ;*    1.0.1  : 08/23/06 pop-up                                    *
   ;*    1.0.2  : 08/25/06 OK button is default                      *
   ;*    1.0.3  : 09/14/06 use built-in static-text-dialog-item      *
   ;*    1.1    : 07/19/07 tight label                               *
   ;*    1.1.1  : 08/14/07 no text symbol completion, setable slider *
   ;*    2.0    : 11/18/08 XLUI                                      *
   ;*    2.1    : 03/23/09 scroll-box                                *
   ;* HW/SW     : G4, OS X 10.5.5, MCL 5.2, CCL 1.2                  *
   ;* Abstract  : XMLisp GUI components wrapped up as classes        *
   ;* Portable  : Gray: Read macros                                  *
   ;*                                                                *
   ;******************************************************************

(in-package :XLUI)

;;***********************************************
;;*    Views                                    *
;;***********************************************

;________________________________________________
; Rectangle                                      |
;   colored rectangle                            |fcolor
;   Examples: color picker                       |
;                                                |
;  <rectangle color="FF0000"/>                   |
;________________________________________________

(defclass RECTANGLE (rectangle-view xml-layout-interface)
  ((color :accessor color :initform "000000" :type string :documentation "hex RGB value, e.g. 'FF0000' is red"))
  (:documentation "A colored rectangle"))


(defmethod PRINT-SLOTS ((Self rectangle))
  '(x y width height color))


(defmethod INITIALIZE-INSTANCE ((Self rectangle) &rest Args)
  (declare (ignore Args))
  (call-next-method)
  (let ((Color (read-from-string (format nil "#x~A" (color Self)))))
    (set-color Self 
               :red (/ (logand (ash Color -16) 255) 255.0)
               :green (/ (logand (ash Color -8) 255) 255.0)
               :blue (/ (logand Color 255) 255.0))))


;________________________________________________
; Scroll-Box                                     |
;   scrollable view                              |
;   Examples: color picker                       |
;                                                |
;                                                |
;________________________________________________

(defclass SCROLL-BOX (scroll-view xml-layout-interface)
  ()
  (:documentation "box with scrollable content"))


(defmethod PRINT-SLOTS ((Self scroll-box))
  '(x y width height))

;________________________________________________
; Scroll-Box                                     |
;   scrollable view                              |
;   Examples: color picker                       |
;                                                |
;                                                |
;________________________________________________

(defclass BADGED-IMAGE-GROUP-LIST-MANAGER (badged-image-group-list-manager-view xml-layout-interface)
  ()
  (:documentation "box with scrollable content"))


(defclass IMAGE-BADGE-GROUP-LIST-ITEM (xml-serializer)
  (;(text :accessor text :initform "untitled")
   ;(action :accessor action :initform 'default-action  :type symbol :documentation "method: window dialog")
   (name :accessor name :initarg :name :initform "untitled")
   (image-name :accessor image-name :initarg :image-name :initform "redlobster.png")
   (item-list :accessor item-list :initarg :item-list :initform nil);'(("shape3" "redlobster.png")( "shape4" "redlobster.png")))
   )
  (:documentation "a pop up menu item"))


(defmethod ADD-SUBOBJECT ((manager badged-image-group-list-manager) (item image-badge-group-list-item))
  (add-group manager `(,(name item) ,(image-name item) ,(item-list item)) ))
 ; (add-group manager '( "hello" "redlobster.png"  (("shape3" "redlobster.png")( "shape4" "redlobster.png")))))


(defmethod PRINT-SLOTS ((Self badged-image-group-list-manager))
  '(x y width height))


(defclass AGENT-GALLERY (agent-gallery-view xml-layout-interface)
  ()
  (:documentation "box with scrollable content"))


(defmethod ADD-SUBOBJECT ((manager agent-gallery) (item image-badge-group-list-item))
  (add-group manager `(,(name item) ,(image-name item) ,(item-list item)) ))
 ; (add-group manager '( "hello" "redlobster.png"  (("shape3" "redlobster.png")( "shape4" "redlobster.png")))))


(defmethod PRINT-SLOTS ((Self agent-gallery))
  '(x y width height))


;;***********************************************
;;*    Images                                   *
;;***********************************************

(defclass IMG (image-control xml-layout-interface)
  ((alt :accessor alt :initform "" :type string :documentation "HTML img alt attribute: use for tooltips"))
  (:documentation "Image in HTML style"))


;;***********************************************
;;*    Buttons                                  *
;;***********************************************

;________________________________________________
; Button                                         |
;   Regular full size button: constant height    |
;   Examples: OK, Cancel buttons                 |
;                                                |
;  <button text="Stop Global Warming"/>          |
;________________________________________________

(defclass BUTTON (button-control xml-layout-interface)
  ()
  (:default-initargs )
  (:documentation "Regular, full size button"))

;________________________________________________
; Bevel Button                                   |
;   Small square looking button                  |
;   Can assume any size                          |
;   Typical use: toolbars                        |
;                                                |
;  <bevel-button text="Stop Global Warming"/>    |
;________________________________________________

(defclass BEVEL-BUTTON (bevel-button-control xml-layout-interface)
  ()
  (:default-initargs )
  (:documentation "Toolbar, adjustable height & widht button"))



;_______________________________________________
;  Image Button                                 |
;                                               |
;    <image-button image="redo-button.png"/>    |
;_______________________________________________|


(defclass IMAGE-BUTTON (image-button-control xml-layout-interface)
  ()
  (:default-initargs
      :width 22
    :height 22)
  (:documentation "Compact button containing image"))


(defmethod FINISHED-READING ((Self image-button) Stream)
  ;do nothing
(declare (ignore Stream)))

;______________________________________________________________________________________________________
; PopUp Button                                                                                        |
;                                                                                                     |
; <application-window title="resizable button and window">                                            |
;  <column  align="center" valign="middle" flex="3">                                                  |
;    <pop-up width="200" >                                                                            |
;      <pop-up-item text="on ground" action="test" />                                                 |
;      <pop-up-item text="upright" action="test2"/>                                                   | 
;      <pop-up-item text="wrap around cube" action="test3"/>                                          |
;    </pop-up>                                                                                        | 
;  </column>                                                                                          |
;</application-window>                                                                                |
;______________________________________________________________________________________________________

(defclass POP-UP (popup-button-control xml-layout-interface)
  ()
  (:default-initargs
      :width 20
    :height 25)
  (:documentation "Compact button containing image"))


(defclass POP-UP-ITEM (xml-serializer)
  ((text :accessor text :initform "untitled")
   (action :accessor action :initform 'default-action  :type symbol :documentation "method: window dialog"))
  (:documentation "a pop up menu item"))


(defmethod ADD-SUBOBJECT ((Button popup-button-control) (Item pop-up-item))
  (add-item Button (text Item) (action Item)))


(defmethod DEFAULT-ACTION ((window window) (self pop-up))
  (declare (ignore window))
  (declare (ignore self)))


;___________________________________________________________________________________________________________________________________
; Choice Image Button                                                                                                               |
;    <choice-image-button width="180">                                                                                              |
;      <choice-button-item text="no mirror" image="mirror-none-button.png" action="mirror-none-action"/>                            |
;      <choice-button-item text="mirror horizontally" image="mirror-horizontally-button.png" action="mirror-horizontally-action"/>  |
;      <choice-button-item text="mirror vertically" image="mirror-vertically-button.png" action="mirror-vertically-action"/>        |
;      <choice-button-item text="mirror both" image="mirror-both-button.png" action="mirror-both-action"/>                          |
;    </choice-image-button>                                                                                                         |
;___________________________________________________________________________________________________________________________________

(defclass CHOICE-IMAGE-BUTTON (choice-button-control xml-layout-interface)
  ()
  (:default-initargs
      :width 20
    :height 25)
  (:documentation "Compact button containing image"))


(defmethod PRINT-SLOTS ((Self choice-image-button))
  '(x y width height))


(defclass CHOICE-BUTTON-ITEM (xml-serializer)
  ((text :accessor text :initform "untitled")
   (image :accessor image :initform  nil)
   (action :accessor action :initform 'print-window-and-dialog-action  :type symbol :documentation "method: window dialog"))
  (:documentation "a pop up menu item"))


(defmethod PRINT-SLOTS ((Self choice-button-item))
  '(x y width height))


(defmethod ADD-SUBOBJECT ((Button choice-button-control) (Item choice-button-item))
  (add-menu-item Button (text Item) (action Item) (image Item)))



;__________________________________________________________________________________________________
; Image Button Cluster                                                                             |
;                                                                                                  |
;      <image-button-segment>                                                                      |
;        <image-button image="draw-button.png" action="draw-tool-action" />                        | 
;        <image-button image="erase-button.png" action="erase-tool-action" />                      |
;      </image-button-cluster>                                                                     |
;__________________________________________________________________________________________________


(defclass IMAGE-BUTTON-CLUSTER (column) 
  ((selected-button :accessor selected-button :initform nil :documentation "currently selected button")
   (images :accessor images :initform ())
   )
  (:default-initargs
    :padding -1
    :action 'default-action
    :minimize :box)
  (:documentation "a column of image-buttons. Pressing a button will select it and unselect all others in same segment."))


(defmethod CHANGE-CLUSTER-SELECTIONS ((Self image-button-cluster) button)
  (dolist (image-button (images self))
    (unless (eql image-button button)
      (set-button-off image-button)
      )))


(defmethod DEFAULT-ACTION ((Window window) (Self image-button-cluster)))

(defmethod ADD-SUBOBJECT ((cluster image-button-cluster) (button image-button))
  (call-next-method)
  (setf (in-cluster button) 'T)
  (setf (container button) cluster)
  (setf (user-action button) (action button))
  (setf (action button) 'cluster-action)
  (initialize-event-handling button)
  (setf (images cluster) (append (images cluster) (list button))))

;__________________________________________________________________________________________________
; Image Button Cluster                                                                             |
;                                                                                                  |
;      <image-button-segment>                                                                      |
;        <image-button image="draw-button.png" action="draw-tool-action" />                        | 
;        <image-button image="erase-button.png" action="erase-tool-action" />                      |
;      </image-button-cluster>                                                                     |
;__________________________________________________________________________________________________


(defclass IMAGE-BUTTON-ROW (row) 
  ((selected-button :accessor selected-button :initform nil :documentation "currently selected button")
   (images :accessor images :initform ())
   )
  (:default-initargs
    :padding -1
    :action 'default-action
    :minimize :box)
  (:documentation "a row of image-buttons. Pressing button will select it and unselect all others in same segment."))


(defmethod CHANGE-CLUSTER-SELECTIONS ((Self image-button-row) button)
  (dolist (image-button (images self))
    (unless (eql image-button button)
      (set-button-off image-button)
      )))


(defmethod DEFAULT-ACTION ((Window window) (Self image-button-row)))


(defmethod ADD-SUBOBJECT ((cluster image-button-row) (button image-button))
  (call-next-method)
  (setf (in-cluster button) 'T)
  (setf (container button) cluster)
  (setf (user-action button) (action button))
  (setf (action button) 'cluster-action)
  (initialize-event-handling button)
  (setf (images cluster) (append (images cluster) (list button))))


;_____________________________________________________________________
; Radio Button Cluster                                                |
;                                                                     |
;<application-window title="resizable button and window">             |
;  <column  align="left" valign="center" flex="3">                    |
;    <radio-button-cluster width="300" height="100">                  |
;      <radio-item text="text1" action="a1"/>                         |
;      <radio-item text="text2" action="a1"/>                         |
;      <radio-item text="text3" action="a1"/>                         |
;    </radio-button-cluster>                                          |
;  </column>                                                          |
;</application-window>                                                |
;                                                                     |
;_____________________________________________________________________


(defclass RADIO-BUTTON-CLUSTER (radio-button-control xml-layout-interface)
  ()
  (:default-initargs
      :width 40
    :height 25)
  (:documentation "contains radio buttons"))


(defmethod INITIALIZE-INSTANCE :after ((Self radio-button-cluster) &rest Args)
  (declare  (ignore Args)))


(defclass RADIO-ITEM (xml-serializer)
  ((text :accessor text :initform "option")
   (action :accessor action :initform 'print-window-and-dialog-action  :type symbol :documentation "method: window dialog"))
  (:documentation "a radio button item"))


(defmethod FINISHED-READING ((Self radio-button-cluster) Stream)
  (declare (ignore Stream))
  (finalize-cluster Self))


(defmethod ADD-SUBOBJECT ((radio-control radio-button-cluster) (Item radio-item))
  (add-item radio-control (text Item) (action Item)))


;____________________________________________
; Color Well                                 |
;                                            |
; <color-well action="color-well-action"/>   |
;____________________________________________

(defclass COLOR-WELL (color-well-control xml-layout-interface)
  ()
  (:default-initargs
      :action 'default-action
    :width 22
    :height 22)
  (:documentation "Color Well"))


(defmethod INITIALIZE-INSTANCE :after ((Self color-well) &rest Args)
  ;; need to invoke the dialog action without color
  (declare (ignore Args)))


(defmethod PRINT-SLOTS ((Self color-well-control))
  '(color x y width height))


(defmethod color-action ((window window) (self COLOR-WELL-Control))
  (declare (ignore self)))


;______________________________________________
; Slider                                       |
;                                              |
; <slider action="adjust-pressure-action"/>    |
;______________________________________________

(defclass SLIDER (slider-control xml-layout-interface)
  ()
  (:documentation "Slider: adjust continuous value"))


(defmethod PRINT-SLOTS ((Self slider))
  '(max-value min-value tick-marks x y width height))
;__________________________________________________________________
; Tab-View                                                         |
;                                                                  |
;                                                                  |
;__________________________________________________________________

(defclass TAB-VIEW (tab-view-control xml-layout-interface)
  (title :accessor )
  (:default-initargs
    :width 100 
    :height 15))


(defmethod PRINT-SLOTS ((self tab-view-control))
  '(x y width height))

(defclass TAB-ITEM (xml-serializer view)
  ((text :accessor text :initform "option")
   )
  (:documentation "a radio button item"))

(defmethod ADD-SUBOBJECT ((tab-view-control tab-view) (Item tab-item))
  (add-tab-view-item tab-view-control (text Item) ))

(defclass TAB-VIEW-ITEM (tab-view-item-control xml-layout-interface)
  (title :accessor )
  (:default-initargs
    :width 100 
    :height 15))

(defmethod ADD-SUBOBJECT ((tab-view-control tab-view) (Item tab-view-item))
  (add-tab-view-item tab-view-control (text Item) ))



(defmethod ADD-SUBOBJECT ((tab-view-item tab-view-item) (tab-view view))
  (add-tab-view-item-view tab-view-item  tab-view) )

;__________________________________________________________________
; Check Box                                                        |
;                                                                  |
; <check-box text="important" action="snap-sound" width="90"/>     |
;__________________________________________________________________


(defclass CHECK-BOX (checkbox-control xml-layout-interface)
  (title :accessor )
  (:default-initargs
    :width 100 
    :height 15))


;__________________________________________________________________
; Check Box                                                        |
;                                                                  |
; <check-box text="important" action="snap-sound" width="90"/>     |
;__________________________________________________________________


(defclass  SCROLLER (scroller-control xml-layout-interface)
  (title :accessor )
  (:default-initargs
    :width 15 
    :height 150))


;__________________________________________________________________
; Editable-Text                                                    |
;                                                                  |
; <editable-text text="bla"/>                                      |
;__________________________________________________________________


(defclass EDITABLE-TEXT (editable-text-control xml-layout-interface)
  ((action :accessor action :initform 'print-window-and-dialog-action :type layout-value :initarg :action :documentation "method: window dialog"))
  (:default-initargs
    :width 100 
    :height 20)
  (:documentation "Editable Text"))


(defmethod PRINT-SLOTS ((Self editable-text))
  '(x y width height))


(defmethod SYMBOL-COMPLETION-ENABLED-P ((Self editable-text))
  ;; not a good idea for end users
  nil)
  

;__________________________________________________________________
; Status Bar                                                       |
;                                                                  |
; <status-bar text="bla"/>                                         |
;__________________________________________________________________


(defclass STATUS-BAR (status-bar-control xml-layout-interface)
  ((action :accessor action :initform 'print-window-and-dialog-action :type layout-value :initarg :action :documentation "method: window dialog"))
  (:default-initargs
    :width 100 
    :height 20)
  (:documentation "Editable Text"))


(defmethod PRINT-SLOTS ((Self status-bar))
  '(x y width height))


(defmethod SYMBOL-COMPLETION-ENABLED-P ((Self status-bar))
  ;; not a good idea for end users
  nil)


;__________________________________________________________________
; Editable-Number                                                  |
;                                                                  |
; <editable-number text="3.0"/>                                    |
; contains valid numbers: get/set numbers                          |
; invalid numbers or empty text fields return nil                  |
;__________________________________________________________________

(defclass EDITABLE-NUMBER (editable-text)
  ()
  (:default-initargs
    :width 100 
    :height 20)
  (:documentation "Editable Number"))


(defmethod VALUE ((Self editable-number))
  (let ((String-Value (call-next-method)))
    (when String-Value
      (let ((Value (read-from-string String-Value)))
        (when (numberp Value)
          Value)))))


(defmethod (setf VALUE)  (Number (Self editable-number))
  (when (numberp Number)
    (call-next-method (write-to-string Number) Self)))

;__________________________________________________________________
; Spacer                                                           |
;                                                                  |
; <spacer width="90"/>                                             |
;__________________________________________________________________


(defclass SPACER (control xml-layout-interface)
  ()
  (:default-initargs
    :width 20
    :height 20)
  (:documentation "invisible component to make spaces"))


(defmethod initialize-event-handling ((Self spacer))
  ;; no event handling 
  )

(defclass SMALL-SPACER (spacer)
  ()
  (:default-initargs
    :width 8
    :height 8)
  (:documentation "small spacer, e.g., between text and dialog item"))



;__________________________________________________________________
; Separator                                                        |
;                                                                  |
; <separator width="200"/>                                         |
;__________________________________________________________________

(defclass SEPARATOR (SEPERATOR-CONTROL xml-layout-interface)
  ()
  (:default-initargs 
    :width 1
    :height 1)
  (:documentation "separator line. If separator is wider than it is tall, the separator line is horizontal; otherwise it is vertical"))




;__________________________________________________________________
; Label                                                            |
;                                                                  |
; <label width="65" text="Pressure" align="right"/>                |
;__________________________________________________________________


(defclass LABEL (label-control xml-layout-interface)
  ()
  (:default-initargs
    :width 10
    :height 14)
  (:documentation "Static text"))

;__________________________________________________________________
; Label                                                            |
;                                                                  |
; <tight-label text="Pressure"/>                                   |
;__________________________________________________________________

(defclass TIGHT-LABEL (label)
  ()
  (:documentation "Static text automatically sized with tight border"))


(defmethod FINISHED-READING :after ((Self tight-label) Stream)
  (setf (width Self) (string-width (dialog-item-text Self) (view-font Self)))
  (set-view-size Self (width Self) (point-v (view-size Self))))
    


  
;;***********************************************
;;*    Web Browser                              *
;;***********************************************

(defclass WEB-BROWSER (web-browser-control xml-layout-interface)
  ()
  (:documentation "Web Browser"))


(defmethod PRINT-SLOTS ((Self web-browser))
  '(url x y width height))

;;***********************************************
;;*    3D   OpenGL                              *
;;***********************************************

;________________________________________________
; OpenGL Dialog                                  |
;________________________________________________

(defclass OPENGL-DIALOG (opengl-view xml-layout-interface)
  ())


(defmethod PRINT-SLOTS ((Self opengl-dialog))
  ;; cannot have any view-subviews
  '(x y width height))




#| Examples: STILL FROM MCL NOW WORKING YET !!


(defparameter *Inflation-Editor* (make-instance 'inflatable-icon-editor-window))

(setf (root-view *Inflation-Editor*)
<column align="middle" valign="center">
  <check-box text="important" action="snap-sound" width="90"/>
  <check-box text="or not sfsdfsdf" action="snap-sound" width="130"/>
</column>)



(setf (root-view *Inflation-Editor*)
<column align="middle" row="center">
  <radio-button-cluster minimize="vertical" size="200 300">         
    <radio-button text="front" radio-button-pushed-p="true" action="snap-sound"/>
    <radio-button text="front &amp; back" action="snap-sound"/>               
    <radio-button text="front &amp; back, connected" action="snap-sound"/>    
  </radio-button-cluster>  
</column>)


;; editable text: does not render well

(setf (root-view *Inflation-Editor*)
<column align="middle" valign="center" padding="10" width="200">
  <editable-text text="Edit me" width="200" size="200 16"/>
  <editable-text text="Edit me" width="200" size="200 16" height="20"/>
</column>)




(defparameter *w2* (make-instance 'application-window))



  <row-of-squares>
    <icon-editor img-height="32" img-width="32"/>
    <opengl-dialog/>
  </row-of-squares>
</column>)


(setf (selected-tool *w2*) 'draw)


(setf (root-view *w2*)
<image-editor img-height="32" img-width="32"/>)


(setf (root-view *w2*)
<radio-button-cluster>
  <radio-button text="me"/>
  <radio-button text="no, me!"/>
</radio-button-cluster>)


(setf (root-view *w2*)


<application-window>
  <row align="right" valign="bottom" padding="20">
    <button text="Maybe"/>
    <cancel-button/>
    <ok-button/>
  </row>
</application-window>


<application-window>
  <row align="right" padding="10">
    <button text="a" size="80 80" />
    <button text="b" size="80 120"/>
    <button text="c" size="200 40"/>
  </row>
</application-window>


(setf (root-view *w2*)
<row align="middle" valign="middle" padding="10">
  <button text="a" size="80 80"/>
  <button text="b" size="80 120"/>
  <button text="c" size="200 40"/>
</row>)


(setf (root-view *w2*)
<row valign="bottom" padding="10">
  <button text="a" size="80 80"/>
  <button text="b" size="80 120"/>
  <button text="c" size="200 40"/>
</row>)

(setf (root-view *w2*)
<row align="stretch">
  <row align="distribute" valign="middle" padding="10" minimize="vertical">
    <button text="a" size="80 80"/>
    <button text="b" size="80 120"/>
    <button text="c" size="200 40"/>
  </row>
  <row align="stretch" valign="middle" padding="10" minimize="vertical">
    <button text="a" size="80 80"/>
    <button text="b" size="80 120"/>
    <button text="c" size="200 40"/>
  </row>
</row>)


(setf (root-view *w2*)
<row align="stretch"  valign="middle" padding="6">
  <button text="a" size="80 80" width="80"/>
  <button text="b" size="80 120"/>
  <button text="c" size="200 40" width="40"/>
</row>)

;; column

(setf (root-view *w2*)
<column align="stretch" valign="center" padding="10">
  <button text="a" size="80 80"/>
  <slider value="500"/>
  <slider/>
  <button text="b" size="80 120"/>
  <slider/>
  <button text="c" size="200 40"/>
</column>)


;; Inflatable icon editor

(defmethod ADJUST-PRESSURE-ACTION ((Window application-window) (Slider slider))
  (let ((Value (+ -1.0 (/ (* 2.0 (get-slider-setting Slider)) 1000))))
    (let ((Text-View (view-named 'pressuretext Window)))
      (set-dialog-item-text Text-View (format nil "~4,2F" Value))
      (view-draw-contents Text-View))))




(setf (root-view *w2*)
<column align="stretch" valign="distribute" padding="10">
  <row size="500 20">
    <image-button image="draw-button.png"/>
    <image-button image="paint-bucket-button.png"/>
    <image-button image="erase-button.png"/>
    <label text="speed" width="60" align="right"/><slider size="100 16"/>
  </row>
  <row-of-squares>
    <icon-editor img-height="32" img-width="32"/>
    <opengl-dialog/>
  </row-of-squares>
  <row align="stretch" minimize="vertical" padding="10">
    <column align="stretch" valign="top" size="100 120">
      <row align="stretch">
        <label width="65" text="Pressure"/>
        <slider action="adjust-pressure-action"/>
        <label name="pressuretext" align="right" width="35" text="0.0"/>
     </row>
      <row align="stretch"><label width="65" text="Ceilling"/><slider/><label align="right" width="35" text="0.0"/></row>
      <row align="stretch"><label width="65" text="Noise"/><slider/><label align="right" width="35" text="0.0"/></row>
      <separator size="200 24"/>
      <row><label text="orientation:" width="65"/><check-box/><label width="60" text="upright"/></row>
      <row><spacer size="65 10"/><check-box/><label width="60" text="mirrored"/></row>
    </column>
    <separator width="12" size="12 120"/>
    <column align="stretch" valign="top" size="100 180">
      <row align="stretch"><label width="65" text="Smooting"/><slider /><label align="right" width="25" text="0.0"/></row>
      <row align="stretch"><label width="65" text="Distance"/><slider /><label align="right" width="25" text="0.0"/></row>
      <row align="stretch"><label width="55" text="x" align="right"/><spacer width="10"/><slider /><label align="right" width="25" text="0.0"/></row>
      <separator size="200 24"/>
      <radio-button-cluster minimize="vertical" size="200 300">
        <radio-button text="front" radio-button-pushed-p="true" />
        <radio-button text="front &amp; back"/>
        <radio-button text="front &amp; back, connected"/>
      </radio-button-cluster>
    </column>
  </row>
  <row align="right" valign="bottom" padding="20" size="50 50">
    <cancel-button/>
    <ok-button/>
  </row>
</column>)



;; Quiz - Two radio button clusters: need to be independent

(setf (root-view *w2*)
<column align="left" valign="middle">
  <row align="left" valign="middle" size="300 30"><label text="addition" width="70"/><separator size="200 12"/></row>
  <radio-button-cluster minimize="vertical">
    <radio-button text="A" radio-button-pushed-p="true" />
    <radio-button text="B"/>
    <radio-button text="C"/>
  </radio-button-cluster>
  <row align="left" valign="middle" size="300 30"><label text="multiplication" width="80"/><separator size="200 12"/></row>
    
    <radio-button-cluster size="200 200">
      <radio-button text="1" radio-button-pushed-p="true" />
      <radio-button text="2"/>
      <radio-button text="3"/>
    </radio-button-cluster>
</column>)



  <row size="200 200">

<label text="12 x 12 =" width="100"/>


  </row>




(setf (root-view *w2*)
  <row-of-squares>
    <opengl-dialog size="50 50"/>
    <opengl-dialog/>
  </row-of-squares>)




(setf (root-view *w2*)
<column>
  <row-of-squares>
    <opengl-dialog/>
    <opengl-dialog/>
  </row-of-squares>
  <row-of-squares>
    <column>
       <check-box dialog-item-text="symetric"/>
       <row><label width="70" text="Pressure:"/><slider/></row>
       <row><label width="70" text="Noise:"/><slider/></row>
       <row><label width="70" text="Softness:"/><slider/></row>
       <slider/>
       <slider/>
       <slider/>
    </column>
    <column>
      <label text="Label:"/>
      <label text="not going to stop"/>
       <slider/>
       <slider/>
    </column>
  </row-of-squares>
</column>)



<column>
  <row-of-squares>
    <image-editor/>
    <inflatable-icon-editor name="inflatable icon"/>
  </row-of-squares>
  <row height="variable">
     <column>
        <check-box symetric="true"/>
        <slider name="Pressure" min="-3.0" max="3.0" action-fn="adjust-pressure-action"/>
     </column>
   </row>
</column>
  

(defmethod AJUST-PRESSURE-ACTION ((Window application-window) (Item xml-layout-interface))
  (set-pressure (view-named 'inflatable-icon Window) (value Item)))





(add-subviews
  *A-window*
  (make-instance 'opengl-view  :view-position #@(100 5)))


(add-subviews 
  *A-Window*
<opengl-dialog view-position="100 5" view-size="300 200"/>)


  <row>
    <opengl-dialog view-position="100 5"/>
  </row> )





(setq *rv* (make-instance 'view))

(add-subviews 
  *rv* 
  (make-instance 'opengl-view
                     :view-position #@(5 5)
                     :view-size #@(100 100))
  (make-instance 'opengl-view
                     :view-position #@(110 5)
                     :view-size #@(150 150)))


(add-subviews *A-Window* *rv*)


(setq od <opengl-dialog/>)

(setf (bla od) (make-point 100 100))



(inspect 
<row>
  <opengl-dialog/>
</row>  )




|#