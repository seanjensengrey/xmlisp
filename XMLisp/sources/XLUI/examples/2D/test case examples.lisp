;; XLUI examples
;; 2/4/09 Alexander Repenning
;; updated: 4/26/09 native-path


(in-package :xlui)

#|
A XMLisp based Lisp User Interface, XLUI for short, consists of up to 3 pieces:

* Class definitions subclassing exisiting XLUI classes
* x-expressions representing window and controls layout
* action methods invoked through LUI interface actions defined by the developer

Below are a number of increasingly complex LUI interface examples. Select the code, including 
the XML expressions (remember they look like XML expression but ARE lisp s-expressions) and 
evaluate them one by one. 


NOT RECOMMENDED: if you load the entire buffer at once then you just get a mess of windows (some modal) on the 
 screen. This will work but you are better off evaluating things selectively.

|#


;;********************************************
;;*    Windows, Buttons and basic layout     *
;;********************************************

<application-window x="100" y="100" width="300" height="176" title="valign='middle'"minimizable="false">
  <row valign="middle">
    <button text="Stop Global Warming" width="200"/>
    <bevel-button text="multiple&#10;lines&#10;of&#10;text" x="50" y="50" height="100"/>
  </row>
</application-window> 





<application-window title="resizable button and window">
  <bevel-button text="this button&#10;will assume window size&#10;when&#10;you resize the window"/>
</application-window>


<application-window title="align left" width="400">
  <row width="200" height="200">
    <bevel-button text="a"/>
    <bevel-button text="b" width="200"/>
    <bevel-button text="c"/>
  </row>
</application-window>


<application-window title="align right">
  <row width="200" height="200" align="right">
    <bevel-button text="a"/>
    <bevel-button text="b"/>
    <bevel-button text="c"/>
  </row>
</application-window>


<application-window height="50" title="align=distribute, valign=middle">
  <row width="200" valign="middle" align="distribute">
    <bevel-button text="a" width="30"/>
    <bevel-button text="b" width="30"/>
    <bevel-button text="c" width="30"/>
    <bevel-button text="d" width="30"/>
    <bevel-button text="e" width="30"/>
    <bevel-button text="f" width="30"/>
    <bevel-button text="g" width="30"/>
    <bevel-button text="h" width="30"/>
  </row>
</application-window>


<application-window title="flex to stretch" width="500" height="100">
  <row width="200" height="200" align="stretch" valign="stretch">
    <bevel-button text="a&#10;fixed, flex = 0" width="150"/>
    <bevel-button text="b&#10;flex = 1" flex="1"/>
    <bevel-button text="c&#10;flex = 3" flex="3"/>
  </row>
</application-window> 


<application-window title="valign middle, button b with flex 1">
  <row width="200" height="200" align="stretch" valign="middle">
    <bevel-button text="a"/>
    <bevel-button text="b" flex="1"/>
    <bevel-button text="c"/>
  </row>
</application-window>


;; Vertical alignment

<application-window margin="0" title="valign=distribute">
  <column width="200" height="200" align="center" valign="distribute"> 
    <bevel-button text="a"/>
    <bevel-button text="b"/>
    <bevel-button text="c"/>
  </column>
</application-window>


<application-window margin="0" title="valign=stretch with button b flex 1">
  <column width="200" height="200" align="center" valign="stretch"> 
    <bevel-button text="a"/>
    <bevel-button text="b" vflex="1"/>
    <bevel-button text="c"/>
  </column>
</application-window>


<application-window title="left, center, distribute and right alignment">
  <column width="200" height="200" align="stretch" valign="distribute">
    <row align="left" minimize="vertical">
      <bevel-button text="a"/>
      <bevel-button text="b"/>
      <bevel-button text="c"/>
    </row>
    <row align="center" minimize="vertical">
      <bevel-button text="a"/>
      <bevel-button text="b"/>
      <bevel-button text="c"/>
    </row>
    <row align="distribute" minimize="vertical">
      <bevel-button text="a"/>
      <bevel-button text="b"/>
      <bevel-button text="c"/>
    </row>
    <row align="right" minimize="vertical">
      <bevel-button text="a"/>
      <bevel-button text="b"/>
      <bevel-button text="c"/>
    </row>
  </column>
</application-window>

;; OK, cancel buttons

<application-window title="dialog">
  <column align="stretch" valign="bottom">
    <row align="right" minimize="vertical">
      <button text="Cancel"/>
      <button text="OK" default-button="true"/>
    </row>
  </column>
</application-window> 


;;********************************************
;;*           Interaction                    *
;;********************************************


;; sliders

<application-window title="sliders">
  <column align="stretch" valign="distribute">
    <slider/>
    <slider tick-marks="50"/>
    <slider tick-marks="5"/>
  </column>
</application-window> 


;; Color picker 1: just layout

<application-window title="color picker">
  <column align="stretch" valign="distribute">
    <row align="stretch" valign="stretch">
      <column align="stretch" flex="2" valign="top">
        <slider name="red" tick-marks="10"/>
        <slider name="green" tick-marks="10"/>
        <slider name="blue" tick-marks="10"/>
      </column>
      <rectangle color="00FF00" flex="1"/>
    </row>
      <row align="right" minimize="vertical">
        <button text="Cancel"/>
        <button text="OK"/>
      </row>
  </column>
</application-window> 


;; color picker 2: with actions

(defclass COLOR-PICKER-WINDOW (application-window)
  ())


(defmethod ADJUST-COLOR-ACTION ((W color-picker-window) (Slider slider))
  (set-color 
   (view-named W "color well")
   :red (value (view-named W "red"))
   :green (value (view-named W "green"))
   :blue (value (view-named W "blue")))
  (display W))


<color-picker-window title="color picker">
  <row align="stretch" valign="stretch">
    <column align="stretch" flex="2" valign="middle">
      <slider name="red" action="adjust-color-action" max-value="1.0" tick-marks="10"/>
      <slider name="green" action="adjust-color-action" max-value="1.0" tick-marks="10"/>
      <slider name="blue" action="adjust-color-action" max-value="1.0" tick-marks="10"/>
    </column>
    <spacer/>
    <rectangle name="color well" flex="1"/>
  </row>
</color-picker-window>


;; Color Picker 3 with labels


(defclass LABLED-COLOR-PICKER-WINDOW (application-window)
  ())


(defmethod ADJUST-COLOR-ACTION ((W labled-color-picker-window) (Slider slider))
  ;; color well update
  (set-color 
   (view-named W "color well")
   :red (value (view-named W "red"))
   :green (value (view-named W "green"))
   :blue (value (view-named W "blue")))
  ;; labels update
  (setf (text (view-named W "red label")) (format nil "~,2F" (value (view-named W "red"))))
  (setf (text (view-named W "green label")) (format nil "~,2F" (value (view-named W "green"))))
  (setf (text (view-named W "blue label")) (format nil "~,2F" (value (view-named W "blue"))))
  (display W))


<labled-color-picker-window title="color picker (resize me!!)" height="130">
  <row align="stretch" valign="stretch">
    <column align="stretch" flex="2" valign="middle">
      <row minimize="vertical" align="stretch">
         <label text="red" align="right" width="50"/>
         <slider name="red" action="adjust-color-action" max-value="1.0" tick-marks="10" flex="1"/>
         <label text="0.0" name="red label" width="40"/>
      </row>
      <row minimize="vertical" align="stretch">
         <label text="green" align="right" width="50"/>
         <slider name="green" action="adjust-color-action" max-value="1.0" tick-marks="10" flex="1"/>
         <label text="0.0" name="green label" width="40"/>
      </row>
      <row minimize="vertical" align="stretch">
         <label text="blue" align="right" width="50"/>
         <slider name="blue" action="adjust-color-action" max-value="1.0" tick-marks="10" flex="1"/>
         <label text="0.0" name="blue label" width="40"/>
      </row>
    </column>
    <spacer/>
    <rectangle name="color well" flex="1"/>
  </row>
</labled-color-picker-window>


;;********************************************
;;*           Text                           *
;;********************************************

<application-window title="some text, resize to see alignment">
  <column align="stretch" valign="distribute">
    <label text="some text on the left" align="left"/>
    <label text="some text on the right" align="right"/>
    <label text="some centered text" align="center"/>
  </column>
</application-window> 


<application-window title="large text: resize to explore wrapping">
  <label align="justified" text="A lawyer died and arrived at the pearly gates. To his dismay, there were thousands of people ahead of him in line to see St. Peter. But, to his surprise, St. Peter left his desk at the gate and came down the long line to where the lawyer was standing. St. Peter greeted him warmly. Then St. Peter and one of his assistants took the lawyer by the hands and guided him up to the front of the line into a comfortable chair by his desk.

The lawyer said, 'I don't mind all this attention, but what makes me so special?'

St. Peter replied, 'Well, I've added up all the hours for which you billed your clients, and by my calculation you must be about 193 years old!'"/>
</application-window> 


;; Editable-Text

(defmethod CONVERT-CURRENCY-ACTION ((w application-window) (Button button))
  (setf (value (view-named w "amount"))
        (* (value (view-named w "rate"))
           (value (view-named w "dollars")))))


<application-window title="Currency Converter" width="300" height="180">
  <column align="stretch" valign="stretch" padding="9">
    <row align="stretch" minimize="vertical" valign="bottom">
     <label text="Exchange Rate per $1:" align="right" flex="2"/>
     <editable-number name="rate" text="1.0" flex="1"/>
    </row>
    <row align="stretch" minimize="vertical" valign="bottom">
     <label text="Dollars to Convert:" align="right" flex="2"/>
     <editable-number name="dollars" text="1.0" flex="1"/>
    </row>
    <row align="stretch" minimize="vertical" valign="bottom">
     <label text="Amount in the other Currency:" align="right" flex="2"/>
     <editable-number  name="amount" text="1.0" flex="1"/>
    </row>
    <row align="right" valign="bottom" vflex="1">
      <button text="Convert" action="convert-currency-action" default-button="true"/>
    </row>
  </column>
</application-window>


;; get string from user

<application-window title="get string" height="100">
  <column align="stretch" valign="distribute">
    <editable-text  name="amount"/>
    <row align="right" minimize="vertical">
      <button text="Cancel"/>
      <button text="OK" default-button="true"/>
    </row>
  </column>
</application-window>

;;********************************************
;;*           Pop ups                        *
;;********************************************

(defmethod POP-UP-IMAGE-MENU-AND-DISPLAY ((Window application-window) (Button button))
  (setf (text (view-named Window "direction"))
        (display-pop-up-menu (make-instance 'direction-pop-up-image-menu))))
                       

<application-window title="DIRECTION-POP-UP-IMAGE-MENU" width="300" height="100">
  <column align="stretch" valign="stretch" padding="9">
    <row align="stretch" minimize="vertical" valign="middle">
      <button text="pop up menu" action="pop-up-image-menu-and-display" width="120" default-button="true"/>
      <label text="<undefined>" name="direction" width="200"/>
    </row>
  </column>
</application-window>

;; Pop up button control

(defparameter *Window* 
 <application-window title="resizable button and window">                                            
  <column  align="center" valign="middle" flex="3">                                                  
    <pop-up width="200" name="pop">                                                                            
      <pop-up-item text="on ground" action="test" /> 
      <pop-up-item text="upright" action="test"/>                                                    
      <pop-up-item text="wrap around cube" action="test"/>                                          
    </pop-up>                                                                    
  </column>                                                                                     
</application-window>)



;;****************************************************
;; IMAGES  (assumed to be in LUI:resources;images;)  *
;;****************************************************

<application-window title="project logo">
  <img src="scalable-game-design.png"/>
</application-window>


;; rows with images containing images
<application-window title="iPhone" margin="0" height="200">
  <column align="stretch" valign="stretch">
    <row src="aqua-gradient.png" align="stretch" valign="middle" height="50" vflex="1">
      <spacer/>
      <img src="green-leaves.jpg" width="32" height="32"/>
      <label text="Tree 1" flex="1"/>
      <button text="more &gt;"/>
      <spacer/>
    </row>
    <row src="aqua-gradient.png" align="stretch" valign="middle" height="50" vflex="1">>
      <spacer/>
      <img src="green-leaves.jpg" width="32" height="32"/>
      <label text="Tree 2: bla bla" flex="1"/>
      <button text="more &gt;"/>
      <spacer/>
    </row>
    <row src="aqua-gradient.png" align="stretch" valign="middle" height="50" vflex="1">>
      <spacer/>
      <img src="green-leaves.jpg" width="32" height="32"/>
      <label text="Tree 3: more bla bla" flex="1"/>
      <button text="more &gt;"/>
      <spacer/>
    </row>
    <row src="aqua-gradient.png" align="stretch" valign="middle" height="50" vflex="1">>
      <spacer/>
      <img src="green-leaves.jpg" width="32" height="32"/>
      <label text="Tree 4: and even more bla bla" flex="1"/>
      <button text="more &gt;"/>
      <spacer/>
    </row>
  </column>
</application-window>


<application-window title="what a mess!! drawing order exploration">
  <label text="notice drawing order: one button is below the other one on top of the image"/>
  <canvas>
    <button text="below" x="100" y="100"/>
  </canvas>
  <column align="stretch" valign="stretch">
    <img src="scalable-game-design.png" flex="1" vflex="1"/>
    <img src="scalable-game-design.png"/>
  </column>
  <canvas>
    <button text="on top" x="100" y="140"/>
  </canvas>
</application-window>

;;********************************************
;;*  String List                             *
;;********************************************

;;Exmaple 1
<application-window title="scrolling" margin="0">
  <string-list action="default-action3" width="50">
    <string-list-item text="ITEM1"/>
    <string-list-item text="ITEM2"/>
    <string-list-item text="ITEM3"/>
    <string-list-item text="ITEM5"/>
    <string-list-item text="ITEM6"/>
    <string-list-item text="ITEM7"/>
  </string-list>
</application-window> 


;;Example 2 The Menu


<application-window title="My Menu" margin="0">
  <row> 
    <column width="100">
      <label align="center" width="100" text="Courses"/>
      <string-list action="course-change-action" width="100">
        <string-list-item text="Salads"/>
        <string-list-item text="Appetizers"/>
        <string-list-item text="Main Courses"/>
        <string-list-item text="Desserts"/>
      </string-list>
    </column>
    <spacer/>
    <column width="200">
      <label align="center" width="100" text="Choices"/>
      <string-list name="column2" action="default-action3" width="200"/>
    </column>
  </row>
</application-window>

<application-window title="scrolling" margin="0">
  <attribute-value-list width="300" >
    <string-list-item text="ITEM1"/>
    <string-list-item text="ITEM2"/>
    <string-list-item text="ITEM3"/>
    <string-list-item text="ITEM5"/>
    <string-list-item text="ITEM6"/>
    <string-list-item text="ITEM7"/>
  </attribute-value-list>
</application-window> 


(defmethod COURSE-CHANGE-ACTION ((window window) (self string-list))
  (cond
   ((equal "Salads" (selected-string self ))
    (set-list (view-named window "column2") (list "Caesar" "Gorgonzola w/ hearts of romaine" "organic mixed greens")))
   ((equal "Appetizers" (selected-string self ))
    (set-list (view-named window "column2") (list "Grilled Pork Shoulder" "Asparagus Soup" "Farm Spinach Custard ")))
   ((equal "Main Courses" (selected-string self ))
    (set-list (view-named window "column2") (list "Riso Superfino Carnaroli" "Russet Potato Gnocchi" "Farm Rabbit Agnolotti")))
   ((equal "Desserts" (selected-string self ))
    (set-list (view-named window "column2") (list "Starwberry-Rhubarb Tart" "Caramel Shake" "Chocolate Torta" "Gelati and Sorbet" "Bombolini")))))

;;********************************************
;;*  Attribute Editor                         *
;;********************************************

<application-window title="scrolling" margin="0">
  <attribute-editor attribute-changed-action="attribute-changed"/>
</application-window> 

(defmethod attribute-changed ((Self window) (window window) )
  (print "ATRIBUTE CHANGED!!!!!!"))

 ;;********************************************
;;*          SCROLLER                        *
;;********************************************


;;Example 1


(defmethod MY-SCROLL-ACTION ((window window) (self scroller-control))
  (let ((button (view-named Window "button")))
    (set-position button 0 (* (value self) (- (height window) 20)))
    (display window)))


<application-window title="scrolling" margin="0">
  <row valign="stretch" align="distribute">
    <column>
      <bevel-button name="button" width="100" height="26" text="SCROLL-ME" vflex="0"/>
    </column>
    <scroller action="my-scroll-action" vflex="1" height="200"/>
  </row>
</application-window> 


;;Example 2

(defmethod GL-SCROLL-ACTION ((window window) (self scroller-control))
  (setf (y-translation (view-named window "opengl")) (* -1 (value self)))
  (display (view-named window "opengl")))


(defclass TRANSLATING-TRIANGLE-OPENGL-DIALOG (opengl-dialog)
  ((y-translation :accessor y-translation :initform 0.0)))


(defmethod DRAW ((Self translating-triangle-opengl-dialog))
  (glTranslatef 0.0 (y-translation self) 0.0)
  (glBegin GL_TRIANGLES)  
    (glVertex3f 0.0 1.1 0.0)  
    (glVertex3f -0.6 0.0 0.0)    
    (glVertex3f 0.6 0.0 0.0)
  (glEnd))


<application-window title="scrolling" margin="0">
  <row valign="stretch" align="distribute">
      <translating-triangle-opengl-dialog name="opengl" vflex="3"/>
    <scroller small-scroller-size="true" action="gl-scroll-action" vflex="1" height="200"/>
  </row>
</application-window> 

;;********************************************
;;*           OpenGL (3D)                    *
;;********************************************


;; resize to see how many nano seconds it takes to draw the obbligatory OpenGL triangle

<application-window title="OpenGL 3D" margin="0">
  <opengl-dialog/>
</application-window>


;; example 2:

(defclass TRIANGLE-OPENGL-DIALOG (opengl-dialog)
  ())


(defmethod DRAW ((Self triangle-opengl-dialog))
  (glBegin GL_TRIANGLES)
    (glColor3f (value (view-named (window Self) "red")) 0.0 0.0)
    (glVertex3f 0.0 0.6 0.0)
    (glColor3f 0.0 (value (view-named (window Self) "green")) 0.0)
    (glVertex3f -0.6 -0.3 0.0) 
    (glColor3f 0.0 0.0 (value (view-named (window Self) "blue")))
    (glVertex3f 0.6 -0.3 0.0)
  (glEnd))


(defmethod ADJUST-VERTEX-COLOR-ACTION ((W application-window) (S slider))
  ;; openGL view update
  (display (view-named W "opengl"))
  (display (view-named W "opengl2"))
  (display (view-named W "opengl3"))
  ;; labels update
  (setf (text (view-named W "red label")) (format nil "~,2F" (value (view-named W "red"))))
  (setf (text (view-named W "green label")) (format nil "~,2F" (value (view-named W "green"))))
  (setf (text (view-named W "blue label")) (format nil "~,2F" (value (view-named W "blue")))))


<application-window title="OpenGL 3D" margin="12" width="300" height="300">
  <column align="stretch" valign="stretch">
    <row minimize="vertical" align="stretch" valign="middle">
       <label text="red" align="right" width="50"/>
       <slider name="red" action="adjust-vertex-color-action" max-value="1.0" flex="1"/>
       <label text="0.00" name="red label" width="40"/>
    </row>
    <row minimize="vertical" align="stretch" valign="middle">
       <label text="green" align="right" width="50"/>
       <slider name="green" action="adjust-vertex-color-action" max-value="1.0" flex="1"/>
       <label text="0.00" name="green label" width="40"/>
    </row>
    <row minimize="vertical" align="stretch" valign="middle">
       <label text="blue" align="right" width="50"/>
       <slider name="blue" action="adjust-vertex-color-action" max-value="1.0" flex="1"/>
       <label text="0.00" name="blue label" width="40"/>
    </row>
    <spacer height="12"/>
    <triangle-opengl-dialog name="opengl" vflex="1"/>
    <spacer height="12"/>
    <row align="stretch" valign="stretch" vflex="2">
      <triangle-opengl-dialog name="opengl2" flex="1"/>
      <spacer height="12" width="12"/>
      <triangle-opengl-dialog name="opengl3" flex="1"/>
    </row>
  </column>
</application-window>

;;********************************************
;;*    badged-image-group-list-manager       *
;;********************************************
;; A Special class for 2 level image + text list with disclosable content

;; button actions

(defmethod ADD-AGENT-ACTION ((w application-window) (Button button))
  (let ((image-badge (value (view-named w "image-badge"))) (name (value (view-named w "text-field"))))
    (add-group image-badge `(,name "redlobster.png"  (("shape5" "redlobster.png")( "shape4" "redlobster.png"))))))


(defmethod DELETE-SELECTED-AGENT ((w application-window) (Button button))
  (let ((image-badge (value (view-named w "image-badge"))))
    (delete-group image-badge  (selected-group image-badge))))


(defmethod ADD-SHAPE-ACTION ((w application-window) (Button button))
  (let ((image-badge (value (view-named w "image-badge"))) (name (value (view-named w "text-field"))))
    (add-group-item image-badge (selected-group image-badge) `(,name "redlobster.png"))))

    
(defmethod DELETE-SHAPE-ACTION ((w application-window) (Button button))
  (let ((image-badge (value (view-named w "image-badge"))))
    (delete-group-item image-badge  (selected-group image-badge) (selected-group-item image-badge))))

;; window 

<application-window title="agent-gallery" margin="0" height="500">
  <column align="stretch" valign="stretch">  
    <scroll-box vflex="1">
      <badged-image-group-list-manager name="image-badge">
        <image-badge-group-list-item name="lobster!!" />
      </badged-image-group-list-manager>
    </scroll-box>
    <row align="stretch" valign="stretch" height="30">
      <bevel-button text="Add Agent" action="add-agent-action" width="95" />
      <bevel-button text="Add Shape" action="add-shape-action" width="95" />
      <bevel-button text="Delete"  action="delete-selected-agent" width="80" />
    </row>
    <editable-text  name="text-field" height="20" text="lobster?"/>  
  </column>
</application-window> 

    
;; ************************
;; *  WEB BROWSER         *
;; ************************

;; BE PATIENT: there is no progress report


<application-window title="browser" margin="0">
  <web-browser url="http://www.agentsheets.com"/>
</application-window> 


<application-window title="browser" margin="0">
  <web-browser url="http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/gl/vertex.html"/>
</application-window> 


<application-window title="4 x News" margin="0">
  <column align="stretch" valign="stretch">
    <row align="stretch" valign="stretch" vflex="1">
      <web-browser url="http://www.cnn.com" flex="1"/>
      <web-browser url="http://news.bbc.co.uk" flex="1"/>
    </row>
    <row align="stretch" valign="stretch" vflex="1">
      <web-browser url="http://www.sf.tv/sf1/10vor10/" flex="1"/>
      <web-browser url="http://www.apple.com" flex="1"/>
    </row>
  </column>
</application-window>


#|
More functional browser example
|#

(defmethod GO-TO-URL ((w application-window) (button image-button))
  (let ((url (value (view-named w "url"))))
    (setf  (url (view-named w "browser")) url)
    (print url)
    (load-url (view-named w "browser") url)
    ))

(defmethod load-yahoo ((w application-window) (button choice-image-button))
  (setf (url (view-named w "browser")) "http://www.yahoo.com")
  (load-url (view-named w "browser") "http://www.yahoo.com"))

(defmethod load-espn ((w application-window) (button choice-image-button))
  (setf (url (view-named w "browser")) "http://www.espn.com")
  (load-url (view-named w "browser") "http://www.espn.com"))

(defmethod load-digg ((w application-window) (button choice-image-button))
  (setf (url (view-named w "browser")) "http://www.digg.com")
  (load-url (view-named w "browser") "http://www.digg.com"))
  
(defmethod load-new-york-times ((w application-window) (button choice-image-button))
  (setf (url (view-named w "browser")) "http://www.newyorktimes.com")
  (load-url (view-named w "browser") "http://www.newyorktimes.com"))  
        
<application-window title="browser" height="600" width="800" margin="0">
  <column align="stretch" height="400" valign="stretch">
    <row height="20" align="stretch" valign="stretch">
      <choice-image-button width="140" >                                             
        <choice-button-item text="Yahoo" action="load-yahoo" image="images/excited2.jpg"/>   
        <choice-button-item text="ESPN" action="load-espn" image="images/ESPN.jpg"/>             
        <choice-button-item text="DIGG" action="load-digg" image="images/diggin3.jpg"/> 
        <choice-button-item text="New York Times" action="load-new-york-times" image="images/NYtimes.jpg"/>      
      </choice-image-button>  
      <image-button image="arrow-button.png" height="25" action="go-to-url" width="60" />
      <editable-text width="600" name="url" height="20" text="http://www.google.com"/>  
    </row>
    <web-browser height="800"  name="browser"  url="http://www.google.com"/>
  </column>
</application-window> 

#|
opengl documentation browser
|#

(defmethod GO-TO-URL ((w application-window) (button image-button))
  (let ((gl-command (value (view-named w "gl-command"))))
    ;(setf  (url (view-named w "browser")) gl-command)
    ;(print url)
    (load-url (view-named w "browser") (concatenate 'string "http://www.opengl.org/sdk/docs/man/xhtml/" gl-command ".xml"))
    ))

<application-window title="browser" height="600" width="800" margin="0">
  <column align="stretch" height="400" valign="stretch">
    <row height="20" align="stretch" valign="stretch">
      <image-button image="arrow-button.png" height="25" action="go-to-url" width="60" />
      <editable-text width="600" name="gl-command" height="20" text="glVertex"/>  
    </row>
    <web-browser height="800"  name="browser"  url="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml"/>
  </column>
</application-window> 

;; ****************************************
;; *  Modal Windows                       *
;; *    Subclass dialog-window            *
;; *    have action to call stop-modal    *
;; ****************************************

;; Example 1: get slider value


(defmethod OK-ACTION ((Window dialog-window) (Button button))
  (stop-modal Window (value (view-named Window "value"))))


(defmethod CANCEL-ACTION ((Window dialog-window) (Button button))
  (cancel-modal Window))


(defmethod ADJUST-VALUE-ACTION ((Window dialog-window) (Slider slider))
  (format t "~%value of slider = ~A" (value Slider)))


<dialog-window height="100" title="get a value between 0 and 100">
  <column align="stretch" valign="bottom">
   <slider name="value" action="adjust-value-action" tick-marks="5"/>
    <row align="right" minimize="vertical">
      <button text="Cancel" action="cancel-action"/>
      <button text="OK" action="ok-action"/>
    </row>
  </column>
</dialog-window>   


;; example 2: modal color picker
;; Of course one would use a built in color picker (e.g., OS X NSColorPanel) but this
;; is to illustrate how to return a value and how to set up actions for modal use of a dialog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass COLOR-PICKER-DIALOG (dialog-window)
  ())

;; actions

(defmethod ADJUST-COLOR-ACTION ((W color-picker-dialog) (Slider slider))
  ;; color well update
  (set-color 
   (view-named W "color well")
   :red (value (view-named W "red"))
   :green (value (view-named W "green"))
   :blue (value (view-named W "blue")))
  ;; labels update
  (setf (text (view-named W "red label")) (format nil "~,2F" (value (view-named W "red"))))
  (setf (text (view-named W "green label")) (format nil "~,2F" (value (view-named W "green"))))
  (setf (text (view-named W "blue label")) (format nil "~,2F" (value (view-named W "blue"))))
  (display W))


(defmethod OK-ACTION ((D color-picker-dialog) (Button button))
  (stop-modal D (make-instance 'rgb-color 
                  :red (value (view-named D "red"))
                  :green (value (view-named D "green"))
                  :blue (value (view-named D "blue")))))


(defmethod CANCEL-ACTION ((Window color-picker-dialog) (Button button))
  (cancel-modal Window))

;; may as well return the value as RGB-color object

(defclass RGB-COLOR (xml-serializer)
  ((red :accessor red :type float :initarg :red)
   (green :accessor green :type float :initarg :green)
   (blue :accessor blue :type float :initarg :blue)))


(defmethod PRINT-SLOTS ((Color rgb-color))
  '(red green blue))


<color-picker-dialog title="color picker" height="180">
 <column align="stretch" valign="bottom">
  <row align="stretch" valign="stretch">
    <column align="stretch" flex="2" valign="middle">
      <row minimize="vertical" align="stretch">
         <label text="red" align="right" width="50"/>
         <slider name="red" action="adjust-color-action" max-value="1.0" tick-marks="10" flex="1"/>
         <label text="0.0" name="red label" width="40"/>
      </row>
      <row minimize="vertical" align="stretch">
         <label text="green" align="right" width="50"/>
         <slider name="green" action="adjust-color-action" max-value="1.0" tick-marks="10" flex="1"/>
         <label text="0.0" name="green label" width="40"/>
      </row>
      <row minimize="vertical" align="stretch">
         <label text="blue" align="right" width="50"/>
         <slider name="blue" action="adjust-color-action" max-value="1.0" tick-marks="10" flex="1"/>
         <label text="0.0" name="blue label" width="40"/>
      </row>
    </column>
    <spacer/>
    <rectangle name="color well" flex="1"/>
  </row>
    <spacer/>
    <row align="right" minimize="vertical">
      <button text="Cancel" action="cancel-action"/>
      <button text="OK" action="ok-action"/>
    </row>
  </column>
</color-picker-dialog>


;; more likely than inlining you would be loading the object from a file. This would return the RGB-Color value
;; stick the above XML with a valid XML header into a file and load as object:
;; (load-object (native-path "lui:resources;windows;" "color-picker-dialog.window"))

;; ****************************************
;; *  writeme: More Controls              *
;; ****************************************

#|

Adding a new control to LUI/XLUI with Cocoa is pretty simple. Lets assume you need a check box

1) Create new file checkbox-control.lisp containing control in package :lui, subclassing (most likely) the control class. 
   This file should not contain any reference to Cocoa, CCL and other platform specific code. 
   Look at file "LUI.lisp" for examples

2) Create new file checkbox-control-cocoa.lisp in platform specific folder, e.g., Lisp User Interface/Mac CCL/
   This file uses all the good Cocoa methods. 
   Look at file "LUI Cocoa.lisp" for examples.

3) Export class and slots that will be relevant to XLUI, i.e., developer controllable at XML level

4) Create new file checkbox.lisp in XLUI/ folder
   Probably all you need to do is to mix in the xml-layout-interface, e.g.,

   (defclass CHECKBOX (checkbox-control xml-layout-interface)
     ()
     (:default-initargs )
     (:documentation "checkbox"))

   and perhaps deal with initargs or special initialization.

DONE! Use your new element in a layout

   <window   ....>
     <checkbox selected="true"/>
   </window>

|#









