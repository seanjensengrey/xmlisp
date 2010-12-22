;; Cursor Test
;; 10/12/10 Michael Minerva
;; This test shows the functionality of the view-cursor method.  When you move the mouse over the red or blue rectangle views you should see the drawCursor,
;; moving over the green view should display the eraseCursor and when over the cyan view or anywhere else on the screen you should see default arrowCursor.  

(in-package :xlui)


(defclass CURSOR-WINDOW (application-window)
  ()
  (:default-initargs
      :track-mouse t))


(defclass DRAW-CURSOR-VIEW (rectangle)
  ())


(defmethod VIEW-CURSOR ((Self draw-cursor-view) x y)
  (declare (ignore x y))
  "drawCursor")


(defclass ERASER-CURSOR-VIEW (rectangle)
  ())


(defmethod VIEW-CURSOR ((Self eraser-cursor-view) x y)
  (declare (ignore x y))
  "eraseCursor")


<cursor-window title="Cursor Test" height="375" y="50">
  <column align="stretch" valign="stretch">>
    <draw-cursor-view color="FF0000" name="draw-cursor-view" />
    <draw-cursor-view color="0000FF" name="draw-cursor-view" />
    <eraser-cursor-view color="00FF00" name="erase-cursor-view" />
    <rectangle  color="00FFFF"/>
  </column>
</cursor-window>
