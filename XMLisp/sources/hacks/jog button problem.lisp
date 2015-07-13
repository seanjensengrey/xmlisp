(in-package :xlui)


(defclass M-WINDOW (dialog-window)
  ()
  (:default-initargs
      :do-show-app-window-immediately nil))


(defmethod READ-RETURN-VALUE ((Self m-window))
  ;; We want to return the actual window instead of the selected button so that we can create a window and show it later.
  self)



(defmethod BIG-BUTTON-ACTION ((Self application-window) (Button jog-button))
  (print "press")
  (play-sound "Radar-beep.mp3"))


(defmethod DONE-ACTION ((Self application-window) (Button button))
  (stop-modal Self 99))


(progn

(defparameter *wi* 
<m-window>
  <row>
    <jog-button image="big-minus.png" action="big-button-action"/>
    <button text="OK" action="done-action"/>
  </row>
</m-window>  )


(show-and-run-modal  *wi* ) )