;; Look who is talking 
;; concepts: picking and speech
;; Alexander Repenning, 5/20/09

(in-package :xlui)


(defclass CHATTY-WINDOW (application-window)
  ()
  (:documentation "pointing at objects inside me will make them talk"))


(defmethod MOUSE-HOVER-ENTER-EVENT-HANDLER ((Self cube))
  (call-next-method)
  (when (equal (type-of (window Self)) 'chatty-window)
    (if (texture Self)
      (speak (format nil "Cube with texture ~A, of size ~A" (subseq (texture Self) 0 (position #\. (texture Self) :from-end t)) (size Self)))
      (speak (format nil "Cube of size ~A" (size Self)) "Victoria"))))


(defmethod MOUSE-HOVER-ENTER-EVENT-HANDLER ((Self sphere))
  (call-next-method)
  (when (equal (type-of (window Self)) 'chatty-window)
    (if (texture Self)
      (speak (format nil "sphere with texture ~A, and a radius of ~F" (subseq (texture Self) 0 (position #\. (texture Self) :from-end t)) (float (size Self) 0.0)))
      (speak (format nil "sphere with radius of ~F" (float (size Self) 0.0)) "Victoria"))))


;; to trigger hover handlers the window needs to be track-mouse enabled

<chatty-window track-mouse="true" title="Objects" margin="0">
  <agent-3d-view name="scene">
    <cube texture="crate.png"/>
    <sphere x="2.0"/>
    <sphere x="3.0" texture="earth.png"/>
    <cube size="3.0" z="-3.5" texture="crate.png"/>
    <cube x="5" roll="45.0"/>
    <sky-dome pitch="-90"/>
  </agent-3d-view>
</chatty-window>

