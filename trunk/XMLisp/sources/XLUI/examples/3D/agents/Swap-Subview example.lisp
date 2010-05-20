;; Swapping OpenGL views
;; concepts: view swapping
;; Alexander Repenning, 03/04/10

(in-package :xlui)



(defclass SWAPPING-WINDOW (application-window)
  ())


;; actions

(defmethod SWITCH-TO-SCENE-1 ((w swapping-window) (Button bevel-button))
  (swap-subview (superview (view-named w "scene")) (view-named w "scene") *Scene1*))


(defmethod SWITCH-TO-SCENE-2 ((w swapping-window) (Button bevel-button))
  (swap-subview (superview (view-named w "scene")) (view-named w "scene") *Scene2*))


(defmethod SWITCH-TO-SCENE-3 ((w swapping-window) (Button bevel-button))
  (swap-subview (superview (view-named w "scene")) (view-named w "scene") *Scene3*))


;; window

(defparameter *S-Window* 
<swapping-window track-mouse="true" title="Swap Scenes" margin="0">
  <row align="stretch" valign="stretch">
    <agent-3d-view name="scene" flex="3" vflex="1">
      <text-3d text="empty" x="-0.5" size="5.0"/>
    </agent-3d-view>
    <column align="stretch" width="120" valign="stretch" vflex="1">
      <bevel-button text="scene 1" action="switch-to-scene-1" height="30"/>
      <bevel-button text="scene 2" action="switch-to-scene-2" height="30"/>
      <bevel-button text="scene 3" action="switch-to-scene-3" height="30"/>
    </column>
  </row>
</swapping-window> )

;; Scenes 

(defparameter *Scene1* 
<agent-3d-view name="scene" flex="3" vflex="1">
  <camera eye-x="5.0" eye-y="1.7" eye-z="5.5" center-x="2.6" center-y="0.4" center-z="-0.05"/>
  <cube draggable="true" texture="crate.png"/>
  <sphere draggable="true" x="2.0"/>
  <sphere draggable="true" x="3.0" texture="earth.png"/>
  <cube draggable="true" size="3.0" z="-3.5" texture="crate.png"/>
  <cube x="5" draggable="true" roll="45.0"/>
  <sky-dome pitch="-90"/>
  <cylinder x="3.0" y="3.0" z="-0.6" top-radius="1.0" depth="0.6" texture="tiretrack.png"/>
  <disk x="3.0" y="3" z="-0.1" texture="wheel.png"/>
  <disk x="3.0" y="3" texture="wheel.png"/>
  <text-3d text="box" draggable="true" x="1.0" y="3.2" z="-1.5" size="5.0"/>
  <text-3d text="hello world" draggable="true" y="-2.0" size="5.0"/>
  <text-3d text="earth" draggable="true" size="2.5" x="2.6" y="-0.8" z="0.2"/>
</agent-3d-view>  )


(defparameter *Scene2* 
<agent-3d-view name="scene" flex="3" vflex="1">
  <cube draggable="true" texture="crate.png"/>
</agent-3d-view>)


(defparameter *Scene3* 
<agent-3d-view name="scene" flex="3" vflex="1">
  <sphere draggable="true" x="0.0" texture="earth.png"/>
</agent-3d-view>)


;;Simple Example
#| 
(defmethod SWITCH-TO-1 ((w swapping-window) (Button bevel-button))
  (swap-subview (superview (view-named w "scene")) (view-named w "scene") *Scene-1*)
  
  )

(defmethod SWITCH-TO-2 ((w swapping-window) (Button bevel-button))
  (swap-subview (superview (view-named w "scene")) (view-named w "scene") *Scene1*)
  (set-size (view-named w "scene")  (width (view-named w "scene")) (height (view-named w "scene"))))

(defparameter *S-Window* 
<swapping-window track-mouse="true" title="Swap Scenes" margin="0">
  <row align="stretch" valign="stretch">
    <rectangle name="scene" flex="3" vflex="1"/>
    <column align="stretch" width="120" valign="stretch" vflex="1">
      <bevel-button text="scene 1" action="switch-to-1" height="30"/>
      <bevel-button text="scene 2" action="switch-to-2" height="30"/>
   </column>
  </row>
</swapping-window> )

(defparameter *Scene-1* 
<rectangle name="scene" color="FF0000"/> )


(defparameter *Scene-2* 
<rectangle name="scene" color="0000FF"/> )
|#




