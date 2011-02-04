;; Picking.lisp
;; concepts: picking and selection
;; Alexander Repenning, 5/20/09

;; Windows Issues:
;; It seems that when you draw the window to a new location, the whole openGLView goes white
;; and does not return until you try and drag the contents of the view.  Also, rotation causes
;; terrible frame rates.  

(in-package :xlui)


(defclass SCENE-INSPECTOR-WINDOW (application-window)
  ())


(defmethod INSPECT-AGENT ((Self scene-inspector-window) (Agent agent-3d))
  (setf (value (view-named Self "x")) (x Agent))
  (setf (value (view-named Self "y")) (y Agent))
  (setf (value (view-named Self "z")) (z Agent))
  (setf (value (view-named Self "roll")) (roll Agent))
  (setf (value (view-named Self "pitch")) (pitch Agent))
  (setf (value (view-named Self "heading")) (heading Agent))
  ;; agent type
  (setf (text (view-named Self "type")) (string-capitalize (symbol-name (type-of Agent)))))


(defmethod INSPECT-AGENT ((Self application-window) (Agent agent-3d))
  ;; nothing
  )

(defmethod MOUSE-HOVER-ENTER-EVENT-HANDLER ((Self agent-3d))
  (display (view Self))
  (inspect-agent (window Self) Self))


(defmethod MOUSE-HOVER-LEAVE-EVENT-HANDLER ((Self agent-3d))
  (display (view Self)))


(defmethod MOUSE-CLICK-EVENT-HANDLER ((Self agent-3d))
  (display (view Self)))


(defparameter *Track-Mouse-Window*
<scene-inspector-window track-mouse="true" title="Hover, Select &amp; Drag" margin="0">
  <row align="stretch" valign="stretch">
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
    </agent-3d-view>
    <column align="stretch" width="130" valign="stretch" vflex="1">
      <label name="type" text="" align="left" flex="1"/>
      <spacer height="5"/>
      <row align="stretch" minimize="vertical" valign="bottom">
       <label text="x:" align="right" flex="2"/>
       <editable-number name="x" text="0.0" flex="1"/>
      </row>
      <row align="stretch" minimize="vertical" valign="bottom">
       <label text="y:" align="right" flex="2"/>
       <editable-number name="y" text="0.0" flex="1"/>
      </row>
      <row align="stretch" minimize="vertical" valign="bottom">
       <label text="z:" width="70" align="right" flex="2"/>
       <editable-number name="z" text="0.0" flex="1"/>
      </row>
      <row align="stretch" minimize="vertical" valign="bottom">
       <label text="roll:" width="70" align="right" flex="2"/>
       <editable-number name="roll" text="0.0" flex="1"/>
      </row>
      <row align="stretch" minimize="vertical" valign="bottom">
       <label text="pitch:" width="70" align="right" flex="2"/>
       <editable-number name="pitch" text="0.0" flex="1"/>
      </row>
      <row align="stretch" minimize="vertical" valign="bottom">
       <label text="heading:" width="70" align="right" flex="2"/>
       <editable-number name="heading" text="0.0" flex="1"/>
      </row>
    </column>
  </row>
</scene-inspector-window>
)

