;; Look who is talking 
;; concepts: picking and speech
;; Alexander Repenning, 5/20/09

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


;; to trigger hover handlers the window needs to be track-mouse enabled

<application-window track-mouse="true" title="Hover &amp; Select" margin="0">
  <agent-3d-view name="scene">
    <cube texture="crate.png"/>
    <sphere x="2.0"/>
    <sphere x="3.0" texture="earth.png"/>
    <cube size="3.0" z="-3.5" texture="crate.png"/>
    <cube x="5" roll="45.0"/>
    <sky-dome pitch="90"/>
    <cylinder x="3.0" y="3.0" z="-0.6" top-radius="1.0" height="0.6" texture="tiretrack.png"/>
    <disk x="3.0" y="3" texture="wheel.png"/>
  </agent-3d-view>
</application-window>

;; otherwise there is no hovering

<application-window track-mouse="false" title="Select only" margin="0">
  <agent-3d-view name="scene">
    <cube texture="crate.png"/>
    <sphere x="2.0"/>
    <sphere x="3.0" texture="earth.png"/>
    <cube size="3.0" z="-3.5" texture="crate.png"/>
    <cube x="5" roll="45.0"/>
    <sky-dome pitch="90"/>
  </agent-3d-view>
</application-window>


<scene-inspector-window track-mouse="true" title="Objects" margin="0">
  <row align="stretch" valign="stretch">
    <agent-3d-view name="scene" flex="3" vflex="1">
      <cube texture="crate.png"/>
      <sphere x="2.0"/>
      <sphere x="3.0" texture="earth.png"/>
      <cube size="3.0" z="-3.5" texture="crate.png"/>
      <cube x="5" roll="45.0"/>
      <sky-dome pitch="90"/>
    </agent-3d-view>
    <column align="stretch" width="100" valign="stretch" vflex="1">
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
