;; Nice Wheels
;; concepts: nested animations (car moves, movement turns wheels), discrete low pass filter, alpha texture sorting
;; 05/22/09 Alexander Repenning

(in-package :xlui)

;; Window

(defclass RACE-TRACK-WINDOW (application-window)
  ())


;; Agents 

(defclass RACE-CAR (group)
  ())


(defmethod ANIMATE ((Self race-car) dt)
  ;; implement digital low pass filter: http://en.wikipedia.org/wiki/Low-pass_filter#Discrete-time_realization
  (let* ((Alpha (/ dt (+ dt (value (view-named (window Self) "delay")))))
         (Position (value (view-named (window Self) "position"))))
    (setf (x Self) (+ (* Alpha Position) (* (- 1 Alpha) (x Self)))))
  (call-next-method))


(defclass WHEEL (group)
  ())


(defmethod ANIMATE ((Self wheel) dt)
  (declare (ignore dt))
  (setf (roll Self) (* -360.0 (/ (x (part-of Self)) 2 #.(float pi 0.0)))))  ;; assume radius = 1


;; Actions

(defmethod START-ACTION ((Window race-track-window) (button bevel-button))
  (start-animation (view-named Window "race")))


(defmethod STOP-ACTION ((Window race-track-window) (button bevel-button))
  (stop-animation (view-named Window "race")))


(defmethod ADJUST-POSITION ((Window race-track-window) (Slider slider))
  (setf (text (view-named Window "position label")) (format nil "~,2F [m]" (value Slider))))


(defmethod ADJUST-DELAY ((Window race-track-window) (Slider slider))
  (setf (text (view-named Window "delay label")) (format nil "~,2F  [sec]" (value Slider))))


;; Scene
;; note: agents with textures including transparency, the wheels, must be last in the scene graph to 
;; minimize transparency issues

<race-track-window title="Race Track" track-mouse="true" margin="0">
  <column align="stretch" valign="stretch">
    <row minimize="vertical" align="center">
      <bevel-button text="start" action="start-action" width="55"/>
      <bevel-button text="stop" action="stop-action" width="55"/>
    </row>
    <agent-3d-view name="race" vflex="1">
      <sky-dome pitch="-90"/>
      <race-car>
        <cube draggable="true" size="4.0"texture="crate.png"/>
        <cube x="4.0" size="4.0" texture="crate.png"/>
        <wheel x="2.0" z="-0.6" heading="180.0">
          <cylinder z="-0.6" top-radius="1.0" height="0.6" texture="tiretrack.png"/>
          <disk texture="wheel.png"/>
        </wheel>
        <wheel x="6.0" z="-0.6" heading="180.0">
          <cylinder z="-0.6" top-radius="1.0" height="0.6" texture="tiretrack.png"/>
          <disk texture="wheel.png"/>
        </wheel>
        <wheel x="2.0" z="4.6">
          <cylinder draggable="true" z="-0.6" top-radius="1.0" height="0.6" texture="tiretrack.png"/>
          <disk draggable="true" z="-0.2" texture="wheel.png"/>
          <disk draggable="true" texture="wheel.png"/>
        </wheel>
        <wheel x="6.0" z="4.6">
          <cylinder z="-0.6" top-radius="1.0" height="0.6" texture="tiretrack.png"/>
          <disk z="-0.2" texture="wheel.png"/>
          <disk texture="wheel.png"/>
        </wheel>
      </race-car>
    </agent-3d-view>
    <row minimize="vertical" align="stretch" valign="middle">
       <label text="position" align="right" width="60"/>
       <slider name="position" max-value="50.0" action="adjust-position" flex="1"/>
       <label text="0.0 [m]" name="position label" width="80"/>
    </row>
    <row minimize="vertical" align="stretch" valign="middle">
       <label text="delay" align="right" width="60"/>
       <slider name="delay" max-value="10.0" action="adjust-delay" flex="1"/>
       <label text="0.0 [sec]" name="delay label" width="80"/>
    </row>
  </column>
</race-track-window>

