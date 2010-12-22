;;; Lisp User Interface: Chopper
;;; concepts: agent groups, e.g., chopper containing parts including animated rotors
;;; Alexander Repenning
;;; 11/24/02 OpenGL for MCL 1.0
;;; 10/06/05 OpenGL for MCL 1.6
;;; 05/07/09 CCL

(in-package :xlui)


;; window

(defclass GAME-WINDOW (application-window)
  ())


;; agents

(defclass ROTOR (tile) 
  ((rotor-speed :accessor rotor-speed :initform 90.0 :type float :documentation "degrees/sec")))


(defmethod ANIMATE ((Self rotor) Time)
  (setf (roll Self) (mod (+ (roll Self) (* Time (rotor-speed Self))) 360.0)))


(defclass CHOPPER (group)
  ())


(defmethod ANIMATE ((Self chopper) dt)
  ;; noise functions with low pass filters are used to shake the chopper gently
  ;; http://en.wikipedia.org/wiki/Low-pass_filter#Discrete-time_realization
  (let* ((Alpha (/ dt (+ dt 5.0)))
         (1-Alpha (- 1 Alpha)))
    (setf (x Self) (+ (* Alpha (- (random 10.0) 5.0)) (* 1-Alpha (x self))))
    (setf (y Self) (+ (* Alpha (- (random 10.0) 5.0)) (* 1-Alpha (y self))))
    (setf (z Self) (+ (* Alpha (- (random 10.0) 5.0)) (* 1-Alpha (z self))))
    (setf (heading Self) (+ (* Alpha (- (random 100.0) 50.0)) (* 1-Alpha (heading Self))))
    (setf (pitch Self) (+ (* Alpha (- (random 100.0) 50.0)) (* 1-Alpha (pitch Self))))
    (setf (roll Self) (+ (* Alpha (- (random 100.0) 50.0)) (* 1-Alpha (roll Self))))
    (call-next-method)))


;; Actions

(defmethod START-ACTION ((Window game-window) (button bevel-button))
  (start-animation (view-named Window "war")))


(defmethod STOP-ACTION ((Window game-window) (button bevel-button))
  (stop-animation (view-named Window "war")))


;; window with OpenGL view and simulation control

<game-window title="Choppers" track-mouse="true" margin="0">
  <column align="stretch" valign="stretch">
    <agent-3d-view name="war" vflex="1">
      <chopper>
        <group pitch="-90" x="-1.0">
          <rotor rotor-speed="360.0" width="5.0" height="0.2" x-turn="2.5" y-turn="0.1" texture="metal1.png"/>
        </group>
        <cube draggable="true" x="1.0" y="-1.3" z="-0.5" texture="metal1.png"/>
        <cylinder x="2.0" y="-0.8" heading="90" base-radius="0.3" top-radius="0.1" depth="2.0" texture="metal1.png"/>
        <rotor rotor-speed="720.0" x="3.5" y="-0.8" z="0.2"  width="1.0" height="0.1" x-turn="0.5" y-turn="0.05" texture="metal2.png"/>
      </chopper>
      <sky-dome pitch="-90"/>
    </agent-3d-view>
    <row minimize="vertical" align="center">
      <bevel-button text="start" action="start-action" width="55"/>
      <bevel-button text="stop" action="stop-action" width="55"/>
    </row>
  </column>
</game-window>


