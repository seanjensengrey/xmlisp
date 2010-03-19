;;; 08/01/09 Alexander Repenning
;;; not quite a game but this could easily be turned into one (hint: differnt sized cogs will be quite a bit more interesting)
;;; concepts: drag and drop, transparency
;;; drag and drop benchmark including a large number of animated agents
;;; observe interaction of drag and drop and animation

(in-package :xlui)

;;______________________________________________
;; 2D Classes                                   |
;;______________________________________________

;; Windows

(defclass GEAR-BOX-WINDOW (application-window)
  ((rotation-speed :accessor rotation-speed :initform 180 :documentation "deg/sec")))

;; Views

(defclass GEAR-VIEW (agent-3d-view)
  ())


(defmethod DRAW ((Self gear-view))
  (glDisable GL_DEPTH_TEST) ;; OpenGL depth test cannot handle overlapping textures with 
  (call-next-method))


;; Agents

;;______________________________________________
;; Cog                                          |
;;______________________________________________

(defclass COG (tile)
  ((spin-right :accessor spin-right :initform nil :type boolean :documentation "direction of spin"))
  (:default-initargs :x-turn 0.5 :y-turn 0.5))


(defmethod PRINT-SLOTS ((Self cog))
  ;; avoid printing slots with irelevant info not needed for duplication
  '(texture spin-right draggable))


(defmethod ANIMATE ((Self cog) dt)
  (setf (roll Self)
        (if (spin-right Self)
          (mod (+ (roll Self) (* dt (rotation-speed (window Self)))) 360)
          (mod (- (roll Self) (* dt (rotation-speed (window Self)))) 360))))


;;______________________________________________
;; Cog Mount                                    |
;;______________________________________________

(defclass COG-MOUNT (tile)
  ((cog :accessor cog :initform nil)
   (row :accessor row :initarg :row)
   (column :accessor column :initarg :column))
  (:documentation "place to mount a cog (via drag and drop)"))


(defmethod COG-LEFT ((Cog-Mount cog-mount) (Cog cog))
  (when (> (column Cog-Mount) 0)
    (cog (aref (cog-mounts (part-of Cog-Mount)) (row Cog-Mount) (- (column Cog-Mount) 1)))))


(defmethod COG-RIGHT ((Cog-Mount cog-mount) (Cog cog))
  (when (< (column Cog-Mount) (1- (columns (part-of Cog-Mount))))
    (cog (aref (cog-mounts (part-of Cog-Mount)) (row Cog-Mount) (+ (column Cog-Mount) 1)))))


(defmethod COG-UP ((Cog-Mount cog-mount) (Cog cog))
  (when (> (row Cog-Mount) 0)
    (cog (aref (cog-mounts (part-of Cog-Mount)) (- (row Cog-Mount) 1) (column Cog-Mount)))))


(defmethod COG-DOWN ((Cog-Mount cog-mount) (Cog cog))
  (when (< (row Cog-Mount) (1- (rows (part-of Cog-Mount))))
    (cog (aref (cog-mounts (part-of Cog-Mount)) (+ (row Cog-Mount) 1) (column Cog-Mount)))))


;;______________________________________________
;; Cog Mount Grid                               |
;;______________________________________________

(defclass COG-MOUNT-GRID (group)
  ((rows :accessor rows :type integer)
   (columns :accessor columns :type integer)
   (cog-mounts :accessor cog-mounts :initform nil))
  (:documentation "grid containing rows x columns cog mounts"))


(defmethod PRINT-SLOTS ((Self cog-mount-grid))
  '(x y z rows columns))


(defmethod INITIALIZE-INSTANCE :after ((Self cog-mount-grid) &rest Args)
  ;; create all cog mounts
  (declare (ignore Args))
  (setf (cog-mounts Self) (make-array (list (rows Self) (columns Self))))   
  (dotimes (r (rows Self) Self)
    (dotimes (c (columns Self))
      (let ((Cog-Mount (make-instance 'cog-mount
                         :row r
                         :column c
                         :x (* 0.9 c)
                         :y (* 0.9 r)
                         :width 0.8
                         :height 0.8
                         :part-of Self
                         :texture "metal2.png"))) 
        (setf (aref (cog-mounts Self) r c) Cog-Mount)
        (attach Self Cog-Mount)))))

;;______________________________________________
;; Drag & Drop Interactions                     |
;;______________________________________________

(defmethod IS-GEAR-COMPATIBLE ((Cog-Mount cog-mount) (Cog cog))
  ;; catch all gearbox cases that could be a problem, i.e., cogs that will lock up
  (not
   (or
    ;; there can be only ONE
    (cog Cog-Mount)
    ;; incompatible cogs up/down/left/right?
    (and (cog-up Cog-Mount Cog) (eq (spin-right (cog-up Cog-Mount Cog)) (spin-right Cog)))
    (and (cog-down Cog-Mount Cog) (eq (spin-right (cog-down Cog-Mount Cog)) (spin-right Cog)))
    (and (cog-left Cog-Mount Cog) (eq (spin-right (cog-left Cog-Mount Cog)) (spin-right Cog)))
    (and (cog-right Cog-Mount Cog) (eq (spin-right (cog-right Cog-Mount Cog)) (spin-right Cog))))))


(defmethod COULD-RECEIVE-DROP ((Cog-Mount cog-mount) (Cog cog))
  (values
   (is-gear-compatible Cog-Mount Cog)
   t ;; the cog dragged needs to be copied not moved
   "just because"))


(defmethod COULD-RECEIVE-DROP ((Destination-Cog cog) (Source-Cog cog))
  (values
   nil ;;
   nil ;;
   "there can be only ONE!"))


(defmethod RECEIVE-DROP ((Cog-Mount cog-mount) (Cog cog))
  (format t "~%~A droppend onto ~A" (type-of Cog) (type-of Cog-Mount))
  (let ((New-Cog (duplicate Cog (find-package :xlui))))
    ;; new cog should share x,y location but have a z offset
    (setf (z New-Cog) 0.1)
    (setf (x New-Cog) (- (x Cog-Mount) 0.1))
    (setf (y New-Cog) (- (y Cog-Mount) 0.1))
    ;; attach to view and not to cog mount to make sure all the cogs are drawn
    ;; AFTER everything else because of their transparency
    (attach (view Cog-Mount) New-Cog)
    ;; cog mount needs to be aware of cog as well
    (setf (cog Cog-Mount) Cog)
    (unless (is-animated (view Cog-Mount)) (display (view Cog-Mount)))))

;;______________________________________________
;; GUI Actions                                  |
;;______________________________________________

(defmethod START-ACTION ((Window gear-box-window) (button bevel-button))
  (start-animation (view-named Window "gear box")))


(defmethod STOP-ACTION ((Window gear-box-window) (button bevel-button))
  (stop-animation (view-named Window "gear box")))


(defmethod ADJUST-SPEED ((Window gear-box-window) (Slider slider))
  (setf (rotation-speed Window) (value Slider))
  (setf (text (view-named Window "speed label")) (format nil "~,1F  [deg/s]" (value Slider))))

;;______________________________________________
;; GUI                                          |
;;______________________________________________
#|
<gear-box-window title="Drag cogs from this window..." track-mouse="true" margin="0" x="10" y="50">
  <column align="stretch" valign="stretch">
    <row minimize="vertical" align="center">
      <bevel-button text="start" action="start-action" width="55"/>
      <bevel-button text="stop" action="stop-action" width="55"/>
    </row>
    <gear-view name="gear box" vflex="1">
      <sky-dome pitch="-90"/>
      <cog texture="cog24.png" draggable="true"/>
      <cog texture="cog24.png" draggable="true" x="0.9" spin-right="true"/>
    </gear-view>
    <row minimize="vertical" align="stretch" valign="middle">
       <label text="speed" align="right" width="60"/>
       <slider name="speed" max-value="720.0" action="adjust-speed" flex="1"/>
       <label text="0.0 [deg/s]" name="speed label" width="80"/>
    </row>
  </column>
</gear-box-window>


(defparameter *Gears*
<gear-box-window title="...into this window" track-mouse="true" margin="0" x="400" y="50" width="400" height="800">
  <column align="stretch" valign="stretch">
    <row minimize="vertical" align="center">
      <bevel-button text="start" action="start-action" width="55"/>
      <bevel-button text="stop" action="stop-action" width="55"/>
    </row>
    <gear-view name="gear box" vflex="1">
      <camera eye-z="10" eye-x="2.0" eye-y="4.6" center-x="2.0" center-y="4.6"/>
      <sky-dome pitch="-90"/>
      <cog-mount-grid rows="10" columns="5"/>
    </gear-view>
    <row minimize="vertical" align="stretch" valign="middle">
       <label text="speed" align="right" width="60"/>
       <slider name="speed" max-value="720.0" action="adjust-speed" flex="1"/>
       <label text="0.0 [deg/s]" name="speed label" width="80"/>
    </row>
  </column>
</gear-box-window> )

<gear-box-window title="Drag and Drop in one window" track-mouse="true" margin="0" x="10" y="50" height="800" width="500">
  <column align="stretch" valign="stretch">
    <row minimize="vertical" align="center">
      <bevel-button text="start" action="start-action" width="55"/>
      <bevel-button text="stop" action="stop-action" width="55"/>
    </row>
    <gear-view name="gear box" vflex="1">
      <camera eye-z="10" eye-x="2.0" eye-y="4.6" center-x="2.0" center-y="4.6"/>
      <sky-dome pitch="-90" />
      <cog texture="cog24.png" draggable="true" x="-2.0"/>
      <cog texture="cog24.png" draggable="true" x="-0.9" spin-right="true"/>
      <cog-mount-grid rows="10" columns="5"/>
    </gear-view>
    <row minimize="vertical" align="stretch" valign="middle">
       <label text="speed" align="right" width="60"/>
       <slider name="speed" max-value="720.0" action="adjust-speed" flex="1"/>
       <label text="0.0 [deg/s]" name="speed label" width="80"/>
    </row>
  </column>
</gear-box-window>
|#

;; (camera (view-named *Gears* "gear box"))