
(in-package :xlui)


(defclass VISUAL-AGENTALK-EDITOR-WINDOW (application-window)
  ()
  (:documentation "visual agentalk editor window"))


(defclass VISUAL-AGENTALK-VIEW (agent-3d-view)
  ()
  
  (:documentation "Contains Visual AgenTalk code"))


(defmethod PREPARE-OPENGL ((Self visual-agentalk-view))
  ;; no lighting 
  (glClearColor 1.0 1.0 1.0 0.0)
  ;; enablers
  (glDisable GL_LIGHTING)
  (glEnable GL_DEPTH_TEST)
  ;; Textures
  (glEnable GL_TEXTURE_2D)
  ;; Alpha
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA))


(defmethod SET-SIZE :after ((Self visual-agentalk-view) Width Height)
  ;; layout
  (dolist (Agent (agents Self))
    (layout Agent))
  ;; adjust camera
  (let ((X (/ (+ (object-coordinate-view-width Self) (* 16 *Spacing*)) 2.0d0)))
    (setf (eye-x (camera Self)) x)
    (setf (center-x (camera Self)) x)))


(defmethod INITIALIZE-POSITION ((Self visual-agentalk-view))
  (let* ((Scroller (view-named (window Self) (second (or (member (name Self) *View-Name-Scroller-Names*  :test #'string-equal)
                                                         (return-from initialize-position)))))
         (Scroll-Increment 0.001d0)  ;; this increment should depend on document size 
         (New-Value (min (max (- (value Scroller) (* Scroll-Increment 0.0d0)) 0.0d0) 1.0d0))
         (Camera-y (float (+ (* (- 1.0 New-Value) (height (or (first (agents Self))
                                                              (return-from initialize-position))))
                             0d0))))
    ;; y adjust camera and update-
    (setf (eye-y (camera Self)) Camera-y)
    (setf (center-y (camera Self)) Camera-y)
    (display Self)
    ;; update scroller
    (set-scroller-position Scroller (float New-Value 0.0))))


;;; Actions


(defmethod SWITCH-TO-BEHAVIOR ((Window window) (Button bevel-button))
  (let ((Agent-Name 
         (show-string-popup 
          Window
          (mapcar
           #'(lambda (Agent-Manager)
               (string-capitalize (agentcubes::agentcubes-class-string (name Agent-Manager))))
           (agent-managers *Project-Manager*)))))
    (let ((Behavior (behavior (find-agent-manager *Project-Manager* (agentcubes::agentcubes-class-symbol Agent-Name)))))
      (setf (agents (view-named Window "behavior")) nil)
      (attach (view-named Window "behavior") Behavior)
      (dolist (Agent (agents (view-named Window "behavior")))
        (layout Agent))
      (display (view-named Window "behavior")))))


(defmethod SCROLL-BEHAVIOR ((Window window) (Scroller Scroller))
  (let ((Behavior-View (view-named Window "behavior")))
    (setf (eye-y (camera Behavior-View)) (* -2d0 (value Scroller)))
    ;;(setf (center-y (camera Behavior-View)) (* -2d0 (value Scroller)))
    (display Behavior-View)))



;; Window layout

<visual-agentalk-editor-window title="VAT Explorer" margin="0" width="350" height="400">
  <column valign="stretch" align="stretch">
    <row align="center" valign="middle" height="50">
      <bevel-button text="select agent" width="120" action="switch-to-behavior"/>
    </row>

   <row vflex="1" valign="stretch" align="stretch">
     <visual-agentalk-view name="conditions" flex="1" vflex="1">
      <test/>
     </visual-agentalk-view>
     <scroller/>
     <visual-agentalk-view name="behavior" flex="3" vflex="1"/>
     <scroller action="scroll-behavior"/>
     <visual-agentalk-view name="actions" flex="1" vflex="1"/>
     <scroller/>
   </row>

    <row align="center" valign="middle" height="50">
      <bevel-button text="+ rule" width="100" action="add-rule"/>
      <bevel-button text="+ method" width="100" action="add-method"/>
      <bevel-button text="duplicate" width="100" action="duplicate-instruction"/>
      <bevel-button text="test" width="100" action="test-instruction"/>
    </row>
  </column>
</visual-agentalk-editor-window>

