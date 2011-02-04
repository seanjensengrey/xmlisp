;;; 08/13/09 Full-Screen-Support.lisp  Alexander Repenning
;;; concepts: full screen and window mode
;;; full screen mode will have the content of a window take over the entire screen while hiding the menu bar
;;; and the dock. Menu bar and dock can still be activated by moving the mouse towards the edge of the screen.
;;; good idea: turn off FSAA (full-scene-anti-aliasing) because especially with large screens, e.g., 30" FSAA switches 
;;; to software rendering mode with older GPUs.
;;; In this full screen mode other windows can still be layers on top, e.g., menus, drag and drop, ...

;; Windows Issues:
;; This example does not switch to full screen when the button is pressed.  
(in-package :xlui)


(defclass FULL-SCREEN-WINDOW (application-window)
  ())


(defmethod SWITCH-WINDOW-MODE-ACTION ((Window full-screen-window) (Button bevel-button))
  (cond
   ((full-screen Window)
    (switch-to-window-mode Window)
    (setf (text (view-named Window "mode switch button")) "switch to full screen"))
   (t
    (switch-to-full-screen-mode Window)
    (setf (text (view-named Window "mode switch button")) "switch to window"))))


<full-screen-window title="Windows can be boring!" margin="0">
 <column align="stretch" valign="stretch">
    <agent-3d-view name="3D View"  full-scene-anti-aliasing="false" vflex="1">
      <camera eye-z="2.0"/>
      <cube texture="metal2.png"/>
      <cylinder draggable="true" top-radius="0.2" texture="metal2.png"/>
      <cube x="-1" y="-1" texture="metal2.png"/>
    </agent-3d-view>
    <row minimize="vertical" align="center">
      <bevel-button name="mode switch button" text="switch to full screen" action="switch-window-mode-action" width="140"/>
    </row>
  </column>
</full-screen-window>