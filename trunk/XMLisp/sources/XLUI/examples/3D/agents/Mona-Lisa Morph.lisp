;;; Mona Lisa Morph
;;; 8/14/09 Alexander Repenning
;;; concepts: use of image warping/morphing
;;; background: http://www.cs.colorado.edu/~ralex/papers/PDF/AVI08_end-user_visualizations.pdf 

(in-package :xlui)


(defclass MORPH-WINDOW (application-window)
  ((variables :accessor variables :initform (make-hash-table) :documentation "variables used to control morphing"))
  (:documentation "The Morph Window includes all the image warp variables"))


(defmethod GET-MORPH-VARIABLE-VALUE ((Self morph-window) Variable-Name)
  (gethash Variable-Name (variables Self) 0.0)) ;; if variable does not exist assume it to be 0.0


(defmethod SET-MORPH-VARIABLE-VALUE ((Self morph-window) Variable-Name Value)
  (setf (gethash Variable-Name (variables Self)) Value))


(defclass MORPH-VIEW (agent-3d-view)
  ())


(defmethod GET-AGENT-ATTRIBUTE-VALUE ((Self morph-view) Variable-Name)
  ;; every variable access in a vertex coordinate expression, e.g., 
  ;; x="0.520 + 0.0003 * happiness" will result in calling this method with the variable name
  (get-morph-variable-value (window Self) Variable-Name))

;; Actions

(defmethod ADJUST-EMOTION-ACTION ((Window morph-window) (Slider slider))
  (setf (text (view-named Window "happy label")) (format nil "~,1F [haha]" (value Slider)))
  (set-morph-variable-value Window 'happiness (value Slider)))

;; GUI: notice, some vertex definitions of the morph include equations, e.g. "0.520 + 0.0003 * happiness"

<morph-window title="Mona Lisa: sad or happy?" margin="0" height="500" width="300">
  <column align="stretch" valign="stretch">
    <row minimize="vertical" align="stretch" valign="middle">
       <label text="happy:" align="right" width="60"/>
       <slider name="happy" min-value="-34.0" max-value="7.0" action="adjust-emotion-action" flex="1"/>
       <label text="0.0 [haha]" name="happy label" width="100"/>
    </row>
    <morph-view name="Mona Lisa" vflex="1">
      <camera eye-x="0.3" eye-y="0.7" eye-z="0.5" center-x="0.3" center-y="0.7"/>
      <morph image="monalisa.jpg" scale-x="0.6">
      <vertex x="0.000" y="0.000" xt="0.000" yt="0.000"/> 
      <vertex x="0.000" y="1.000" xt="0.000" yt="1.000"/> 
      <vertex x="0.995" y="1.000" xt="0.995" yt="1.000"/> 
      <vertex x="0.995" y="0.000" xt="0.995" yt="0.000"/> 
      <vertex x="0.442" y="0.710" xt="0.442" yt="0.710"/> 
      <vertex x="0.462" y="0.710" xt="0.462" yt="0.710"/> 
      <vertex x="0.462" y="0.686" xt="0.462" yt="0.686"/> 
      <vertex x="0.442" y="0.686" xt="0.442" yt="0.686"/> 
      
      <vertex x="0.520 + 0.0003 * happiness" y="0.712 + 0.0003 * happiness" xt="0.520" yt="0.712"/> 
      <vertex x="0.398 - 0.0003 * happiness" y="0.708 + 0.0003 * happiness" xt="0.398" yt="0.708"/> 

      <vertex x="0.352" y="0.638" xt="0.352" yt="0.638"/> 
      <vertex x="0.622" y="0.638" xt="0.622" yt="0.638"/> 
      <vertex x="0.622" y="0.860" xt="0.622" yt="0.860"/> 
      <vertex x="0.352" y="0.860" xt="0.352" yt="0.860"/> 
      <vertex x="0.378" y="0.752" xt="0.378" yt="0.752"/> 
      <vertex x="0.560" y="0.752" xt="0.560" yt="0.752"/> 
      <triangle vertices="0 1 10"/> 
      <triangle vertices="10 13 1"/> 
      <triangle vertices="10 11 0"/> 
      <triangle vertices="0 11 3"/> 
      <triangle vertices="3 11 2"/> 
      <triangle vertices="11 12 2"/> 
      <triangle vertices="12 13 2"/> 
      <triangle vertices="2 13 1"/> 
      <triangle vertices="11 15 12"/> 
      <triangle vertices="10 14 13"/> 
      <triangle vertices="11 10 6"/> 
      <triangle vertices="6 7 10"/> 
      <triangle vertices="12 13 5"/> 
      <triangle vertices="5 4 13"/> 
      <triangle vertices="5 15 12"/> 
      <triangle vertices="4 14 13"/> 
      <triangle vertices="7 9 10"/> 
      <triangle vertices="6 8 11"/> 
      <triangle vertices="15 8 5"/> 
      <triangle vertices="14 9 4"/> 
      <triangle vertices="8 6 5"/> 
      <triangle vertices="7 6 5"/> 
      <triangle vertices="7 5 4"/> 
      <triangle vertices="7 4 9"/> 
      <triangle vertices="11 8 15"/> 
      <triangle vertices="10 9 14"/> 
      </morph>
    </morph-view>
  </column>
</morph-window>

;; (camera (view-named *mona* "Mona Lisa"))
;; (inspect (first (agents (view-named *mona* "Mona Lisa"))))


