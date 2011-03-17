;;; 9/25/07 Sokoban Levels
;;; generate huge numbers of levels from http://www.sourcecode.se/sokoban/levels.php
;;; warning: there are some extreemly hard and frustrating levels at that site
;;; need to manually strip header



(in-package :ad3d)



(defparameter *Level-File* (choose-file-dialog))


(defun GENERATE-LEVEL-FILE (Rows Name)
  (with-open-file (Out (concatenate 'string "Ristretto to Go:Users:alex:Desktop:Levels:" Name ".world") :direction :output :if-exists :supersede)
    (format Out "<?xml version=\"1.0\"?>
<world-root window-x=\"285\" window-y=\"151\" window-width=\"785\" window-height=\"602\" default-window-type=\"WORLD\">
  <camera eye-x=\"10.144598107034435\" eye-y=\"-0.5504296388647374\" eye-z=\"14.45932870654023\" center-x=\"10.0\" center-y=\"7.499999999999999\" center-z=\"0.0\" up-x=\"0.0012694841556784853\" up-y=\"15.868477888737889\" up-z=\"0.12694418392578832\" fovy=\"60.0\" aspect=\"1.3321799307958477\" near=\"0.004999999888241291\" far=\"2000.0\" azimuth=\"0.009999999999999875\" zenith=\"-0.5080000000000001\"/>
  <light-source x=\"10.0\" y=\"10.4\" z=\"10.0\">
    <ambient red=\"0.0\" green=\"0.0\" blue=\"0.0\"/>
    <diffuse red=\"1.0\" green=\"1.0\" blue=\"1.0\"/>
    <specular red=\"1.0\" green=\"1.0\" blue=\"1.0\"/>
  </light-source>
  <ad3d-sky-dome name=\"The World around me\" pitch=\"0.0\" heading=\"0.0\"/>
  <agent-matrix name=\"AGENT-MATRIX11508\" rows=\"~A\" columns=\"~A\" layers=\"1\" is-grid-visible=\"false\">"
            (length Rows)
            (reduce #'max Rows :key #'length))
    (let ((Index 0))
      (dolist (Row Rows)
        (let ((Some-Agent-P nil))
          (dotimes (I (length Row))
            (let ((Char (char Row I)))
              (case Char
                (#\# 
                 (format Out "~%<wall_agent row=\"~A\" col=\"~A\" shape-name=\"brick\"/>" Index I)
                 (setq Some-Agent-p t))
                (#\$ 
                 (format Out "~%<ground_agent row=\"~A\" col=\"~A\" shape-name=\"floor_tile\"/>" Index I)
                 (format Out "~%<crate_agent row=\"~A\" col=\"~A\" shape-name=\"crate\"/>" Index I)
                 (setq Some-Agent-p t))
                (#\@ 
                 (format Out "~%<ground_agent row=\"~A\" col=\"~A\" shape-name=\"floor_tile\"/>" Index I)
                 (format Out "~%<pusher_agent row=\"~A\" col=\"~A\" shape-name=\"red_lobster\"/>" Index I)
                 (setq Some-Agent-p t))
                (#\. 
                 (format Out "~%<ground_agent row=\"~A\" col=\"~A\" shape-name=\"goal_tile\"/>" Index I)
                 (setq Some-Agent-p t))
                (#\* 
                 (format Out "~%<ground_agent row=\"~A\" col=\"~A\" shape-name=\"goal_tile\"/>" Index I)
                 (format Out "~%<crate_agent row=\"~A\" col=\"~A\" shape-name=\"crate\"/>" Index I)
                 (setq Some-Agent-p t))
                (t
                 (when Some-Agent-p
                   (format Out "~%<ground_agent row=\"~A\" col=\"~A\" shape-name=\"floor_tile\"/>" Index I)))))))
        (incf Index)))
    (format Out "~%</agent-matrix>
</world-root>")))





(defun GENERATE-LEVELS (File)
  (with-open-file (Input File :direction :input)
    (loop
      (let ((Line (read-line Input nil nil)))
        (print Line)
        (unless Line (return))
        ;; begin of level
        (when (char= (char Line 0) #\;)
          (let ((Name (subseq Line 2)))
            ;; (print Name)
            (generate-level-file (read-level Input) Name)))))))



(defun READ-LEVEL (Input)
  (let ((Rows nil))
    (loop
      (let ((Row (read-line Input nil nil)))
        (when (or (null Row) (string= Row "")) (return Rows))
        (push Row Rows)))))



#| Examples:


(generate-levels *Level-File*)



|#



