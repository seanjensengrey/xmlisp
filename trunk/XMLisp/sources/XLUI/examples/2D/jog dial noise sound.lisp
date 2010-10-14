;; Simple example of a jog slider
;; Alexander Repenning
;; Move the slider and notice:
;;  - control action keeps getting called even if knob is not moving anymore. Frequency can be controlled
;;  - after mouse up slider snaps back to stop-value


(in-package :lui)


(defclass NOISE-JOG-SLIDER (jog-slider-control)
  ())


(defmethod START-JOG ((Self noise-jog-slider))
  (call-next-method)
  (play-sound "whiteNoise.mp3" :loops t))


(defmethod STOP-JOG ((Self noise-jog-slider))
  (call-next-method)
  (stop-sound "whiteNoise.mp3"))


(defclass JOG-NOISE-WINDOW (window)
  ()
  (:default-initargs
    :width 400
    :height 200))


(defmethod INITIALIZE-INSTANCE :after ((Self jog-noise-window) &rest Args)
  (declare (ignore Args))
  (add-subviews Self (make-instance 'noise-jog-slider :width 300 :x 50 :action 'JOG :min-value -1.0 :max-value 1.0 :stop-value 0.0)))


(defmethod JOG ((Self jog-noise-window) (Slider noise-jog-slider))
  (set-volume "whiteNoise.mp3" (* 0.1 (abs (value Slider))))
  (format t "~%action: jog value is ~A" (value Slider)))


(defparameter *Jog-Window* (make-instance 'jog-noise-window))
