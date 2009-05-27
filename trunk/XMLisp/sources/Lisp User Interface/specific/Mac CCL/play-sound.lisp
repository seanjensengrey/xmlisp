;; play-sound.lisp
;; status: feature spartan and no real attempt to manage memory but works
;; 11/30/08 Alexander Repenning
;;; updated: 05/13/09 
;;; todo: support parallel sound: new NSsound instance when old one is still playing sound

(in-package :lui)



(defparameter *Sounds* (make-hash-table :test #'equal) "Sound handles")


(defgeneric PLAY-SOUND (Name)
  (:documentation "Play sound in resources/sounds/ folder"))


(defmethod PLAY-SOUND ((Name string))
  (let ((Sound (gethash Name *Sounds*)))
    (cond
     (Sound 
      (#/play Sound))
     (t 
      (setf (gethash Name *Sounds*) 
            (#/initWithContentsOfFile:byReference: 
             (#/alloc ns:ns-sound) 
             (native-string (native-path "lui:resources;sounds;" Name))
             #$YES))
      (#/play (gethash Name *Sounds*))))))


#| Examples:

(play-sound "Radar-beep.mp3")
(play-sound "Inhale.mp3")
(play-sound "Explode.mp3")

|#
