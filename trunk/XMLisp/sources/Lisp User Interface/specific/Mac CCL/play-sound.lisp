;; play-sound.lisp
;; status: feature spartan and no real attempt to manage memory but works
;; 11/30/08 Alexander Repenning
;;; updated: 05/13/09 
;;; todo: support parallel sound: new NSsound instance when old one is still playing sound

(in-package :lui)



(defparameter *Sounds* (make-hash-table :test #'equal) "Sound handles")

(defvar *Secondary-Sound-File-Directory-Hook* nil "lambda () ->  directory-pathname")


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


(defun SOUND-FILES-IN-SOUND-FILE-DIRECTORY () "
  out: Sound-Files lisp of pathname"
  (directory "lui:resources;sounds;*.*"))

#|
  (append
   ;; secondary sounds first because they have higher priority
   (and *Secondary-Sound-File-Directory-Hook* 
        (directory 
         (format nil "~A*"  (mac-directory-namestring (funcall *Secondary-Sound-File-Directory-Hook*)))
         :test #'valid-sound-file-p))
   (directory 
    (format nil "~A*"  (mac-directory-namestring *Sound-File-Directory*))
    :test #'valid-sound-file-p)))

|#


#| Examples:

(play-sound "Radar-beep.mp3")
(play-sound "Inhale.mp3")
(play-sound "Explode.mp3")

|#
