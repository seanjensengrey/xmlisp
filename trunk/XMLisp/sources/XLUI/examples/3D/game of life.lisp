;; LUI Game of Life
;; Alexander Repenning, 4/28/09
;; compare to http://o3d-life.googlecode.com/svn/trunk/life_colors.html
;; Lisp version is still faster than JavaScript

(in-package :xlui)


(defclass GAME-OF-LIFE (opengl-dialog)
  ((size :accessor size :initform 100 :type integer)
   (world1 :accessor world1 :initform nil)
   (world2 :accessor world2 :initform nil)
   (image :accessor image :initform nil)))

(defmethod initialize-instance :after ((Self game-of-life) &key)
  (let ((image (make-vector-of-size (* 128 128 4))))
    (dotimes (i (* 128 128))
      (set-byte image 40 (+ (* 3 i) 0))
      (set-byte image 40 (+ (* 3 i) 1))
      (set-byte image 40 (+ (* 3 i) 2)))
    (setf (image Self) image)))

(defmethod RANDOMIZE ((Self game-of-life))
  (dotimes (i (size Self))
    (dotimes (j (size Self))
      (setf (aref (world1 Self) i j) (random 2))))
  (with-glcontext Self
    (draw Self)))


(defmethod COUNT-LIFE ((Self game-of-life) i j)
  ;; count life cells, toroidal array reference
  (let ((S (size Self))
        (w (world1 Self))
        (Sum 0))
    ;; 8 neighbours
    (dolist (di '(-1 0 1) Sum)
      (let ((i (mod (+ i di) s)))
        (dolist (dj '(-1 0 1))
          (unless (and (= di 0) (= dj 0)) (incf Sum (aref w i (mod (+ j dj) s)))))))))


(defmethod PREPARE-OPENGL ((Self game-of-life))
  ;; OpenGL setup
  (glEnable GL_TEXTURE_2D)
  (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
  ;; Alpha
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  ;; Camera
  (aim-camera (camera Self) :eye-x 0.5 :eye-y 0.3 :eye-z -1.0 :center-x 0.5 :center-y 0.3)
  ;; arrays
  (setf (world1 Self) (make-array (list (size Self) (size Self)) :element-type '(unsigned-byte 8)))
  (setf (world2 Self) (make-array (list (size Self) (size Self)) :element-type '(unsigned-byte 8)))
  (randomize Self))


(defmethod DRAW ((Self game-of-life))
  ;; Texture
  (use-texture Self "black.png")  ;; this texture needs to be >=  than the worlds
  ;; no aliasing of this texture
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
  (let ((s (size Self)))
    (dotimes (i s)
      (dotimes (j s)
        (let ((Count (count-life Self i j)))
          (case (aref (world1 Self) i j)
            (1 ;; alive ... 
             (cond
              ((< Count 2) (setf (aref (world2 Self) i j) 0)) ;; rule 1, die: underpopolation
              ((> Count 3) (setf (aref (world2 Self) i j) 0)) ;; rule 2, die: overpopulation
              (t (setf (aref (world2 Self) i j) 1))))         ;; rule 3, live
            (0 ;; dead ...
             (cond
              ((= Count 3) (setf (aref (world2 Self) i j) 1)) ;; rule 4, become alive
              (t (setf (aref (world2 Self) i j) 0)))))
          ;; update image
          (let ((Index (* 3 (+ i (* 128 j)))))
            (set-byte (image Self) (case (aref (world2 Self) i j) (0 0) (t (- 255 (* Count 60)))) Index)  ; red
            (set-byte (image Self) (case (aref (world2 Self) i j) (0 0) (t (* Count 40))) (1+ Index)))))) ; green
    ;; insert subimage
    (glTexSubImage2D GL_TEXTURE_2D 0 0 0 s s GL_RGB GL_UNSIGNED_BYTE (image Self))
    ;; render
    (glBegin GL_QUADS)
    (glColor4f 1.0 1.0 1.0 1.0)
    (glTexCoord2f 0.0 0.0) (glVertex2f 0.0 0.0)
    (glTexCoord2f 0.0 1.0) (glVertex2f 0.0 1.0)
    (glTexCoord2f 1.0 1.0) (glVertex2f 1.0 1.0)
    (glTexCoord2f 1.0 0.0) (glVertex2f 1.0 0.0)
    ;; reflection
    (glColor4f 1.0 1.0 1.0 0.5)
    (glTexCoord2f 0.0 0.0) (glVertex2f 0.0 0.0)
    (glColor4f 0.0 0.0 0.0 0.0)
    (glTexCoord2f 0.0 1.0) (glVertex2f 0.0 -1.0)
    (gltexcoord2f 1.0 1.0) (glVertex2f 1.0 -1.0)
    (glColor4f 1.0 1.0 1.0 0.5)
    (glTexCoord2f 1.0 0.0) (glVertex2f 1.0 0.0)
    (glEnd)
    ;; swap
    (rotatef (world1 Self) (world2 Self))))


(defmethod CLEAR-BACKGROUND ((Self game-of-life))
  (glClearColor 0.0 0.0 0.0 1.0)
  (glClear #.(logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT)))



;; GUI

(defclass GAME-OF-LIFE-WINDOW (application-window)
  ())


(defmethod START-ACTION ((Window game-of-life-window) (button bevel-button))
  (start-animation (view-named Window "life")))


(defmethod STOP-ACTION ((Window game-of-life-window) (button bevel-button))
  (stop-animation (view-named Window "life")))


(defmethod COMPUTE-FRAME-RATE ((Window game-of-life-window) (button bevel-button))
  (print (frame-rate (view-named Window "life"))))


(defmethod NEW-RANDOM-SETUP ((Window game-of-life-window) (button bevel-button))
  (with-glcontext (view-named Window "life")  ;; only to synchronize
    (randomize (view-named Window "life"))))


<game-of-life-window title="Game of Life" margin="0" height="400">
  <column align="stretch" valign="stretch">
    <game-of-life name="life" vflex="1"/>
      <row minimize="vertical" align="center">
        <bevel-button text="start" action="start-action" width="55"/>
        <bevel-button text="stop" action="stop-action" width="55"/>
        <bevel-button text="frame rate" action="compute-frame-rate" width="100"/>
        <bevel-button text="randomize" action="new-random-setup" width="100"/>
      </row>
  </column>
</game-of-life-window>

