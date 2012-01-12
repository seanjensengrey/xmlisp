;;;; GLSL-Conway: Game of life implemented in GLSL.
;;;; Harold Hausman
;;;; Jan-8-2012

(in-package :xlui)


(defclass GLSL-CONWAY-VIEW (opengl-dialog)
  ((display-vertex-shader :accessor display-vertex-shader :initform nil)
   (display-fragment-shader :accessor display-fragment-shader :initform nil)
   (display-shader-program :accessor display-shader-program :initform nil)
   (life-vertex-shader :accessor life-vertex-shader :initform nil)
   (life-fragment-shader :accessor life-fragment-shader :initform nil)
   (life-shader-program :accessor life-shader-program :initform nil)
   (frame-buffer :accessor frame-buffer :initform nil)
   (tex-1 :accessor tex-1 :initform nil)
   (tex-2 :accessor tex-2 :initform nil)
   (flip-flop :accessor flip-flop :initform nil)))


(defun set-tex-params ()
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE))


(defmethod PREPARE-OPENGL ((Self glsl-conway-view))
  (glEnable GL_TEXTURE_2D)
  (let ((tex1Name {-1})
        (tex2Name {-1}))
    (glGenTextures 1 tex1Name)
    (setf (tex-1 Self) (get-long tex1Name))
    (glGenTextures 1 tex2Name)
    (setf (tex-2 Self) (get-long tex2Name)))
  
  (with-vector-of-size (&tex (* (width Self) (height self) 3))
    (dotimes (i (* (width Self) (height Self) 3)) (set-byte &tex 0 i))
    (dotimes (i (- (width Self) 400))
      (dolist (y `(100 ,(- (height Self) 100)))
        (set-byte &tex 255 (+ (* (width Self) y 3) (* 3 200) (* 3 i) 0))
        (set-byte &tex 255 (+ (* (width Self) y 3) (* 3 200) (* 3 i) 1))
        (set-byte &tex 255 (+ (* (width Self) y 3) (* 3 200) (* 3 i) 2))))
    (glBindTexture GL_TEXTURE_2D (tex-1 Self))
    (set-tex-params)
    (glTexImage2D GL_TEXTURE_2D 0 3 (width Self) (height Self) 0 GL_RGB GL_UNSIGNED_BYTE &tex)
    (glBindTexture GL_TEXTURE_2D (tex-2 Self))
    (set-tex-params)
    (glTexImage2D GL_TEXTURE_2D 0 3 (width Self) (height Self) 0 GL_RGB GL_UNSIGNED_BYTE &tex))
  
  ;;***  shaders ***
  (setf (display-vertex-shader Self) (glCreateShader GL_VERTEX_SHADER))
  (setf (display-fragment-shader Self) (glCreateShader GL_FRAGMENT_SHADER))
  (setf (life-vertex-shader Self) (glCreateShader GL_VERTEX_SHADER))
  (setf (life-fragment-shader Self) (glCreateShader GL_FRAGMENT_SHADER))
  ;; load shaders
  (set-shader-source (display-vertex-shader Self) "display.vs")
  (set-shader-source (display-fragment-shader Self) "display.fs")
  (set-shader-source (life-vertex-shader Self) "life.vs")
  (set-shader-source (life-fragment-shader Self) "life.fs")
  ;; compile shaders
  (glCompileShader (display-vertex-shader Self))  
  (glCompileShader (life-vertex-shader Self))
  (glCompileShader (display-fragment-shader Self))
  (glCompileShader (life-fragment-shader Self))

  ;; make programs
  (setf (display-shader-program Self) (glCreateProgram))
  (glAttachShader (display-shader-program Self) (display-vertex-shader Self))
  (glAttachShader (display-shader-program Self) (display-fragment-shader Self))
  (glLinkProgram (display-shader-program Self))

  (setf (life-shader-program Self) (glCreateProgram))
  (glAttachShader (life-shader-program Self) (life-vertex-shader Self))
  (glAttachShader (life-shader-program Self) (life-fragment-shader Self))
  (glLinkProgram (life-shader-program Self))

  (glUseProgram (display-shader-program Self))
  (with-cstr (uniformName "uSampler")
    (glUniform1i (glGetUniformLocation (display-shader-program Self) uniformName) 0))
  (with-cstr (uniformName "uWidth")
    (glUniform1f (glGetUniformLocation (display-shader-program Self) uniformName) (float (width Self))))
  (with-cstr (uniformName "uHeight")
    (glUniform1f (glGetUniformLocation (display-shader-program Self) uniformName) (float (height Self))))

  (glUseProgram (life-shader-program Self))
  (with-cstr (uniformName "uSampler")
    (glUniform1i (glGetUniformLocation (life-shader-program Self) uniformName) 0))
  (with-cstr (uniformName "uWidth")
    (glUniform1f (glGetUniformLocation (life-shader-program Self) uniformName) (float (width Self))))
  (with-cstr (uniformName "uHeight")
    (glUniform1f (glGetUniformLocation (life-shader-program Self) uniformName) (float (height Self))))
  
  (let ((bufferName {-1}))
    (glGenFramebuffersEXT 1 bufferName)
    (setf (frame-buffer Self) (get-long bufferName)))

  (setf (flip-flop Self) 0))


(defmethod ANIMATE ((Self glsl-conway-view) Time)
  (setf (flip-flop Self) (if (= 0 (flip-flop Self)) 1 0)))


(defun quad ()
  (glBegin GL_QUADS)
    (glTexCoord2f 0.0 0.0) (glVertex2f -1.0 -1.0)
    (glTexCoord2f 0.0 1.0) (glVertex2f -1.0  1.0)
    (glTexCoord2f 1.0 1.0) (glVertex2f  1.0  1.0)
    (glTexCoord2f 1.0 0.0) (glVertex2f  1.0 -1.0)
    (glEnd))


(defmethod DRAW ((Self glsl-conway-view))
  (let ((t1 (if (= 0 (flip-flop Self)) (tex-1 Self) (tex-2 Self)))
        (t2 (if (= 0 (flip-flop Self)) (tex-2 Self) (tex-1 Self))))
    ;; life
    (glBindTexture GL_TEXTURE_2D t1)
    (glBindFramebufferEXT GL_FRAMEBUFFER_EXT (frame-buffer Self))
    (glFramebufferTexture2DEXT GL_FRAMEBUFFER_EXT GL_COLOR_ATTACHMENT0_EXT GL_TEXTURE_2D t2 0)
    (glUseProgram (life-shader-program Self))
    (quad)
    
    ;; display
    (glBindTexture GL_TEXTURE_2D t2)
    (glBindFramebufferEXT GL_FRAMEBUFFER_EXT 0)
    (glFramebufferTexture2DEXT GL_FRAMEBUFFER_EXT GL_COLOR_ATTACHMENT0_EXT GL_TEXTURE_2D t1 0)
    (glUseProgram (display-shader-program Self))
    (quad)))


(defparameter Wi 

<application-window margin="0" title="GLSL-Conway" width="800" height="450">
  <glsl-conway-view name="GLSL-Conway" />
</application-window>

)


;(start-animation (view-named Wi "GLSL-Conway"))

;; lol @ reset while running (works most of the time...)

(let ((View (view-named Wi "GLSL-Conway")))
  (with-glcontext View
    (prepare-opengl View)))


;(stop-animation (view-named Wi "GLSL-Conway"))
