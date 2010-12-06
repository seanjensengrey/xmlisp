;; Shader Demo, Alexander Repenning 12/3/10
;; concepts: how to load, compile, link and use GLSL vertex and fragment shaders
;; tested: Mac OS X 10.6.5, GLSL 1.20
;; how to use: load code and evaluate XML; you should see a window containing a Mona Lisa texture in gray scale. 
;;   Adjust the blur slider to adjust the degree of blur
;;   Adjust the saturation slider to adjust color saturation: 0=gray scale, 1=original colors, > 1 over saturated, < 0 negative saturation


(in-package :xlui)


(defclass BLUR-VIEW (opengl-dialog)
  ((vertex-shader :accessor vertex-shader :initform nil)
   (fragment-shader :accessor fragment-shader :initform nil)
   (shader-program :accessor shader-program :initform nil :documentation "OpenGL GLSL shader program object")))


(defmethod PREPARE-OPENGL ((Self blur-view))
  ;; Textures
  (glEnable GL_TEXTURE_2D)
  (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_DECAL)
  ;; Alpha
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  ;; camera
  (aim-camera (camera Self) :eye-x 0.5 :center-x 0.5 :eye-y 1.0 :center-y 1.0 :eye-z 2.0 :near 1.0)
  ;;***  shaders ***
  (setf (vertex-shader Self) (glCreateShader GL_VERTEX_SHADER))
  (setf (fragment-shader Self) (glCreateShader GL_FRAGMENT_SHADER))
  ;; load shaders
  (set-shader-source (vertex-shader Self) "basic.vs")
  (set-shader-source (fragment-shader Self) "blur.fs")
  ;; compiler vertex shader
  (glCompileShader (vertex-shader Self))
  (with-vector-of-size (&valid (sizeof 'long))
    (glGetShaderiv (vertex-shader Self) GL_COMPILE_STATUS &valid)
    (when (equal (get-long &valid) GL_FALSE)
      (error (get-shader-info-log (vertex-shader Self)))))
  ;; compile fragment shader
  (glCompileShader (fragment-shader Self))
  (with-vector-of-size (&valid (sizeof 'long))
    (glGetShaderiv (fragment-shader Self) GL_COMPILE_STATUS &valid)
    (when (equal (get-long &valid) GL_FALSE)
      (error (get-shader-info-log (fragment-shader Self)))))
  ;; make program
  (setf (shader-program Self) (glCreateProgram))
  (glAttachShader (shader-program Self) (vertex-shader Self))
  (glAttachShader (shader-program Self) (fragment-shader Self))
  (glLinkProgram (shader-program Self)) ;; should do error testing, e.g., OpenGL Superbible V 5, page 242
  (glUseProgram (shader-program Self)))
  

(defmethod ADJUST-BLUR-ACTION ((Window application-window) (Slider slider))
  ;; update slider label
  (setf (text (view-named Window "blur value label")) (format nil "~,2F pixels" (value Slider)))
  ;; update OpenGL picture
  (let ((OpenGL-View (view-named Window "OpenGL View")))
    ;; update shader uniform
    (with-glcontext OpenGL-View
      (with-cstr (uniformName "offset")
        (glUniform1f (glGetUniformLocation (shader-program OpenGL-View) uniformName) (/ (value Slider) 512.0))))
    ;; update OpenGL view
    (display OpenGL-View)))


(defmethod ADJUST-SATURATION-ACTION ((Window application-window) (Slider slider))
  ;; update slider label
  (setf (text (view-named Window "saturation value label")) (format nil "~,3F" (value Slider)))
  ;; update OpenGL picture
  (let ((OpenGL-View (view-named Window "OpenGL View")))
    ;; update shader uniform
    (with-glcontext OpenGL-View
      (with-cstr (uniformName "saturation")
        (glUniform1f (glGetUniformLocation (shader-program OpenGL-View) uniformName) (value Slider))))
    ;; update OpenGL view
    (display OpenGL-View)))


(defmethod DRAW ((Self blur-view))
  (use-texture Self "monalisa.jpg")
  (glBegin GL_QUADS)
  (glNormal3f 0.0 0.0 1.0)
  (glTexCoord2f 0.0 0.0) (glVertex2f 0.0 0.0)
  (glTexCoord2f 0.0 1.0) (glVertex2f 0.0 2.0)
  (glTexCoord2f 1.0 1.0) (glVertex2f 1.0 2.0)
  (glTexCoord2f 1.0 0.0) (glVertex2f 1.0 0.0)
  (glEnd))


;; User Interface: typically this would be put into a seprate file in the /resources/ folder and loaded via load-object

<application-window margin="0" title="Shader Demo" width="400" height="800">
  <column align="stretch" valign="stretch">>
    <blur-view name="OpenGL view" vflex="1"/>
    <row  minimize="vertical" align="stretch" valign="middle">
      <label text="blur" width="90" align="right"/>
      <slider name="Blur" min-value="0.0" max-value="3.0" action="adjust-blur-action" flex="1"/>
      <label text="0.0 [pixels]" name="blur value label" width="80"/>
    </row>
    <row  minimize="vertical" align="stretch" valign="middle">
      <label text="saturation" width="90" align="right"/>
      <slider name="Saturation" min-value="-2.0" max-value="4.0" action="adjust-saturation-action" flex="1"/>
      <label text="0.0" name="saturation value label" width="80"/>
    </row>
  </column>
</application-window>
