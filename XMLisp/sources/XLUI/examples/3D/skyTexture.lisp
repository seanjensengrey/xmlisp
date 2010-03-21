;;; OpenGL for MCL example: Sky Box
;;; non-continuing texture used to show seam 
;;; Alexander Repenning 11/6/02
;;; 10/06/05 :use-global-gl-context
;;; 10/18/06 gltexenvf -> gltexenvi
;;; 04/17/09 Clozure CL

(in-package :xlui)


(defclass SKY-TEXTURE-RECT (opengl-dialog)
  ())


(defmethod PREPARE-OPENGL ((Self sky-texture-rect))
  ;; GL setup
  (glClearColor 0.0 0.0 0.0 0.0)
  (glShadeModel gl_smooth)
  (glPixelStorei GL_UNPACK_ALIGNMENT 1)
  ;; define material
  (glMaterialfv GL_FRONT_AND_BACK GL_SPECULAR { 0.5 0.5 0.5 0.0 })
  (glMaterialf GL_FRONT_AND_BACK GL_SHININESS 128.0)
  (glMaterialfv GL_FRONT_AND_BACK GL_AMBIENT_AND_DIFFUSE { 1.0 1.0 1.0 1.0 })
  ;; light
  (glLightfv GL_LIGHT0 GL_POSITION { 3.0 3.0 3.0 1.0 })
  (glLightfv gl_light0 GL_DIFFUSE { 1.0 1.0 1.0 1.0 })
  (glLightfv gl_light0 GL_SPECULAR { 1.0 1.0 1.0 1.0 })
  ;; enablers
  (glDisable GL_LIGHTING)
  (glEnable GL_LIGHT0)
  (glEnable GL_DEPTH_TEST)
  ;; camera
  (aim-camera (camera Self) :eye-z 10.0)) 


(defmethod DRAW ((Self sky-texture-rect))
  (glEnable GL_TEXTURE_2D)
  ;; Sky Rect 
  (glDisable GL_LIGHTING)
  (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
  (use-texture Self "skyIsland.png")
  (glBegin GL_QUADS)
  ;; front 
  (glNormal3f 0.0 0.0 1.0)
  (glTexCoord2f 0.0 0.0) (glvertex3f -10.0 -10.0 10.0)
  (glTexCoord2f 0.0 1.0) (glvertex3f -10.0  10.0 10.0)
  (glTexCoord2f 1.0 1.0) (glvertex3f  10.0  10.0 10.0)
  (glTexCoord2f 1.0 0.0) (glvertex3f  10.0 -10.0 10.0)
  ;; back
  (glNormal3f 0.0 0.0 -1.0)
  (glTexCoord2f 0.0 0.0) (glVertex3f -10.0 -10.0 -10.0)
  (glTexCoord2f 0.0 1.0) (glVertex3f -10.0  10.0 -10.0)
  (gltexcoord2f 1.0 1.0) (glVertex3f  10.0  10.0 -10.0)
  (gltexcoord2f 1.0 0.0) (glVertex3f  10.0 -10.0 -10.0)
  ;; right
  (glNormal3f  1.0 0.0 0.0)
  (glTexCoord2f 0.0 0.0) (glVertex3f 10.0 -10.0 -10.0)
  (glTexCoord2f 0.0 1.0) (glVertex3f 10.0 -10.0  10.0)
  (glTexCoord2f 1.0 1.0) (glVertex3f 10.0  10.0  10.0)
  (glTexCoord2f 1.0 0.0) (glVertex3f 10.0  10.0 -10.0)
  ;; left
  (glNormal3f -1.0 0.0 0.0)
  (glTexCoord2f 0.0 0.0) (glVertex3f -10.0 -10.0 -10.0)
  (glTexCoord2f 0.0 1.0) (glVertex3f -10.0 -10.0  10.0)
  (glTexCoord2f 1.0 1.0) (glVertex3f -10.0  10.0  10.0)
  (glTexCoord2f 1.0 0.0) (glVertex3f -10.0  10.0 -10.0)
  ;; top
  (glNormal3f 0.0 1.0 0.0)
  (glTexCoord2f 0.0 0.0) (glVertex3f -10.0  10.0 -10.0)
  (gltexcoord2f 0.0 1.0) (glVertex3f -10.0  10.0 10.0)
  (glTexCoord2f 1.0 1.0) (glVertex3f  10.0  10.0 10.0)
  (glTexCoord2f 1.0 0.0) (glVertex3f  10.0  10.0 -10.0)
  ;; bottom
  (glNormal3f 0.0 -1.0 0.0)
  (glTexCoord2f 0.0 0.0) (glVertex3f -10.0 -10.0 -10.0)
  (glTexCoord2f 0.0 1.0) (glVertex3f -10.0 -10.0 10.0)
  (glTexCoord2f 1.0 1.0) (glVertex3f  10.0 -10.0 10.0)
  (glTexCoord2f 1.0 0.0) (glVertex3f  10.0 -10.0 -10.0)
  (glEnd)

  (glDisable GL_TEXTURE_2D))
  

<application-window>
  <sky-texture-rect/>
</application-window>

