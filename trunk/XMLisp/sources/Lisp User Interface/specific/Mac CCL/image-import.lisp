;;;-*- Mode: Lisp; Package: ad3d -*-
;*********************************************************************
;*                                                                   *
;*                    I M A G E   I M P O R T                        *
;*                                                                   *
;*********************************************************************
   ;* Author    : Alexander Repenning (alexander@agentsheets.com)    *
   ;*             http://www.agentsheets.com                         *
   ;* Copyright : (c) 1996-2009, AgentSheets Inc.                    *
   ;* Filename  : Texture-and-Image-import.lisp                      *
   ;* Updated   : 08/08/06                                           *
   ;* Version   :                                                    *
   ;*   1.0     : 11/29/02 from MacOpenGL 0.8.2                      *
   ;*   1.1     : 02/17/04 use pathname for create-image-from-file   *
   ;*   1.1.1   : 03/05/04 &key Verbose for create-image-from-file   *
   ;*   1.2     : 04/10/04 set-image-rgba                            *
   ;*   1.3     : 10/25/04 mipmaps support                           *
   ;*   1.4     : 12/30/04 support 8 and 16 bit images and textures  *  
   ;*   1.4.1   : 01/27/05 better file missing error handling        *
   ;*   1.5     : 05/10/05 8 bit GIF with alpha support              *
   ;*   1.5.1   : 08/09/05 keyword wrap to create-texture-from-file  *
   ;*   1.6     : 09/26/05 forced-depth                              *
   ;*   1.6.1   : 12/07/05 gl_clamp -> gl_clamp_to_edge              *
   ;*   1.6.2   : 06/26/06 Mag-Filter parameter                      *
   ;*   1.6.3   : 08/08/06 export fill-buffer                        *
   ;*   2.0     : 02/04/08 NSimage based                             *               
   ;* SW/HW     : PowerPC G4, CCL 1.3                                *
   ;* Abstract  : Texture and Image import functions                 *
   ;* Todo: - 
   ;*                                                                *
   ;*****************************************************************

(in-package :lui)

(export '(create-image-from-file 
          fill-buffer
          rgba-image-red rgba-image-green rgba-image-blue rgba-image-alpha
          set-image-rgba))


(defun CREATE-IMAGE-FROM-FILE (Filename &key Verbose Forced-Depth (Flip-Vertical t)) "
  in:  Filename string-or-pathname, &key Verbose boolean, Forced-Depth int, 
  out: Pixels byte-vector,
       Width Height Forced-Depth int; Has-Alpha boolean.
  Create an image buffer from <Filename>
  - File must be 32 bit ARGB compatible, e.g., .png with mask or 24 bit RGB."
  (when Verbose (format t "CREATE-IMAGE-FROM-FILE: ~A~%" Filename))
  (let* ((Image-Representation (#/imageRepWithContentsOfFile: ns:NS-Image-Rep (native-string Filename))))
    ;; should massage data: GL_BGRA, GL_UNSIGNED_INT_8_8_8_8_REV for best performance
    ;; http://developer.apple.com/documentation/graphicsimaging/Conceptual/OpenGL-MacProgGuide/opengl_texturedata/opengl_texturedata.html
    (cond
     ((%null-ptr-p Image-Representation)
      (format t "~%missing texture ~S" Filename)
      nil)
     (t 
      ;; should copy the data, vertical swap, and release
      (values 
       (#/bitmapData Image-Representation)
       (#/pixelsWide Image-Representation)
       (#/pixelsHigh Image-Representation)
       (#/bitsPerPixel Image-Representation)
       (#/hasAlpha Image-Representation)
       (#/bitmapFormat Image-Representation))))))


#| Examples:

(create-image-from-file "/ASI-Products/Common/resources/textures/skyIsland512x256.png")


|#
