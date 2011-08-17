;;;-*- Mode: Lisp; Package: lui -*-
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
   ;*   2.0     : 02/04/09 NSimage based                             *               
   ;*   2.0.1   : 06/28/09 flip-vertical                             *
   ;* SW/HW     : PowerPC G4, CCL 1.3                                *
   ;* Abstract  : Texture and Image import functions                 *
   ;* Todo: -                                                        *
   ;*                                                                *
   ;*****************************************************************

(in-package :lui)

(export '(create-image-from-file 
          fill-buffer
          rgba-image-red rgba-image-green rgba-image-blue rgba-image-alpha
          set-image-rgba ZERO-ALPHA-32BIT-COLOR-P))


(defun FLIP-VERTICAL-BUFFER (Buffer Number-of-Bytes Bytes-per-Row) "
  in/out: Buffer {bytes}.
  in: Number-of-Bytes Bytes-per-Row int
  Flip content of buffer vertically"
  ;; a shame that this is necessary but this is how OpenGL wants its buffer
  ;; should be fairly fast: rows * 3 blockmoves
  (unless (zerop (mod Number-of-Bytes Bytes-per-Row))
    (error "cannot flip buffer: rowsize inconsistent with buffersize"))
  (let ((Row-Buffer #-windows-target (#_NewPtr Bytes-per-Row)
		    #+windows-target (#_HeapAlloc (#_GetProcessHeap) 0 Bytes-per-Row))
        (Rows (truncate Number-of-Bytes Bytes-per-Row)))
    ;; outside-in swap top with bottom until reaching middle
    (dotimes (Row (truncate Rows 2))
      ;; 1) row from above into row buffer
      (#_memmove
       Row-Buffer
       (%inc-ptr Buffer (* Row Bytes-per-Row))
       Bytes-per-Row)
      ;; 2) row from below into row above 
      (#_memmove
       (%inc-ptr Buffer (* Row Bytes-per-Row))
       (%inc-ptr Buffer (* (- Rows Row 1) Bytes-per-Row))
       Bytes-per-Row)
      ;; 2) row buffer into row below 
      (#_memmove
       (%inc-ptr Buffer (* (- Rows Row 1) Bytes-per-Row))
       Row-Buffer
       Bytes-per-Row))
    #-windows-target (#_DisposePtr Row-Buffer)
    #+windows-target (#_HeapFree (#_GetProcessHeap) 0 Row-Buffer)))


(defun NS-IMAGE-REP-FROM-FILE (native-filename)
  ;; Cocotron doesn't implement #/imageRepWithContentsOfFile: (Issue 376)
  #+cocotron
  (let* ((images (#/imageRepsWithContentsOfFile: ns:NS-Image-Rep native-filename)))
    (cond
     ((%null-ptr-p images) images)
     (t 
      (if (> (#/count images) 0)
        (#/objectAtIndex: images 0)
        +null-ptr+))))
  #-cocotron (#/imageRepWithContentsOfFile: ns:NS-Image-Rep native-filename))


(defun CREATE-32BIT-RGBA-IMAGE-FROM-24BIT-RGB-IMAGE (Image Width Height) 
  ;; may have to consider deallocating original image
  (let ((New-Image (make-vector-of-size (* Width Height 4))))
    (dotimes (i (* Width Height))
      (let ((Source-Offset (* i 3))
            (Destination-Offset (* i 4)))
        (set-byte New-Image (get-byte Image Source-Offset) Destination-Offset)
        (set-byte New-Image (get-byte Image (+ Source-Offset 1)) (+ Destination-Offset 1))
        (set-byte New-Image (get-byte Image (+ Source-Offset 2)) (+ Destination-Offset 2))
        (set-byte New-Image #xFF (+ Destination-Offset 3))))
    New-Image))


(defun CREATE-IMAGE-FROM-FILE (Filename &key Verbose Forced-Depth (Flip-Vertical t)) "
  in:  Filename string-or-pathname, &key Verbose boolean, Forced-Depth int, 
  out: Pixels byte-vector,
       Width Height Forced-Depth int; Has-Alpha boolean.
  Create an image buffer from <Filename>
  - File must be 32 bit ARGB compatible, e.g., .png with mask or 24 bit RGB."
  (when Verbose (format t "CREATE-IMAGE-FROM-FILE: ~A~%" Filename))
  ;; access image data from file, copy into vector and return
  (ccl::with-autorelease-pool
      (let* ((Image-Representation (ns-image-rep-from-file (native-string (namestring (translate-logical-pathname Filename))))))
        ;; should massage data: GL_BGRA, GL_UNSIGNED_INT_8_8_8_8_REV for best performance
        ;; http://developer.apple.com/documentation/graphicsimaging/Conceptual/OpenGL-MacProgGuide/opengl_texturedata/opengl_texturedata.html
        (when (%null-ptr-p Image-Representation)
          (error "~%missing texture ~S" Filename)
          (return-from create-image-from-file))
        ;; do the OpenGL vertical image flip
        (when Flip-Vertical
          (flip-vertical-buffer 
           (#/bitmapData Image-Representation) 
           (* (#/bytesPerRow Image-Representation) (#/pixelsHigh Image-Representation))
           (#/bytesPerRow Image-Representation)))
        ;(format t "bitmap Data = ~A pixelsWide = ~A pixelsHigh = ~A bitsPerPixel = ~A" (#/bitmapData Image-Representation) (#/pixelsWide Image-Representation) (#/pixelsHigh Image-Representation) (#/bitsPerPixel Image-Representation))
        (if (and Forced-Depth
                 (= Forced-Depth 32)
                 (= (#/bitsPerPixel Image-Representation) 24))
          (values 
           (create-32bit-rgba-image-from-24bit-rgb-image 
            (#/bitmapData Image-Representation)
            (#/pixelsWide Image-Representation)
            (#/pixelsHigh Image-Representation))
           (#/pixelsWide Image-Representation)
           (#/pixelsHigh Image-Representation)
           Forced-Depth
           t
           (#/bitmapFormat Image-Representation))
          (values 
           (copy-vector (#/bitmapData Image-Representation) 
                        (* (#/pixelsWide Image-Representation)
                           (#/pixelsHigh Image-Representation)
                           (ecase (#/bitsPerPixel Image-Representation)
                             (32 4)
                             (24 3))))
           (#/pixelsWide Image-Representation)
           (#/pixelsHigh Image-Representation)
           (#/bitsPerPixel Image-Representation)
           (#/hasAlpha Image-Representation)
           (#/bitmapFormat Image-Representation))))))


(defun RGBA-IMAGE-RED (Image X Y Width) "
  in:  Image {RGBAimage}, x, y, Width {fixnum}.
  out: Byte {byte}."
  (%get-byte Image (* (+ (* y Width) x) 4)))


(defun RGBA-IMAGE-GREEN (Image X Y Width) "
  in:  Image {RGBAimage}, x, y, Width {fixnum}.
  out: Byte {byte}."
  (%get-byte Image (+ (* (+ (* y Width) x) 4) 1)))


(defun RGBA-IMAGE-BLUE (Image X Y Width) "
  in:  Image {RGBAimage}, x, y, Width {fixnum}.
  out: Byte {byte}."
  (%get-byte Image (+ (* (+ (* y Width) x) 4) 2)))


(defun RGBA-IMAGE-ALPHA (Image X Y Width) "
  in:  Image {RGBAimage}, x, y, Width {fixnum}.
  out: Byte {byte}."
  (%get-byte Image (+ (* (+ (* y Width) x) 4) 3)))


(defun SET-IMAGE-RGBA (Image X Y Width R G B A) "
 in: Image {RGGAimage}, x, y, Width {fixnum}, R, G, B, A {byte}"
 (let ((Index (* (+ (* y Width) x) 4)))
   (setf (%get-byte Image Index) R)
   (setf (%get-byte Image (+ Index 1)) G)
   (setf (%get-byte Image (+ Index 2)) B)
   (setf (%get-byte Image (+ Index 3)) A)))


(defun ZERO-ALPHA-32BIT-COLOR-P (Color) "
  in:  Color pointer to 32 RGBA color value.
  out: is-zero boolean.
  If the alpha of the color is zero the color is invisible."
  (zerop (%get-byte Color 3)))



#| Examples:

(time (create-image-from-file "/Users/alex/working copies/XMLisp svn/trunk/XMLisp/resources/textures/palm-araceae02.png"))

(time (create-image-from-file "/Users/alex/working copies/XMLisp svn/trunk/XMLisp/resources/textures/palm-araceae02.png" :flip-vertical nil))


;; watch "Real Memory" in Activity Monitor: should not change much

(dotimes (i 100)
  (print i)
  (dispose-vector (create-image-from-file "lui:resources;textures;palm-araceae02.png")))

|#
