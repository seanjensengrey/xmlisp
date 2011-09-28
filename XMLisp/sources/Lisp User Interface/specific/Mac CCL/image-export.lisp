;;;-*- Mode: Lisp; Package: lui -*-
;*********************************************************************
;*                                                                   *
;*                    I M A G E   E X P O R T                        *
;*                                                                   *
;*********************************************************************
   ;* Author    : Alexander Repenning (alexander@agentsheets.com)    *
   ;*             http://www.agentsheets.com                         *
   ;* Copyright : (c) 1996-2010, AgentSheets Inc.                    *
   ;* Filename  : image-export.lisp                                  *
   ;* Updated   : 02/17/10                                           *
   ;* Version   :                                                    *
   ;*   1.0     : 02/16/10 based on MCL image-utile.lisp             *
   ;* SW/HW     : Intel-Mac, CCL 1.4                                 *
   ;* Abstract  : Texture and Image export functions                 *
   ;* Todo: -                                                        *
   ;*                                                                *
   ;*****************************************************************

(in-package :lui)

(export '(SAVE-TEXTURE-AS-IMAGE write-rgba-image-to-file))


(defun RGBA-TO-ARGB-BUFFER (Pixel-Buffer)
  "Rearanges pixel buffer from RGBA to ARGB order."
  ; the slow way
  (dotimes (I (truncate (sizeof Pixel-Buffer) 4))
    (let* ((Addr (* i 4))
           (Alpha (%get-byte Pixel-Buffer (+ Addr 3))))
      (setf (%get-byte Pixel-Buffer (+ Addr 3)) (%get-byte Pixel-Buffer (+ Addr 2)))
      (setf (%get-byte Pixel-Buffer (+ Addr 2)) (%get-byte Pixel-Buffer (+ Addr 1)))
      (setf (%get-byte Pixel-Buffer (+ Addr 1)) (%get-byte Pixel-Buffer Addr))
      (setf (%get-byte Pixel-Buffer Addr) Alpha))))


(defun WRITE-RGBA-IMAGE-TO-FILE (Image Height Width Pathname &key (Depth 4) (Image-Type :png)) "
  in: Image pointer to RBGA buffer; Height, Width int; Pathname pathname; &optional Depth int default 4.
  Depth in bytes.
  Write RGBA image buffer into an image file."
  (rlet ((Planes :address))  ;; initWithBitmapDataPlanes expects an array of pointers to buffers
    (setf (%get-ptr Planes) Image)
    (let ((Bitmap (#/alloc ns:ns-bitmap-image-rep)))
      ;; welcome to Cocoa's longest function
      (let ((Initialized-Bitmap
             (#/initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bitmapFormat:bytesPerRow:bitsPerPixel:
              Bitmap
              Planes
              Width
              Height
              (* 2 Depth)
              4 ;; channels (RGBA)
              #$YES
              #$NO
              #$NSCalibratedRGBColorSpace
              #$NSAlphaNonpremultipliedBitmapFormat
              (* Width Depth)
              (* 8 Depth))))
        (unless (%ptr-eql Bitmap Initialized-Bitmap) (error "Trapped the rare white Cocoa elephant: initWithBitmapDataPlanes... returns a different pointer!!"))
        (#/representationUsingType:properties: 
          Initialized-Bitmap 
          (case Image-Type
            (:png #$NSPNGFileType)
            (:tiff #$NSTIFFFileType)
            (:jpeg #$NSJPEGFileType)
            (:jpeg2000 #$NSJPEG2000FileType))
          nil)
        (let ((Return-Value
               (#/writeToFile:atomically:
                (#/representationUsingType:properties: 
                 Initialized-Bitmap 
                 (case Image-Type
                   (:png #$NSPNGFileType)
                   (:tiff #$NSTIFFFileType)
                   (:jpeg #$NSJPEGFileType)
                   (:jpeg2000 #$NSJPEG2000FileType))
                 nil)
                (let ((Path (probe-file Pathname)))
                  (if Path
                    (native-string (namestring (translate-logical-pathname Path)))
                    (native-string Pathname)))
                #$YES)))
          (when (equal Return-Value #$NO) (error "File ~A could not be written" Pathname)))
        (#/release Bitmap)))))


(defmethod SAVE-FRAME-BUFFER-AS-IMAGE ((Self opengl-view) To-Pathname)
  (let ((Depth 4))
    (with-glcontext Self
      (with-vector-of-size (&buffer (* (height Self) (width Self) Depth))
        (glReadPixels 0 0 (width Self) (height Self) GL_RGBA GL_UNSIGNED_BYTE &buffer)
        (flip-vertical-buffer &buffer (* (width Self) (height Self) Depth) (* (width Self) Depth))
        (write-rgba-image-to-file &buffer (height Self) (width Self) To-Pathname :depth Depth)))))
      


(defun SAVE-TEXTURE-AS-IMAGE (Texture To-Pathname Width Height &key (Depth 4)) "
  in: Texture id; to-pathname pathname; width, height int; &key Depth int default 4.
  Depth in bytes.
  Save texture into pathname.
  This needs to be run within a with-glcontext"
  (with-vector-of-size (&image (* Width Height Depth))
    (glBindTexture GL_TEXTURE_2D Texture)  ; make the texture current
    (glGetTexImage GL_TEXTURE_2D 0 (ecase Depth
                                     (3 GL_RGB)
                                     (4 GL_RGBA))
                   GL_UNSIGNED_BYTE &image)
    (flip-vertical-buffer &image (* Width Height Depth) (* Width Depth))  ;unflip buffer
    (write-rgba-image-to-file &image Height Width To-Pathname :depth Depth)))



#| Examples:



(with-vector-of-size (&Image (* 32 32 4))
  (dotimes (x 32)
    (dotimes (y 32)
      (let ((Offset (* (+ (* y 32) x) 4)))
        (set-byte &Image (* y 8) Offset)
        (set-byte &Image 0 (+ Offset 1))
        (set-byte &Image 0 (+ Offset 2))
        (set-byte &Image (* x 8) (+ Offset 3)))))
  (write-rgba-image-to-file &Image 32 32 "/Users/alex/Desktop/image99.png"))


;; same by jpeg 2000
(with-vector-of-size (&Image (* 32 32 4))
  (dotimes (x 32)
    (dotimes (y 32)
      (let ((Offset (* (+ (* y 32) x) 4)))
        (set-byte &Image (* y 8) Offset)
        (set-byte &Image 0 (+ Offset 1))
        (set-byte &Image 0 (+ Offset 2))
        (set-byte &Image (* x 8) (+ Offset 3)))))
  (write-rgba-image-to-file &Image 32 32 "/Users/alex/Desktop/image99.jp2" :image-type :jpeg2000))



;;; if you have agentcubes running

(save-frame-buffer-as-image 
  (xlui::view-named (xlui::project-window xlui::*Project-Manager*) "the world")
  "/Users/alex/Desktop/image102.png")



(width (xlui::view-named (xlui::project-window xlui::*Project-Manager*) "the world"))
(height (xlui::view-named (xlui::project-window xlui::*Project-Manager*) "the world"))


|#


