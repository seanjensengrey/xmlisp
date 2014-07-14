(in-package :xlui)


(defun CONVERT-IMAGE-FILE (Source Destination
                           &key (Width 32) (Height 32) (Depth 32) (image-format :png)) "
  in:  Source Destination Pathname; 
       &key Width fixnum default 32; Height fixnum default 32; Depth fixnum default 32; 
       Image-Format can be :png :bmp :jpg :tiff
  Create a new image file of different size and image-format."
  ;; I still need to find a way to set the image depth.
  (declare (ignore Depth))
  (unless (probe-file source)
    (error "Cannot covnert image file because ~A does not exist." source)
    (return-from convert-image-file))
  (ns:with-ns-size (Size Width Height)
    (let* ((Image-Pathname (lui::native-string (format nil "~A" (truename (ccl::native-untranslated-namestring Source)))))
           (source-image #-cocotron (#/autorelease (#/initByReferencingFile: (#/alloc ns:ns-image) Image-Pathname))
                         #+cocotron (#/initWithContentsOfFile: (#/alloc ns:ns-image) Image-Pathname))
           (resized-image (#/initWithSize: (#/alloc ns:ns-image) size))
           (original-size (#/size source-image)))
      (cond ((and (= width (NS:NS-SIZE-WIDTH original-size)) (= height (NS:NS-SIZE-HEIGHT original-size)))
             (setq resized-image source-image))
            (t ;; resize
              (ns:with-ns-rect (Frame 0 0 width height)
               (ns:with-ns-rect (orig-rect 0 0 (NS:NS-SIZE-WIDTH Original-Size) (NS:NS-SIZE-HEIGHT Original-Size))
                 (#/lockFocus resized-image)
                 (#/drawInRect:fromRect:operation:fraction: Source-Image Frame orig-rect #$NSCompositeCopy 1.0)
                 (#/unlockFocus resized-image)))
              #+cocotron (standard-alert-dialog 
                          "Warning: resizing images (e.g. agent depictions or backgrounds) does not currently work on Windows."
                          :explanation-text "Your project will be converted, but it will contain empty image files for your agent shapes or your background.")))
      (let ((image-rep (#/initWithData: (#/alloc ns:ns-bitmap-image-rep) (#/TIFFRepresentation resized-image))))
        ;(#/setBitsPerSample: image-rep depth)
        ;(#/setAlpha: image-rep #$YES)
        (#/writeToFile:atomically: 
         (#/representationUsingType:properties: 
          image-rep  
          (case image-format
            (:png #$NSPNGFileType)
            (:jpg #$NSJPEGFileType)
            (:bmp #$NSBMPFileType)
            (:tiff #$NSTIFFFileType))
          ccl::+null-ptr+)
         (lui::native-string (ccl::native-untranslated-namestring Destination))
         #$YES)))))


(defun IMAGE-FILE-INFORMATION (Pathname) "
  in:  Pathname pathname.
  out: Width Height fixnum.
  Return basic image information."
  (unless (probe-file Pathname)
    (error "File ~A does not exist." Pathname)
    (return-from image-file-information))
  (let* ((Image (#/imageRepWithContentsOfFile: ns:ns-image-rep (lui::native-string (namestring Pathname)))))
    (values 
     (#/pixelsHigh image)
     (#/pixelsWide image))))


(defun VALIDATE-OPENGL-COMPLIANCE (Pathname) "
  in: Pathname pathname.
  out: t if the pathname will comply with openGL requirements I.E. power of 2
       nil with a dialog explaining the situation if it will not"
  (when (> (count #\. (if (stringp Pathname) (namestring (pathname-name (truename Pathname))) (namestring (pathname-name Pathname)))) 1)
    (standard-alert-dialog "You have selected an invalid file."  :explanation-text "AgentCubes does not support files that have multiple \".\"s, please try again.")
    (return-from validate-opengl-compliance))
  (multiple-value-bind (width height) (image-file-information Pathname)
    (if (and (power-of-2-p width) (power-of-2-p height))
      (return-from validate-opengl-compliance t)
      (standard-alert-dialog "I am sorry but the height and width of the image file for this operation must be a power of 2."  :explanation-text (format nil "Please select another image file or convert the height to ~A and width to ~A." (expt 2 (truncate (/ (log height) (log 2)))) (Expt 2 (truncate (/ (log width) (log 2))) ))))
    nil))


(defun POWER-OF-2-P (number)
  (when (equal (- (truncate (/ (log number) (log 2)))  (/ (log number) (log 2))) 0.0)
    t))
   
(defun PRINT-IMAGE-BUFFER (Image height width &key (has-alpha nil))
  (dotimes (i (* height width))
    (print (get-byte Image i))))

(defun GET-RGBA-VALUE-AT-ROW-COL (pixel-buffer row col &key (img-height 32) (img-width 32)  (img-depth 32))
  (let ((Byte-offset (* (+ Col (* (- img-height Row 1) img-width))  (truncate img-depth 8))))
    (values
     (get-byte pixel-buffer  Byte-Offset)
     (get-byte pixel-buffer (+ Byte-Offset 1))
     (get-byte  pixel-buffer (+ Byte-Offset 2))
     (get-byte pixel-buffer (+ Byte-Offset 3)))))
  
#| Example:

(convert-image-file "/Users/Mike/working copies/XMLisp svn/trunk/XMLisp/resources/images/logo_active.gif" "/Users/Mike/working copies/XMLisp svn/trunk/XMLisp/resources/images/logo_active.png" :width 64 :height 64)

|#

