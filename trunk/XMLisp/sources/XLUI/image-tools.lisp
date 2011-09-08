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
    (let* ((source-image (#/initWithContentsOfFile: (#/alloc ns:ns-image)(lui::native-string (format nil "~A" (truename (ccl::native-untranslated-namestring Source))))))
           (resized-image (#/initWithSize: (#/alloc ns:ns-image) size))
           (original-size (#/size source-image)))
      (unless (and (equal width (NS:NS-SIZE-WIDTH original-size)) (equal height (NS:NS-SIZE-HEIGHT original-size)))
        (#/lockFocus resized-image)
        (#/drawInRect:fromRect:operation:fraction: 
         source-image 
         (ns::make-ns-rect 0 0 width height)
         (ns::make-ns-rect 0 0 (NS:NS-SIZE-WIDTH original-size) (NS:NS-SIZE-HEIGHT original-size))
         #$NSCompositeSourceOver
         1.0)
        (#/unlockFocus resized-image))
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
  (multiple-value-bind (width height) (image-file-information Pathname)
    (if (and (power-of-2-p width) (power-of-2-p height))
      (return-from validate-opengl-compliance t)
      (standard-alert-dialog "I am sorry but the height and width of the image file for this operation must be a power of 2."  :explanation-text (format nil "Please select another image file or convert the height to ~A and width to ~A." (expt 2 (truncate (/ (log height) (log 2)))) (Expt 2 (truncate (/ (log width) (log 2))) ))))
    nil))


(defun POWER-OF-2-P (number)
  (when (equal (- (truncate (/ (log number) (log 2)))  (/ (log number) (log 2))) 0.0)
    t))
    

#| Example:

(convert-image-file "/Users/Mike/working copies/XMLisp svn/trunk/XMLisp/resources/images/logo_active.gif" "/Users/Mike/working copies/XMLisp svn/trunk/XMLisp/resources/images/logo_active.png" :width 64 :height 64)

|#

