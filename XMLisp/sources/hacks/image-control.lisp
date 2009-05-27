
(in-package :lui)


;; http://developer.apple.com/samplecode/GLSLShowpiece/listing14.html
;; http://www.idevgames.com/forum/archive/index.php/t-704.html


(defparameter *File* "/Users/alex/Documents/grants/ITEST/artwork/sgd\ large.png")

(#/length (#/TIFFRepresentation (#/initByReferencingFile: (#/alloc ns:ns-image) *File*)))

(#/bitmapData (#/initWithData: 
 (#/alloc ns:NS-Bitmap-Image-Rep)
 (#/TIFFRepresentation (#/initByReferencingFile: (#/alloc ns:ns-image) *File*))))